#!/usr/bin/env python3

"""
json2haskell.py

Infer path-sensitive Haskell data definitions and Aeson FromJSON
instances from a JSON document.

Main rules
==========

1. Arrays aggregate all their elements into ONE element schema.

2. Schemas are path-sensitive. Unrelated fields having the same name
   never share generated Haskell types merely because they have the
   same name or shape.

3. Objects containing the configured discriminator field
   ("content_type" by default) become tagged Haskell sum types.

4. "metadata" is opaque by default:

       "metadata": { ... arbitrary JSON object ... }

   becomes:

       Mp.Map Text Value

   Its internal structure is deliberately not inferred.

5. "code_blocks" is treated as an indexed-object encoding of a vector:

       "code_blocks": {
         "0": { ... },
         "1": { ... },
         "2": { ... }
       }

   becomes:

       V.Vector CodeBlock...

   All values are aggregated into one element schema. Numeric JSON
   object keys are sorted numerically during parsing.

6. Missing and explicit-null fields are distinguished:

       missing key
           Maybe a
           .:?

       always present, but sometimes null
           Maybe a
           .:

7. Genuine non-tagged representation differences become ordinary
   Haskell sum types parsed with <|>.


Examples
========

    python json2haskell.py input.json

    python json2haskell.py input.json \
        --root ImportData \
        --module Import.Schema \
        --show-schema \
        -o Import/Schema.hs


Additional opaque Map fields can be declared:

    python json2haskell.py input.json \
        --opaque-map-field properties \
        --opaque-map-field attributes


Additional indexed-vector fields can be declared:

    python json2haskell.py input.json \
        --indexed-vector-field blocks
"""

from __future__ import annotations

import argparse
import json
import re
import sys

from dataclasses import dataclass, field, replace
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple


# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------

JsonPath = Tuple[str, ...]


@dataclass
class InferenceConfig:
    discriminator: Optional[str]

    opaque_map_fields: Set[str] = field(
        default_factory=lambda: {
            "metadata",
        }
    )

    indexed_vector_fields: Set[str] = field(
        default_factory=lambda: {
            "code_blocks",
        }
    )


@dataclass
class FieldSchema:
    """
    Schema for one object property.

    present records how many instances of the containing object
    actually contained this key.
    """

    schema: "Schema"
    present: int = 0


@dataclass
class Schema:
    """
    Aggregate schema.

    kind:

        unknown
        bool
        integer
        scientific
        text
        object
        array
        opaque_map
        indexed_vector
        union
        tagged_union
    """

    kind: str
    path: JsonPath

    # Explicit JSON null was observed.
    nullable: bool = False

    # Object.
    fields: Dict[str, FieldSchema] = field(
        default_factory=dict
    )

    observations: int = 0

    # Array / indexed_vector.
    item: Optional["Schema"] = None

    # Ordinary structural union.
    variants: List["Schema"] = field(
        default_factory=list
    )

    # Tagged union.
    discriminator: Optional[str] = None

    tagged_variants: Dict[str, "Schema"] = field(
        default_factory=dict
    )


# ---------------------------------------------------------------------------
# Schema helpers
# ---------------------------------------------------------------------------

def nonnull(
    schema: Schema,
) -> Schema:
    return replace(
        schema,
        nullable=False,
    )


def with_nullable(
    schema: Schema,
    flag: bool,
) -> Schema:
    return replace(
        schema,
        nullable=(
            schema.nullable
            or flag
        ),
    )


# ---------------------------------------------------------------------------
# Special field recognition
# ---------------------------------------------------------------------------

def is_index_key(
    key: str,
) -> bool:
    """
    Is this a legal poor-man vector index?

        "0"
        "1"
        "27"

    Negative and non-numeric keys are rejected.
    """

    return bool(
        re.fullmatch(
            r"[0-9]+",
            key,
        )
    )


def is_indexed_object(
    value,
) -> bool:
    """
    Detect the representation:

        {
          "0": value,
          "1": value,
          ...
        }

    The empty object is accepted because it is a valid empty encoding
    of the same container.
    """

    if not isinstance(
        value,
        dict,
    ):
        return False

    return all(
        is_index_key(key)
        for key
        in value
    )


def infer_opaque_map(
    value,
    path: JsonPath,
) -> Optional[Schema]:
    """
    Recognise an opaque map.

    Returning None means that the observed value is incompatible with
    the special representation and should be inferred normally.
    """

    if value is None:
        return Schema(
            "opaque_map",
            path,
            nullable=True,
        )

    if isinstance(
        value,
        dict,
    ):
        # Deliberately DO NOT visit any children.
        return Schema(
            "opaque_map",
            path,
        )

    return None


def infer_indexed_vector(
    value,
    path: JsonPath,
    config: InferenceConfig,
) -> Optional[Schema]:
    """
    Recognise an object being used as an integer-indexed array.

    Example:

        {
          "0": {"id": "a"},
          "1": {"id": "b"},
          "2": {"id": "c"}
        }

    All three child objects are inferred at:

        path + ("[]",)

    and consequently aggregate into ONE schema.
    """

    if value is None:
        return Schema(
            "indexed_vector",
            path,
            nullable=True,
            item=None,
        )

    if not is_indexed_object(
        value
    ):
        return None

    item_path = (
        path
        + ("[]",)
    )

    item_schema: Optional[
        Schema
    ] = None

    # Numeric order makes first-observation field ordering deterministic
    # and agrees with the eventual Haskell Vector order.
    keys = sorted(
        value,
        key=lambda key: int(key),
    )

    for key in keys:
        child = value[key]

        child_schema = infer_schema(
            child,
            item_path,
            config,
        )

        if item_schema is None:
            item_schema = child_schema

        else:
            item_schema = merge_schema(
                item_schema,
                child_schema,
            )

    return Schema(
        "indexed_vector",
        path,
        item=item_schema,
    )


def infer_field(
    key: str,
    value,
    path: JsonPath,
    config: InferenceConfig,
) -> Schema:
    """
    Infer one named JSON object field, applying field-name-specific
    representations before ordinary recursive inference.
    """

    if (
        key
        in config.opaque_map_fields
    ):
        special = infer_opaque_map(
            value,
            path,
        )

        if special is not None:
            return special

    if (
        key
        in config.indexed_vector_fields
    ):
        special = infer_indexed_vector(
            value,
            path,
            config,
        )

        if special is not None:
            return special

    return infer_schema(
        value,
        path,
        config,
    )


# ---------------------------------------------------------------------------
# General schema inference
# ---------------------------------------------------------------------------

def infer_schema(
    value,
    path: JsonPath,
    config: InferenceConfig,
) -> Schema:

    if value is None:
        return Schema(
            "unknown",
            path,
            nullable=True,
        )

    # bool is a subclass of int in Python.
    if isinstance(
        value,
        bool,
    ):
        return Schema(
            "bool",
            path,
        )

    if isinstance(
        value,
        int,
    ):
        return Schema(
            "integer",
            path,
        )

    if isinstance(
        value,
        float,
    ):
        return Schema(
            "scientific",
            path,
        )

    if isinstance(
        value,
        str,
    ):
        return Schema(
            "text",
            path,
        )

    # ------------------------------------------------------------------
    # Array
    # ------------------------------------------------------------------

    if isinstance(
        value,
        list,
    ):
        item_path = (
            path
            + ("[]",)
        )

        item_schema: Optional[
            Schema
        ] = None

        for child in value:
            child_schema = infer_schema(
                child,
                item_path,
                config,
            )

            if item_schema is None:
                item_schema = child_schema

            else:
                item_schema = merge_schema(
                    item_schema,
                    child_schema,
                )

        return Schema(
            "array",
            path,
            item=item_schema,
        )

    # ------------------------------------------------------------------
    # Object
    # ------------------------------------------------------------------

    if isinstance(
        value,
        dict,
    ):
        discriminator = (
            config.discriminator
        )

        # --------------------------------------------------------------
        # Tagged union
        # --------------------------------------------------------------

        if (
            discriminator is not None
            and isinstance(
                value.get(
                    discriminator
                ),
                str,
            )
        ):
            tag = value[
                discriminator
            ]

            variant_path = (
                path
                + (
                    f"@{tag}",
                )
            )

            fields = {}

            for (
                key,
                child,
            ) in value.items():
                if (
                    key
                    == discriminator
                ):
                    continue

                child_path = (
                    variant_path
                    + (key,)
                )

                fields[key] = (
                    FieldSchema(
                        infer_field(
                            key,
                            child,
                            child_path,
                            config,
                        ),
                        present=1,
                    )
                )

            payload = Schema(
                "object",
                variant_path,
                fields=fields,
                observations=1,
            )

            return Schema(
                "tagged_union",
                path,
                observations=1,
                discriminator=(
                    discriminator
                ),
                tagged_variants={
                    tag: payload,
                },
            )

        # --------------------------------------------------------------
        # Ordinary object
        # --------------------------------------------------------------

        fields = {}

        for (
            key,
            child,
        ) in value.items():
            child_path = (
                path
                + (key,)
            )

            fields[key] = (
                FieldSchema(
                    infer_field(
                        key,
                        child,
                        child_path,
                        config,
                    ),
                    present=1,
                )
            )

        return Schema(
            "object",
            path,
            fields=fields,
            observations=1,
        )

    return Schema(
        "unknown",
        path,
    )


# ---------------------------------------------------------------------------
# Schema aggregation
# ---------------------------------------------------------------------------

def compatible(
    a: Schema,
    b: Schema,
) -> bool:

    if (
        a.kind == "unknown"
        or b.kind == "unknown"
    ):
        return True

    if {
        a.kind,
        b.kind,
    } <= {
        "integer",
        "scientific",
    }:
        return True

    if (
        a.kind == "tagged_union"
        and b.kind == "tagged_union"
    ):
        return (
            a.discriminator
            == b.discriminator
        )

    return (
        a.kind
        == b.kind
    )


def merge_objects(
    a: Schema,
    b: Schema,
) -> Schema:

    fields: Dict[
        str,
        FieldSchema,
    ] = {}

    keys = list(
        a.fields
    )

    keys.extend(
        key
        for key
        in b.fields
        if key not in a.fields
    )

    for key in keys:
        fa = a.fields.get(
            key
        )

        fb = b.fields.get(
            key
        )

        if (
            fa is not None
            and fb is not None
        ):
            fields[key] = (
                FieldSchema(
                    schema=merge_schema(
                        fa.schema,
                        fb.schema,
                    ),
                    present=(
                        fa.present
                        + fb.present
                    ),
                )
            )

        elif fa is not None:
            fields[key] = (
                FieldSchema(
                    schema=fa.schema,
                    present=fa.present,
                )
            )

        else:
            assert (
                fb is not None
            )

            fields[key] = (
                FieldSchema(
                    schema=fb.schema,
                    present=fb.present,
                )
            )

    return Schema(
        "object",
        a.path,
        fields=fields,
        observations=(
            a.observations
            + b.observations
        ),
    )


def merge_sequence_items(
    a: Schema,
    b: Schema,
    kind: str,
) -> Schema:
    """
    Merge normal arrays or indexed-object vectors.
    """

    if a.item is None:
        item = b.item

    elif b.item is None:
        item = a.item

    else:
        item = merge_schema(
            a.item,
            b.item,
        )

    return Schema(
        kind,
        a.path,
        item=item,
    )


def merge_tagged_unions(
    a: Schema,
    b: Schema,
) -> Schema:

    assert (
        a.discriminator
        == b.discriminator
    )

    variants: Dict[
        str,
        Schema,
    ] = {}

    tags = list(
        a.tagged_variants
    )

    tags.extend(
        tag
        for tag
        in b.tagged_variants
        if tag
        not in a.tagged_variants
    )

    for tag in tags:
        va = (
            a.tagged_variants.get(
                tag
            )
        )

        vb = (
            b.tagged_variants.get(
                tag
            )
        )

        if (
            va is not None
            and vb is not None
        ):
            variants[tag] = (
                merge_schema(
                    va,
                    vb,
                )
            )

        elif va is not None:
            variants[tag] = va

        else:
            assert (
                vb is not None
            )

            variants[tag] = vb

    return Schema(
        "tagged_union",
        a.path,
        observations=(
            a.observations
            + b.observations
        ),
        discriminator=(
            a.discriminator
        ),
        tagged_variants=variants,
    )


def add_union_variant(
    variants: List[Schema],
    candidate: Schema,
) -> List[Schema]:

    candidate = nonnull(
        candidate
    )

    if (
        candidate.kind
        == "unknown"
    ):
        return variants

    if (
        candidate.kind
        == "union"
    ):
        for variant in (
            candidate.variants
        ):
            variants = (
                add_union_variant(
                    variants,
                    variant,
                )
            )

        return variants

    for (
        index,
        current,
    ) in enumerate(
        variants
    ):
        if compatible(
            current,
            candidate,
        ):
            variants[index] = (
                nonnull(
                    merge_schema(
                        current,
                        candidate,
                    )
                )
            )

            return variants

    variants.append(
        candidate
    )

    return variants


def merge_schema(
    a: Schema,
    b: Schema,
) -> Schema:
    """
    Merge two observations of the SAME logical JSON path.
    """

    if (
        a.path
        != b.path
    ):
        raise ValueError(
            "attempted cross-path schema merge: "
            f"{a.path!r} vs {b.path!r}"
        )

    is_nullable = (
        a.nullable
        or b.nullable
    )

    a0 = nonnull(a)
    b0 = nonnull(b)

    if (
        a0.kind
        == "unknown"
    ):
        return with_nullable(
            b0,
            is_nullable,
        )

    if (
        b0.kind
        == "unknown"
    ):
        return with_nullable(
            a0,
            is_nullable,
        )

    # Existing ordinary unions.
    if (
        a0.kind == "union"
        or b0.kind == "union"
    ):
        variants: List[
            Schema
        ] = []

        variants = (
            add_union_variant(
                variants,
                a0,
            )
        )

        variants = (
            add_union_variant(
                variants,
                b0,
            )
        )

        if len(
            variants
        ) == 1:
            return with_nullable(
                variants[0],
                is_nullable,
            )

        return Schema(
            "union",
            a.path,
            nullable=is_nullable,
            variants=variants,
        )

    # Numeric widening.
    if {
        a0.kind,
        b0.kind,
    } <= {
        "integer",
        "scientific",
    }:
        kind = (
            "scientific"
            if "scientific"
            in {
                a0.kind,
                b0.kind,
            }
            else "integer"
        )

        return Schema(
            kind,
            a.path,
            nullable=is_nullable,
        )

    if (
        a0.kind == "object"
        and b0.kind == "object"
    ):
        return with_nullable(
            merge_objects(
                a0,
                b0,
            ),
            is_nullable,
        )

    if (
        a0.kind == "array"
        and b0.kind == "array"
    ):
        return with_nullable(
            merge_sequence_items(
                a0,
                b0,
                "array",
            ),
            is_nullable,
        )

    if (
        a0.kind
        == "indexed_vector"
        and b0.kind
        == "indexed_vector"
    ):
        return with_nullable(
            merge_sequence_items(
                a0,
                b0,
                "indexed_vector",
            ),
            is_nullable,
        )

    if (
        a0.kind
        == "tagged_union"
        and b0.kind
        == "tagged_union"
        and a0.discriminator
        == b0.discriminator
    ):
        return with_nullable(
            merge_tagged_unions(
                a0,
                b0,
            ),
            is_nullable,
        )

    # Same primitive/special opaque type.
    if (
        a0.kind
        == b0.kind
    ):
        return Schema(
            a0.kind,
            a.path,
            nullable=is_nullable,
        )

    # Genuine representation mismatch.
    variants: List[
        Schema
    ] = []

    variants = (
        add_union_variant(
            variants,
            a0,
        )
    )

    variants = (
        add_union_variant(
            variants,
            b0,
        )
    )

    return Schema(
        "union",
        a.path,
        nullable=is_nullable,
        variants=variants,
    )


# ---------------------------------------------------------------------------
# Naming
# ---------------------------------------------------------------------------

HASKELL_RESERVED = {
    "as",
    "anyclass",
    "by",
    "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "family",
    "forall",
    "foreign",
    "group",
    "hiding",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "mdo",
    "module",
    "newtype",
    "of",
    "pattern",
    "qualified",
    "role",
    "static",
    "stock",
    "then",
    "type",
    "using",
    "via",
    "where",
}


BUILTIN_TYPES = {
    "Bool",
    "Integer",
    "Maybe",
    "Scientific",
    "Text",
    "Value",
}


NAME_PART_OVERRIDES = {
    "multimodal": "MultiModal",
}


def identifier_words(
    value: str,
) -> List[str]:
    return (
        re.findall(
            r"[A-Za-z0-9]+",
            value,
        )
        or ["Field"]
    )


def upper_camel(
    value: str,
) -> str:
    result = "".join(
        NAME_PART_OVERRIDES.get(
            part.lower(),
            (
                part[:1].upper()
                + part[1:]
            ),
        )
        for part
        in identifier_words(value)
    )

    if result[0].isdigit():
        result = (
            "N"
            + result
        )

    return result


def lower_camel(
    value: str,
) -> str:
    result = upper_camel(
        value
    )

    return (
        result[:1].lower()
        + result[1:]
    )


def singular(
    value: str,
) -> str:

    lower = (
        value.lower()
    )

    if (
        lower.endswith("ies")
        and len(value) > 3
    ):
        return (
            value[:-3]
            + "y"
        )

    if lower.endswith(
        (
            "sses",
            "xes",
            "zes",
            "ches",
            "shes",
        )
    ):
        return value[:-2]

    if lower.endswith(
        (
            "ss",
            "us",
            "is",
        )
    ):
        return value

    if (
        lower.endswith("s")
        and len(value) > 1
    ):
        return value[:-1]

    return value


def semantic_segments(
    parts: Tuple[str, ...],
) -> List[str]:

    result: List[
        str
    ] = []

    index = 0

    while (
        index
        < len(parts)
    ):
        part = parts[index]

        if (
            part
            == "[]"
        ):
            index += 1
            continue

        if (
            part.startswith("@")
        ):
            result.append(
                upper_camel(
                    part[1:]
                )
            )

            index += 1
            continue

        if (
            index + 1
            < len(parts)
            and parts[index + 1]
            == "[]"
        ):
            result.append(
                upper_camel(
                    singular(
                        part
                    )
                )
            )

            index += 2
            continue

        result.append(
            upper_camel(
                part
            )
        )

        index += 1

    return result


def suggested_type_name(
    path: JsonPath,
) -> str:

    if path == ("$",):
        return "Root"

    if (
        path[-1]
        == "[]"
    ):
        local = upper_camel(
            singular(
                path[-2]
            )
        )

        ancestors = (
            path[1:-2]
        )

    elif (
        path[-1].startswith("@")
    ):
        local = upper_camel(
            path[-1][1:]
        )

        ancestors = (
            path[1:-1]
        )

    else:
        local = upper_camel(
            path[-1]
        )

        ancestors = (
            path[1:-1]
        )

    suffix = "".join(
        semantic_segments(
            ancestors
        )
    )

    return (
        local
        + suffix
    )


# ---------------------------------------------------------------------------
# Haskell generation
# ---------------------------------------------------------------------------

class HaskellGenerator:

    def __init__(
        self,
        module_name: str,
        root_name: str,
    ):
        self.module_name = (
            module_name
        )

        self.root_name = (
            upper_camel(
                root_name
            )
        )

        self.names: Dict[
            int,
            str,
        ] = {}

        self.used_type_names = set(
            BUILTIN_TYPES
        )

        self.used_type_names.add(
            self.root_name
        )

        self.used_field_names = set()

        self.emitted = set()

        self.definitions: List[
            Optional[str]
        ] = []

    # ------------------------------------------------------------------
    # Names
    # ------------------------------------------------------------------

    def fresh_name(
        self,
        suggested: str,
    ) -> str:

        base = upper_camel(
            suggested
        )

        candidate = base

        number = 2

        while (
            candidate
            in self.used_type_names
        ):
            candidate = (
                f"{base}"
                f"{number}"
            )

            number += 1

        self.used_type_names.add(
            candidate
        )

        return candidate

    def assign_name(
        self,
        schema: Schema,
        suggested: Optional[
            str
        ] = None,
    ) -> str:

        old = self.names.get(
            id(schema)
        )

        if old is not None:
            return old

        if (
            schema.path
            == ("$",)
        ):
            name = (
                self.root_name
            )

        else:
            name = (
                self.fresh_name(
                    suggested
                    or suggested_type_name(
                        schema.path
                    )
                )
            )

        self.names[
            id(schema)
        ] = name

        return name

    def force_name(
        self,
        schema: Schema,
        suggested: str,
    ) -> str:

        old = self.names.get(
            id(schema)
        )

        if old is not None:
            return old

        name = self.fresh_name(
            suggested
        )

        self.names[
            id(schema)
        ] = name

        return name

    def field_name(
        self,
        key: str,
        owner: str,
    ) -> str:

        base = (
            lower_camel(
                key
            )
            + upper_camel(
                owner
            )
        )

        if (
            base
            in HASKELL_RESERVED
        ):
            base += "Field"

        candidate = base

        number = 2

        while (
            candidate
            in self.used_field_names
        ):
            candidate = (
                f"{base}"
                f"{number}"
            )

            number += 1

        self.used_field_names.add(
            candidate
        )

        return candidate

    # ------------------------------------------------------------------
    # Haskell types
    # ------------------------------------------------------------------

    def hs_type(
        self,
        schema: Schema,
        keep_nullable: bool = True,
    ) -> str:

        nullable = (
            schema.nullable
            if keep_nullable
            else False
        )

        if (
            schema.kind
            == "text"
        ):
            result = "Text"

        elif (
            schema.kind
            == "integer"
        ):
            result = "Integer"

        elif (
            schema.kind
            == "scientific"
        ):
            result = (
                "Scientific"
            )

        elif (
            schema.kind
            == "bool"
        ):
            result = "Bool"

        elif (
            schema.kind
            == "unknown"
        ):
            result = "Value"

        elif (
            schema.kind
            == "opaque_map"
        ):
            result = (
                "Mp.Map Text Value"
            )

        elif (
            schema.kind
            == "array"
        ):
            if (
                schema.item
                is None
            ):
                result = "[Value]"

            else:
                result = (
                    "["
                    + self.hs_type(
                        schema.item
                    )
                    + "]"
                )

        elif (
            schema.kind
            == "indexed_vector"
        ):
            if (
                schema.item
                is None
            ):
                inner = "Value"

            else:
                inner = self.hs_type(
                    schema.item
                )

            result = (
                "V.Vector "
                + inner
            )

        elif (
            schema.kind
            == "object"
        ):
            name = (
                self.assign_name(
                    schema
                )
            )

            self.emit_object(
                schema,
                name,
            )

            result = name

        elif (
            schema.kind
            == "tagged_union"
        ):
            name = (
                self.assign_name(
                    schema
                )
            )

            self.emit_tagged_union(
                schema,
                name,
            )

            result = name

        elif (
            schema.kind
            == "union"
        ):
            name = (
                self.assign_name(
                    schema
                )
            )

            self.emit_union(
                schema,
                name,
            )

            result = name

        else:
            result = "Value"

        if nullable:
            return (
                f"Maybe {result}"
            )

        return result

    # ------------------------------------------------------------------
    # Special field parser expressions
    # ------------------------------------------------------------------

    def parser_expression(
        self,
        json_key: str,
        field_schema: FieldSchema,
        object_observations: int,
    ) -> str:
        """
        Produce the expression yielding one constructor argument.

        Ordinary fields produce:

            o .: "field"
            o .:? "field"

        metadata produces:

            objectToMap <$> o .: "metadata"

        and an indexed-vector field produces:

            o .: "code_blocks" >>= indexedObjectToVector
        """

        schema = (
            field_schema.schema
        )

        missing = (
            field_schema.present
            < object_observations
        )

        null_value = (
            schema.nullable
        )

        key = json.dumps(
            json_key,
            ensure_ascii=False,
        )

        # --------------------------------------------------------------
        # Opaque JSON object -> Map Text Value
        # --------------------------------------------------------------

        if (
            schema.kind
            == "opaque_map"
        ):
            if missing:
                return (
                    "(fmap objectToMap "
                    f"<$> o .:? {key})"
                )

            if null_value:
                return (
                    "(fmap objectToMap "
                    f"<$> o .: {key})"
                )

            return (
                "(objectToMap "
                f"<$> o .: {key})"
            )

        # --------------------------------------------------------------
        # Numeric-keyed object -> Vector
        # --------------------------------------------------------------

        if (
            schema.kind
            == "indexed_vector"
        ):
            if missing:
                return (
                    f"(o .:? {key} "
                    ">>= traverse "
                    "indexedObjectToVector)"
                )

            if null_value:
                return (
                    f"(o .: {key} "
                    ">>= traverse "
                    "indexedObjectToVector)"
                )

            return (
                f"(o .: {key} "
                ">>= indexedObjectToVector)"
            )

        # --------------------------------------------------------------
        # Normal JSON property
        # --------------------------------------------------------------

        operator = (
            ".:?"
            if missing
            else ".:"
        )

        return (
            f"o {operator} {key}"
        )

    # ------------------------------------------------------------------
    # Object
    # ------------------------------------------------------------------

    def emit_object(
        self,
        schema: Schema,
        name: str,
    ) -> None:

        if (
            name
            in self.emitted
        ):
            return

        self.emitted.add(
            name
        )

        self.names[
            id(schema)
        ] = name

        slot = len(
            self.definitions
        )

        self.definitions.append(
            None
        )

        rendered = []

        for (
            json_key,
            field_schema,
        ) in schema.fields.items():

            selector = (
                self.field_name(
                    json_key,
                    name,
                )
            )

            inner_type = (
                self.hs_type(
                    field_schema.schema,
                    keep_nullable=False,
                )
            )

            missing = (
                field_schema.present
                < schema.observations
            )

            null_value = (
                field_schema.schema.nullable
            )

            hs_type = (
                f"Maybe {inner_type}"
                if (
                    missing
                    or null_value
                )
                else inner_type
            )

            parser = (
                self.parser_expression(
                    json_key,
                    field_schema,
                    schema.observations,
                )
            )

            rendered.append(
                (
                    selector,
                    hs_type,
                    parser,
                )
            )

        # Empty object.
        if not rendered:
            lines = [
                f"data {name} = {name}",
                "  deriving (Show, Eq)",
                "",
                (
                    f"instance FromJSON "
                    f"{name} where"
                ),
                (
                    f'  parseJSON = '
                    f'withObject "{name}" '
                    f'$ \\_ -> pure {name}'
                ),
            ]

        else:
            lines = [
                f"data {name} = {name}"
            ]

            for (
                index,
                (
                    selector,
                    hs_type,
                    _parser,
                ),
            ) in enumerate(
                rendered
            ):
                prefix = (
                    "  { "
                    if index == 0
                    else "  , "
                )

                lines.append(
                    f"{prefix}"
                    f"{selector} :: "
                    f"{hs_type}"
                )

            lines.extend(
                [
                    (
                        "  } deriving "
                        "(Show, Eq)"
                    ),
                    "",
                    (
                        f"instance FromJSON "
                        f"{name} where"
                    ),
                    (
                        f'  parseJSON = '
                        f'withObject "{name}" '
                        f'$ \\o ->'
                    ),
                    f"    {name}",
                ]
            )

            for (
                index,
                (
                    _selector,
                    _hs_type,
                    parser,
                ),
            ) in enumerate(
                rendered
            ):
                operator = (
                    "<$>"
                    if index == 0
                    else "<*>"
                )

                lines.append(
                    f"      {operator} "
                    f"{parser}"
                )

        self.definitions[
            slot
        ] = "\n".join(
            lines
        )

    # ------------------------------------------------------------------
    # Tagged union
    # ------------------------------------------------------------------

    def tag_constructor(
        self,
        tag: str,
        union_name: str,
        used: set,
    ) -> str:

        base = (
            upper_camel(
                tag
            )
            + union_name
        )

        candidate = base

        number = 2

        while (
            candidate
            in used
        ):
            candidate = (
                f"{base}"
                f"{number}"
            )

            number += 1

        used.add(
            candidate
        )

        return candidate

    def emit_tagged_union(
        self,
        schema: Schema,
        name: str,
    ) -> None:

        if (
            name
            in self.emitted
        ):
            return

        self.emitted.add(
            name
        )

        self.names[
            id(schema)
        ] = name

        slot = len(
            self.definitions
        )

        self.definitions.append(
            None
        )

        alternatives = []

        used_constructors = set()

        for (
            tag,
            payload,
        ) in (
            schema
            .tagged_variants
            .items()
        ):
            constructor = (
                self.tag_constructor(
                    tag,
                    name,
                    used_constructors,
                )
            )

            payload_name = (
                self.force_name(
                    payload,
                    (
                        constructor
                        + "Payload"
                    ),
                )
            )

            self.emit_object(
                payload,
                payload_name,
            )

            alternatives.append(
                (
                    tag,
                    constructor,
                    payload_name,
                )
            )

        lines = [
            f"data {name}"
        ]

        for (
            index,
            (
                _tag,
                constructor,
                payload_name,
            ),
        ) in enumerate(
            alternatives
        ):
            prefix = (
                "  = "
                if index == 0
                else "  | "
            )

            lines.append(
                f"{prefix}"
                f"{constructor} "
                f"{payload_name}"
            )

        lines.extend(
            [
                (
                    "  deriving "
                    "(Show, Eq)"
                ),
                "",
                (
                    f"instance FromJSON "
                    f"{name} where"
                ),
                (
                    f'  parseJSON = '
                    f'withObject "{name}" '
                    f'$ \\o -> do'
                ),
                (
                    "    tagValue <- "
                    f"o .: "
                    f"{json.dumps(schema.discriminator)}"
                ),
                (
                    "    case "
                    "(tagValue :: Text) of"
                ),
            ]
        )

        for (
            tag,
            constructor,
            _payload_name,
        ) in alternatives:
            literal = json.dumps(
                tag,
                ensure_ascii=False,
            )

            lines.append(
                f"      {literal} -> "
                f"{constructor} "
                "<$> parseJSON "
                "(Object o)"
            )

        discriminator = (
            schema.discriminator
            or "discriminator"
        )

        lines.extend(
            [
                "      other ->",
                (
                    "        fail "
                    f'("Unknown {discriminator} '
                    f'for {name}: " '
                    "<> T.unpack other)"
                ),
            ]
        )

        self.definitions[
            slot
        ] = "\n".join(
            lines
        )

    # ------------------------------------------------------------------
    # Ordinary union
    # ------------------------------------------------------------------

    @staticmethod
    def variant_tag(
        schema: Schema,
    ) -> str:

        return {
            "text": "Text",
            "integer": "Integer",
            "scientific": "Scientific",
            "bool": "Bool",
            "object": "Object",
            "array": "Array",
            "indexed_vector": "Vector",
            "opaque_map": "Map",
            "tagged_union": "Tagged",
            "unknown": "Value",
        }.get(
            schema.kind,
            upper_camel(
                schema.kind
            ),
        )

    def union_variant_type(
        self,
        variant: Schema,
        union_name: str,
    ) -> str:

        if (
            variant.kind
            == "object"
        ):
            name = (
                self.force_name(
                    variant,
                    (
                        "Object"
                        + union_name
                        + "Payload"
                    ),
                )
            )

            self.emit_object(
                variant,
                name,
            )

            return name

        if (
            variant.kind
            == "tagged_union"
        ):
            name = (
                self.force_name(
                    variant,
                    (
                        "Tagged"
                        + union_name
                    ),
                )
            )

            self.emit_tagged_union(
                variant,
                name,
            )

            return name

        return self.hs_type(
            variant,
            keep_nullable=False,
        )

    def emit_union(
        self,
        schema: Schema,
        name: str,
    ) -> None:

        if (
            name
            in self.emitted
        ):
            return

        self.emitted.add(
            name
        )

        self.names[
            id(schema)
        ] = name

        slot = len(
            self.definitions
        )

        self.definitions.append(
            None
        )

        alternatives = []

        used_tags = set()

        for variant in (
            schema.variants
        ):
            base = (
                self.variant_tag(
                    variant
                )
            )

            tag = base
            number = 2

            while (
                tag
                in used_tags
            ):
                tag = (
                    f"{base}"
                    f"{number}"
                )

                number += 1

            used_tags.add(
                tag
            )

            constructor = (
                f"{name}"
                f"As{tag}"
            )

            value_type = (
                self.union_variant_type(
                    variant,
                    name,
                )
            )

            alternatives.append(
                (
                    constructor,
                    value_type,
                )
            )

        lines = [
            f"data {name}"
        ]

        for (
            index,
            (
                constructor,
                value_type,
            ),
        ) in enumerate(
            alternatives
        ):
            prefix = (
                "  = "
                if index == 0
                else "  | "
            )

            lines.append(
                f"{prefix}"
                f"{constructor} "
                f"{value_type}"
            )

        lines.extend(
            [
                (
                    "  deriving "
                    "(Show, Eq)"
                ),
                "",
                (
                    f"instance FromJSON "
                    f"{name} where"
                ),
                "  parseJSON v =",
            ]
        )

        for (
            index,
            (
                constructor,
                _value_type,
            ),
        ) in enumerate(
            alternatives
        ):
            prefix = (
                "    "
                if index == 0
                else "    <|> "
            )

            lines.append(
                f"{prefix}"
                f"({constructor} "
                "<$> parseJSON v)"
            )

        self.definitions[
            slot
        ] = "\n".join(
            lines
        )

    # ------------------------------------------------------------------
    # Whole module
    # ------------------------------------------------------------------

    def generate(
        self,
        schema: Schema,
    ) -> str:

        root_wrapper = None

        if (
            schema.kind
            in {
                "object",
                "tagged_union",
                "union",
            }
            and not schema.nullable
        ):
            self.names[
                id(schema)
            ] = (
                self.root_name
            )

            if (
                schema.kind
                == "object"
            ):
                self.emit_object(
                    schema,
                    self.root_name,
                )

            elif (
                schema.kind
                == "tagged_union"
            ):
                self.emit_tagged_union(
                    schema,
                    self.root_name,
                )

            else:
                self.emit_union(
                    schema,
                    self.root_name,
                )

        else:
            inner_type = (
                self.hs_type(
                    schema
                )
            )

            accessor = (
                "value"
                + self.root_name
            )

            root_wrapper = (
                "\n".join(
                    [
                        (
                            f"newtype "
                            f"{self.root_name} = "
                            f"{self.root_name}"
                        ),
                        (
                            f"  {{ {accessor} :: "
                            f"{inner_type}"
                        ),
                        (
                            "  } deriving "
                            "(Show, Eq)"
                        ),
                        "",
                        (
                            f"instance FromJSON "
                            f"{self.root_name} where"
                        ),
                        (
                            "  parseJSON v = "
                            f"{self.root_name} "
                            "<$> parseJSON v"
                        ),
                    ]
                )
            )

        header = "\n".join(
            [
                (
                    "{-# LANGUAGE "
                    "OverloadedStrings #-}"
                ),
                "",
                (
                    f"module "
                    f"{self.module_name} where"
                ),
                "",
                (
                    "import "
                    "Control.Applicative "
                    "((<|>))"
                ),
                (
                    "import Data.Aeson "
                    "(FromJSON(..), Object, "
                    "Value(..), withObject, "
                    "(.:), (.:?))"
                ),
                (
                    "import Data.Aeson.Key "
                    "qualified as K"
                ),
                (
                    "import Data.Aeson.KeyMap "
                    "qualified as Km"
                ),
                (
                    "import Data.Aeson.Types "
                    "(Parser)"
                ),
                (
                    "import Data.List "
                    "(sortOn)"
                ),
                (
                    "import Data.Map.Strict "
                    "qualified as Mp"
                ),
                (
                    "import Data.Scientific "
                    "(Scientific)"
                ),
                (
                    "import Data.Text "
                    "(Text)"
                ),
                (
                    "import Data.Text "
                    "qualified as T"
                ),
                (
                    "import Data.Vector "
                    "qualified as V"
                ),
                (
                    "import Text.Read "
                    "(readMaybe)"
                ),
                "",
                (
                    "objectToMap :: "
                    "Object -> "
                    "Mp.Map Text Value"
                ),
                (
                    "objectToMap anObject ="
                ),
                (
                    "  Mp.fromList"
                ),
                (
                    "    [ (K.toText key, value)"
                ),
                (
                    "    | (key, value) "
                    "<- Km.toList anObject"
                ),
                (
                    "    ]"
                ),
                "",
                (
                    "indexedObjectToVector"
                ),
                (
                    "  :: FromJSON a"
                ),
                (
                    "  => Object"
                ),
                (
                    "  -> Parser "
                    "(V.Vector a)"
                ),
                (
                    "indexedObjectToVector "
                    "anObject = do"
                ),
                (
                    "  indexed <-"
                ),
                (
                    "    traverse "
                    "toIndexed"
                ),
                (
                    "      (Km.toList "
                    "anObject)"
                ),
                "",
                (
                    "  let ordered ="
                ),
                (
                    "        fmap snd"
                ),
                (
                    "          (sortOn fst "
                    "indexed)"
                ),
                "",
                (
                    "  V.fromList"
                ),
                (
                    "    <$> traverse "
                    "parseJSON ordered"
                ),
                "  where",
                (
                    "    toIndexed"
                ),
                (
                    "      :: (K.Key, Value)"
                ),
                (
                    "      -> Parser "
                    "(Integer, Value)"
                ),
                (
                    "    toIndexed "
                    "(key, value) ="
                ),
                (
                    "      case"
                ),
                (
                    "        readMaybe"
                ),
                (
                    "          (T.unpack "
                    "(K.toText key))"
                ),
                (
                    "      of"
                ),
                (
                    "        Just n"
                ),
                (
                    "          | n >= 0 ->"
                ),
                (
                    "              pure "
                    "(n, value)"
                ),
                "",
                (
                    "        _ ->"
                ),
                (
                    "          fail"
                ),
                (
                    '            ("Invalid '
                    'indexed object key: "'
                ),
                (
                    "            <> "
                    "T.unpack "
                    "(K.toText key))"
                ),
                "",
            ]
        )

        bodies = [
            definition
            for definition
            in self.definitions
            if definition is not None
        ]

        if (
            root_wrapper
            is not None
        ):
            bodies.append(
                root_wrapper
            )

        return (
            header
            + "\n"
            + "\n\n".join(
                bodies
            )
            + "\n"
        )


# ---------------------------------------------------------------------------
# Diagnostics
# ---------------------------------------------------------------------------

def path_text(
    path: JsonPath,
) -> str:
    result = ""

    for component in path:
        if component == "$":
            result = "$"

        elif component == "[]":
            result += "[]"

        elif component.startswith(
            "@"
        ):
            result += (
                "."
                + component
            )

        else:
            result += (
                "."
                + component
            )

    return result


def show_schema(
    schema: Schema,
    label: str = "$",
    indent: int = 0,
) -> None:

    pad = (
        " "
        * indent
    )

    null = (
        " nullable"
        if schema.nullable
        else ""
    )

    path = path_text(
        schema.path
    )

    if (
        schema.kind
        == "object"
    ):
        print(
            (
                f"{pad}{label}: "
                f"object "
                f"path={path} "
                f"observations="
                f"{schema.observations}"
                f"{null}"
            ),
            file=sys.stderr,
        )

        for (
            key,
            field_schema,
        ) in schema.fields.items():

            optional = (
                field_schema.present
                < schema.observations
            )

            marker = (
                " optional"
                if optional
                else ""
            )

            print(
                (
                    f"{pad}  {key}: "
                    f"present="
                    f"{field_schema.present}/"
                    f"{schema.observations}"
                    f"{marker}"
                ),
                file=sys.stderr,
            )

            show_schema(
                field_schema.schema,
                key,
                indent + 4,
            )

    elif (
        schema.kind
        in {
            "array",
            "indexed_vector",
        }
    ):
        print(
            (
                f"{pad}{label}: "
                f"{schema.kind} "
                f"path={path}"
                f"{null}"
            ),
            file=sys.stderr,
        )

        if (
            schema.item
            is None
        ):
            print(
                (
                    f"{pad}  "
                    "<empty>"
                ),
                file=sys.stderr,
            )

        else:
            show_schema(
                schema.item,
                "item",
                indent + 2,
            )

    elif (
        schema.kind
        == "opaque_map"
    ):
        print(
            (
                f"{pad}{label}: "
                "opaque_map "
                "-> Map Text Value "
                f"path={path}"
                f"{null}"
            ),
            file=sys.stderr,
        )

    elif (
        schema.kind
        == "tagged_union"
    ):
        print(
            (
                f"{pad}{label}: "
                "tagged_union "
                f"path={path} "
                "discriminator="
                f"{schema.discriminator!r} "
                "observations="
                f"{schema.observations}"
                f"{null}"
            ),
            file=sys.stderr,
        )

        for (
            tag,
            variant,
        ) in (
            schema
            .tagged_variants
            .items()
        ):
            show_schema(
                variant,
                (
                    f"{schema.discriminator}"
                    f"={tag!r}"
                ),
                indent + 2,
            )

    elif (
        schema.kind
        == "union"
    ):
        print(
            (
                f"{pad}{label}: "
                f"union "
                f"path={path}"
                f"{null}"
            ),
            file=sys.stderr,
        )

        for (
            index,
            variant,
        ) in enumerate(
            schema.variants
        ):
            show_schema(
                variant,
                (
                    "alternative"
                    f"[{index}]"
                ),
                indent + 2,
            )

    else:
        print(
            (
                f"{pad}{label}: "
                f"{schema.kind} "
                f"path={path}"
                f"{null}"
            ),
            file=sys.stderr,
        )


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def parse_arguments() -> argparse.Namespace:

    parser = argparse.ArgumentParser(
        description=(
            "Infer path-sensitive "
            "Haskell data definitions "
            "and Aeson parsers from JSON."
        )
    )

    parser.add_argument(
        "json_file",
        type=Path,
    )

    parser.add_argument(
        "--module",
        default="Generated.Schema",
    )

    parser.add_argument(
        "--root",
        default="Root",
    )

    parser.add_argument(
        "--discriminator",
        default="content_type",
    )

    parser.add_argument(
        "--no-tagged-unions",
        action="store_true",
    )

    parser.add_argument(
        "--opaque-map-field",
        action="append",
        default=None,
        metavar="FIELD",
        help=(
            "field to treat as "
            "Map Text Value; "
            "can be repeated. "
            "Default: metadata"
        ),
    )

    parser.add_argument(
        "--indexed-vector-field",
        action="append",
        default=None,
        metavar="FIELD",
        help=(
            "numeric-keyed object field "
            "to treat as Vector; "
            "can be repeated. "
            "Default: code_blocks"
        ),
    )

    parser.add_argument(
        "--show-schema",
        action="store_true",
    )

    parser.add_argument(
        "-o",
        "--output",
        type=Path,
    )

    return parser.parse_args()


def main() -> int:

    args = (
        parse_arguments()
    )

    try:
        with (
            args.json_file.open(
                "r",
                encoding="utf-8",
            )
        ) as handle:
            document = (
                json.load(
                    handle
                )
            )

    except FileNotFoundError:
        print(
            (
                "error: file not found: "
                f"{args.json_file}"
            ),
            file=sys.stderr,
        )

        return 1

    except json.JSONDecodeError as exc:
        print(
            (
                "error: invalid JSON: "
                f"{exc}"
            ),
            file=sys.stderr,
        )

        return 1

    opaque_fields = set(
        args.opaque_map_field
        if (
            args.opaque_map_field
            is not None
        )
        else [
            "metadata",
        ]
    )

    indexed_fields = set(
        args.indexed_vector_field
        if (
            args.indexed_vector_field
            is not None
        )
        else [
            "code_blocks",
        ]
    )

    config = InferenceConfig(
        discriminator=(
            None
            if args.no_tagged_unions
            else args.discriminator
        ),
        opaque_map_fields=(
            opaque_fields
        ),
        indexed_vector_fields=(
            indexed_fields
        ),
    )

    schema = infer_schema(
        document,
        ("$",),
        config,
    )

    if args.show_schema:
        show_schema(
            schema
        )

    generator = (
        HaskellGenerator(
            module_name=args.module,
            root_name=args.root,
        )
    )

    source = (
        generator.generate(
            schema
        )
    )

    if (
        args.output
        is None
    ):
        sys.stdout.write(
            source
        )

    else:
        args.output.write_text(
            source,
            encoding="utf-8",
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(
        main()
    )