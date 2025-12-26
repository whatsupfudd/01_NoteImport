# Doc Importer

Imports documents in different formats into the 0to1,Done versioned hierarchical block structure.

Usage: docimp [-c|--config notionappCONF] [-d|--debug DEBUGLVL] COMMAND

  notionapp.

Available options:
  -h,--help                Show this help text
  -c,--config notionappCONF
                           Global config file (default is
                           ~/.notionapp/config.yaml). (default: "")
  -d,--debug DEBUGLVL      Global debug state. (default: "")

Available commands:
  oaijson                  OpenAI JSON command.
  ingest                   Ingest command.
  docx                     DocX command.
  notion                   Notion command.
  version                  Shows the version number of importer.
  help                     Help about any command.

## Details

Usage: docimp oaijson [-e|--export] JSONFILE

  OpenAI JSON command.

Available options:
  -e,--export              Comes from the OpenAI export service.
  JSONFILE                 JSON file file path
  -h,--help                Show this help text


### Usage: docimp ingest [-f|--format docx|html|markdown] 
                     ((-i|--file FILE) | --stdin) [--structure pandoc|xml|auto] 
                     [--enrich none|min|full] [--keep-original] [--title TEXT] 
                     [--format-label TEXT] [--out json|pretty] 
                     [--write-json FILE] [--user USERNAME] [--doc-id DOCID]

  Ingest command.

Available options:
  -f,--format docx|html|markdown
                           Input format (default: docx)
  -i,--file FILE           Input file
  --stdin                  Read from stdin
  --structure pandoc|xml|auto
                           Choose structural importer for DOCX (default: pandoc)
  --enrich none|min|full   DOCX XML enrichment level (default: min)
  --keep-original          Retain original bytes in result
  --title TEXT             Override title
  --format-label TEXT      Override format label (default: docx/html/markdown)
  --out json|pretty        Output mode (default: pretty)
  --write-json FILE        Write JSON to file (default: stdout)
  --user USERNAME          User
  --doc-id DOCID           Document ID
  -h,--help                Show this help text

### Usage: docimp docx (-i|--in FILE) [-o|--out FILE] [--yaml] [--promote]

  DocX command.

Available options:
  -i,--in FILE             Input file path
  -o,--out FILE            Output file path
  --yaml                   Output as YAML
  --promote                Promote numbered paragraphs to headers
  -h,--help                Show this help text

### Usage: docimp notion WORDSPACE

  Notion command.

Available options:
  WORDSPACE                Notion workspace to use.
  -h,--help                Show this help text
