SELECT rb.*, rp.*
FROM kms.role_binding rb
JOIN kms.role_permission rp ON rp.role_fk = rb.role_fk
JOIN kms.permission p ON p.uid = rp.permission_fk AND p.code = 'edit'
WHERE rb.principal='user' AND rb.user_fk = p_user
  AND (
    rb.scope='tenant'
    OR (rb.scope='domaintb' AND rb.scope_value = v_dom_code)
    OR (rb.scope='resource' AND rb.scope_value::int = p_document)
  )


SELECT d.uid, d.domain_fk, d.tier_fk, dm.code, tr.code
FROM kms.document d
JOIN kms.domaintb dm ON dm.uid = d.domain_fk
JOIN kms.tier tr ON tr.uid  = d.tier_fk;
WHERE d.uid = p_document;