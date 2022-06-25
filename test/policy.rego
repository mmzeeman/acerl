## Zotonic builtin policy

package zotonic.z_acl

update_actions := {"admin", "insert", "update", "delete", "link"}

default allow = false

allow {
    input.context.acl == "admin" 
}

allow {
    input.context.user_id == 1 
}

deny {
    input.context.acl != "admin"
    input.context.acl_is_read_only

    update_actions[input.action]
}


is_allowed {
    allow 
    not deny
}

