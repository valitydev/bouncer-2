package service.authz.api.anapi

import input.anapi.op
import data.service.authz.roles

api_name := "AnalyticsAPI"

# Set of assertions which tell why operation under the input context is forbidden.
# Each element must be an object of the following form:
# ```
# {"code": "auth_expired", "description": "..."}
# ```
forbidden[why] {
    input.auth.method != "SessionToken"
    why := {
        "code": "unknown_auth_method_forbids_operation",
        "description": sprintf("Unknown auth method for this operation: %v", [input.auth.method])
    }
}

# Restrictions

restrictions[what] {
    not user_is_owner
    user_has_any_role_for_op
    what := {
        "anapi": {
            "op": {
                "shops": [shop | shop := op_shop_in_scope[_]]
            }
        }
    }
}

op_shop_in_scope[shop] {
    some i
    op.shops[i].id == user_roles_by_operation[_].scope.shop.id
    shop := op.shops[i]
}

# Set of assertions which tell why operation under the input context is allowed.
# Each element must be an object of the following form:
# ```
# {"code": "auth_expired", "description": "..."}
# ```

allowed[why] {
    user_is_owner
    why := {
        "code": "org_ownership_allows_operation",
        "description": "User is owner of organization that is subject of this operation"
    }
}

allowed[why] {
    user_role_id := user_roles_by_operation[_].id
    why := {
        "code": "org_role_allows_operation",
        "description": sprintf("User has role that permits this operation: %v", [user_role_id])
    }
}

user_is_owner {
    organization := org_by_operation
    input.user.id == organization.owner.id
}

user_has_any_role_for_op {
    user_roles_by_operation[_]
}

user_roles_by_operation[user_role] {
    user_role := org_by_operation.roles[_]
    op.id == roles.roles[user_role.id].apis[api_name].operations[_]
}

org_by_operation = org {
    org := input.user.orgs[_]
    org.id == op.party.id
}
