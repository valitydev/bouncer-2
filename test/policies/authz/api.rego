package authz.api

import data.authz.blacklists

assertions := {
    "forbidden" : { why | forbidden[why] },
    "allowed"   : { why | allowed[why] }
}

# Set of assertions which tell why operation under the input context is forbidden.
# When the set is empty operation is not explicitly forbidden.
# Each element must be a 2-item array of the following form:
# ```
# ["code", "description"]
# ```
forbidden[why] {
    input
    not input.auth.method
    why := [
        "auth_required",
        "Authorization is required"
    ]
}

forbidden[why] {
    exp := time.parse_rfc3339_ns(input.auth.expiration)
    now := time.parse_rfc3339_ns(input.env.now)
    now > exp
    why := [
        "auth_expired",
        sprintf("Authorization is expired at: %s", [input.auth.expiration])
    ]
}

forbidden[why] {
    ip := input.requester.ip
    blacklist := blacklists["source-ip-range"]
    matches := net.cidr_contains_matches(blacklist, ip)
    ranges := [ range | matches[_][0] = i; range := blacklist[i] ]
    why := [
        "ip_range_blacklisted",
        sprintf("Requester IP address is blacklisted with ranges: %v", [concat(", ", ranges)])
    ]
}

# Set of assertions which tell why operation under the input context is allowed.
# When the set is empty operation is not explicitly allowed.
# Each element must be a 2-item array of the following form:
# ```
# ["code", "description"]
# ```
allowed[why] {
    input.auth.method == "SessionToken"
    input.user
    org_allowed[why]
}

org_allowed[why] {
    org := org_by_operation
    org.owner == input.user.id
    why := [
        "user_is_owner",
        "User is the organisation owner itself"
    ]
}

org_allowed[why] {
    rolename := role_by_operation[_]
    org_by_operation.roles[i].id == rolename
    scopename := scopename_by_role[i]
    why := [
        "user_has_role",
        sprintf("User has role %s in scope %v", [rolename, scopename])
    ]
}

scopename_by_role[i] = sprintf("shop:%s", [shop]) {
    role := org_by_operation.roles[i]
    shop := role.scope.shop.id
    shop == input.capi.op.shop.id
}

scopename_by_role[i] = "*" {
    role := org_by_operation.roles[i]
    not role.scope
}

# Set of roles at least one of which is required to perform the operation in context.
role_by_operation["Manager"]
    { input.capi.op.id == "CreateInvoice" }
    { input.capi.op.id == "GetInvoiceByID" }
    { input.capi.op.id == "GetInvoiceEvents" }
    { input.capi.op.id == "FulfillInvoice" }
    { input.capi.op.id == "RescindInvoice" }
    { input.capi.op.id == "GetPayments" }
    { input.capi.op.id == "GetPaymentByID" }
    { input.capi.op.id == "CancelPayment" }
    { input.capi.op.id == "CapturePayment" }
    { input.capi.op.id == "GetRefunds" }
    { input.capi.op.id == "GetRefundByID" }
    { input.capi.op.id == "CreateRefund" }
    { input.capi.op.id == "CreateInvoiceTemplate" }
    { input.capi.op.id == "GetInvoiceTemplateByID" }
    { input.capi.op.id == "UpdateInvoiceTemplate" }
    { input.capi.op.id == "DeleteInvoiceTemplate" }

role_by_operation["Administrator"]
    { input.orgmgmt.op.id == "ListInvitations" }
    { input.orgmgmt.op.id == "CreateInvitation" }
    { input.orgmgmt.op.id == "GetInvitation" }
    { input.orgmgmt.op.id == "RevokeInvitation" }

# Context of an organisation which is being operated upon.
org_by_operation = org_by_id[id]
    { id = input.capi.op.party.id }
    { id = input.orgmgmt.op.organization.id }

# A mapping of org ids to organizations.
org_by_id := { org.id: org | org := input.user.orgs[_] }
