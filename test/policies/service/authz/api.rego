package service.authz.api

import data.service.authz.api.invoice_access_token
import data.service.authz.api.url_shortener
import data.service.authz.api.binapi
import data.service.authz.api.anapi
import data.service.authz.blacklists
import data.service.authz.whitelists
import data.service.authz.roles
import data.service.authz.org
import data.service.authz.judgement

assertions := {
    "forbidden" : { why | forbidden[why] },
    "allowed"   : { why | allowed[why] },
    "restrictions": { what.type: what.restrictions[what.type] | restrictions[what] }
}

judgement := judgement.judge(assertions)

# Set of assertions which tell why operation under the input context is forbidden.
# When the set is empty operation is not explicitly forbidden.
# Each element must be an object of the following form:
# ```
# {"code": "auth_expired", "description": "..."}
# ```
forbidden[why] {
    input
    not input.auth.method
    why := {
        "code": "auth_required",
        "description": "Authorization is required"
    }
}

forbidden[why] {
    exp := time.parse_rfc3339_ns(input.auth.expiration)
    now := time.parse_rfc3339_ns(input.env.now)
    now > exp
    why := {
        "code": "auth_expired",
        "description": sprintf("Authorization is expired at: %s", [input.auth.expiration])
    }
}

forbidden[why] {
    ip := input.requester.ip
    blacklist := blacklists.source_ip_range
    matches := net.cidr_contains_matches(blacklist, ip)
    matches[_]
    ranges := [ range | matches[_][0] = i; range := blacklist[i] ]
    why := {
        "code": "ip_range_blacklisted",
        "description": sprintf(
            "Requester IP address is blacklisted with ranges: %v",
            [concat(", ", ranges)]
        )
    }
}

forbidden[why] {
    input.anapi
    anapi.forbidden[why]
}

warnings[why] {
    not blacklists.source_ip_range
    why := "Blacklist 'source_ip_range' is not defined, blacklisting by IP will NOT WORK."
}

warnings[why] {
    not whitelists.bin_lookup_allowed_party_ids
    why := "Whitelist 'bin_lookup_allowed_party_ids' is not defined, whitelisting by partyID will NOT WORK."
}

# Set of assertions which tell why operation under the input context is allowed.
# When the set is empty operation is not explicitly allowed.
# Each element must be an object of the following form:
# ```
# {"code": "auth_expired", "description": "..."}
# ```
allowed[why] {
    input.shortener
    url_shortener.allowed[why]
}

allowed[why] {
    input.binapi
    binapi.allowed[why]
}

allowed[why] {
    input.auth.method == "InvoiceAccessToken"
    invoice_access_token.allowed[why]
}

allowed[why] {
    input.anapi
    anapi.allowed[why]
}

# Restrictions

restrictions[what] {
    input.anapi
    rstns := anapi.restrictions[_]
    what := {
        "type": "anapi",
        "restrictions": rstns
    }
}
