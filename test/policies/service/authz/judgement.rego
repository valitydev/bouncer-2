package service.authz.judgement

judge(assertions) = jm {
    count(assertions.forbidden) > 0
    jm := {
        "resolution": ["forbidden", assertions.forbidden]
    }
}

judge(assertions) = jm {
    count(assertions.forbidden) == 0
    assertions.restrictions != {}
    count(assertions.allowed) > 0
    jm := {
        "resolution": ["restricted", assertions.allowed],
        "restrictions": assertions.restrictions
    }
}

judge(assertions) = jm {
    count(assertions.forbidden) == 0
    assertions.restrictions == {}
    count(assertions.allowed) > 0
    jm := {
        "resolution": ["allowed", assertions.allowed]
    }
}

judge(assertions) = jm {
    count(assertions.forbidden) == 0
    count(assertions.allowed) == 0
    jm := {
        "resolution": ["forbidden", []]
    }
}
