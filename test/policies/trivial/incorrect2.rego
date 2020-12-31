package trivial.incorrect2

judgement := {
    "resolution": ["forbidden", forbidden]
}

forbidden[why] = description {
    input
    not input.auth.method
    why := "auth_required"
    description := "Authorization is required"
}
