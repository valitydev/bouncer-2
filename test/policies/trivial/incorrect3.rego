package trivial.incorrect3

judgement := {
    "resolution": ["forbidden", forbidden, allowed]
}

forbidden[why] {
    input
    not input.auth.method
    why := {}
}

allowed[why] = description {
    input.auth.method == "SessionToken"
    input.user
    why := "its_a_user"
    description := "Then why not?"
}
