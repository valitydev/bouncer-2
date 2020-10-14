package trivial.incorrect2

assertions := {
    "forbidden" : forbidden,
    "allowed" : allowed
}

forbidden[why] = description {
    input
    not input.auth.method
    why := "auth_required"
    description := "Authorization is required"
}

allowed[why] = description {
    input.auth.method == "SessionToken"
    input.user
    why := "its_a_user"
    description := "Then why not?"
}
