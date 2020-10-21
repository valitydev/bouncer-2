package trivial.incorrect3

assertions := {
    "forbidden" : { why | forbidden[why] }
}

forbidden[why] {
    input
    not input.auth.method
    why := {}
}
