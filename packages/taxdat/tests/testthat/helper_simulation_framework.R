test_if_seed_setting_works <- function(fun, ...) {
    seed <- get_or_set_seed()
    expect_error({
        result <- fun(...)
    }, NA)
    result <- fun(...)

    expect_equal({
        fun(seed = attr(result, "seed"), ...)
    }, result)
}
