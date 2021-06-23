## Basic test to make sure that testing setup works
test_that("testing works", {
    expect_error({
        1 + 1
    }, NA)
})

