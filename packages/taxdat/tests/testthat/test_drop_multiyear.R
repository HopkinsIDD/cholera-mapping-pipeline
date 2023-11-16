## testing for drop_multiyear


test_that("drop_multiyear works", {
  df1 <- tibble(
    TL = as.Date("2021-01-02"),
    TR = as.Date("2021-01-04"),
    admin_level = 0
  )
  
  df2 <- tibble(
    TL = as.Date("2021-01-02"),
    TR = as.Date("2022-01-04"),
    admin_level = 0
  )
  
  # No drop
  test1 <- drop_multiyear(df1, admin_levels = 0)
  expect_identical(test1, df1)
  
  # Don't drop data of other admin levels
  test2 <- drop_multiyear(df2, admin_levels = 1)
  expect_identical(test2, df2)
  
  # Drop data
  test2a <- drop_multiyear(df2, admin_levels = 0)
  expect_equal(nrow(test2a), 0)
  
  # Missing admin_level column
  expect_error(drop_multiyear(df2 %>% dplyr::select(-admin_level), admin_levels = 0))
})
