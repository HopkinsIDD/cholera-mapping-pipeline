## testing for extraction of admin level

test_that("get_admin_level works general", {
  
  loc1 <- "AFR::CMR"
  loc2 <- "EMR::SDN::aa"
  loc3 <- "EMR::SDN::aa::aa"
  
  expect_equal(get_admin_level(loc1), 0)
  expect_equal(get_admin_level(loc2), 1)
  expect_equal(get_admin_level(loc3), 2)
  
}) 


test_that("get_admin_level works for TZA", {
  
  loc1 <- "AFR::TZA"
  loc1.1 <- "AFR::TZA::aa::aa"
  loc2 <- "AFR::TZA::Mainland"
  loc2.1 <- "AFR::TZA::Mainland::aa"
  loc3 <- "AFR::TZA::Zanzibar"
  loc3.1 <- "AFR::TZA::Zanzibar::aa"
  
  expect_identical(get_admin_level(loc1), 0)
  expect_identical(get_admin_level(loc1.1), 2)
  expect_identical(get_admin_level(loc2), 0)
  expect_identical(get_admin_level(loc2.1), 1)
  expect_identical(get_admin_level(loc3), 0)
  expect_identical(get_admin_level(loc3.1), 1)
  
}) 
