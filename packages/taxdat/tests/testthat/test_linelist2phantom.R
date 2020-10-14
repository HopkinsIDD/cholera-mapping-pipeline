## testing for linelist2phantom

test_that("case column names valid",{
  expect_error({
    dat = read.csv("gnb_linelist_test.csv")
    dat = dplyr::rename(dat,sCh=suspected_cases,dead = deaths) 
    linelist2phantom(dat,assumed_complete_location = "GNB")
  }, NA)
})


test_that("user columns exist",{
  expect_error({
    ## test TRs
    dat = read.csv("gnb_linelist_test.csv")
    linelist2phantom(linelist=dat,
                     time_left="TL",
                     time_right="TR",
                     assumed_complete_location = "GNB")
    
  })
  expect_error({
    ## test TL
    dat = read.csv("gnb_linelist_test.csv")
    linelist2phantom(linelist=dat,
                     time_left="TL",
                     time_right="TR",
                     assumed_complete_location = "GNB")
    
  })
  expect_error({
    ## test sum_cols
    dat = read.csv("gnb_linelist_test.csv")
    linelist2phantom(linelist=dat,
                     time_left="TL",
                     time_right="TR",
                     assumed_complete_location = "GNB")
    
  })
})




## locations should be who_region, ISO_A1, ISO_A2_L1
## when summing over locations that have missing data at a specific-level
## should have a flag for NA aggregation (with options to make interval censored observations or assume complete)
## Some tests with GNB data and more generally:
## Date formats: should throw an error if any date field is NA (user should have to add even upper bound unless we want to add an option to impute missing dates with end of epidemic date?)
## Check that missing spatial level data results in appropriate censored obs being created
## Should do some basic standardization of area names (just perhaps just to title case?)

