username=Sys.getenv("username")
password=Sys.getenv("password")

library(testthat)
#test read_taxonomy_data_sql function
test_that("read_taxonomy_data_sql works",{
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=NULL,password=password,locations=14)
    )
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=NULL,locations=14)
    )
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=NULL)
    )
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations="AFR::ETH")
    )
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,time_left=as.Date("2019-06-01"),time_right=as.Date("2019-01-01"))
    )
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,time_left=NULL,time_right=NULL,uids=301)
    )
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=2,time_left=as.Date("3000-01-01"),time_right=NULL,uids=301)
    )
  
  #expect extract the correct data
  skip("Skip the following tests which require pulling data from the server until testing database is ready.")
  
  expect_warning(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=2,time_left=NULL,time_right=NULL,uids=NULL),
    "No filters specified on data pull, pulling all data."
  )
  
  taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=2,time_left=NULL,time_right=NULL,uids=301) #location 2 is Burund.
  
  expect_false(
    any(duplicated(taxonomy_data))
  )
  expect_equal(
    taxonomy_data$observation_collection_id,301
  )
  expect_equal(
    taxonomy_data$location_id,2
  )
  
  # expect extract the correct data for multiple locations
  taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=c(28365,28366,28369),time_left=NULL,time_right=NULL,uids=NULL)
  expect_false(
    any(duplicated(taxonomy_data))
  )
  expect_true(
    all(unique(taxonomy_data$location_id)%in%c(28365,28366,28369))
  )

  # expect extract the correct data for multiple observation collections
  taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=2,time_left=NULL,time_right=NULL,uids=c(301,812,3540))
  expect_false(
    any(duplicated(taxonomy_data))
  )
  expect_true(
    all(unique(taxonomy_data$observation_collection_id)%in%c(301,812,3540))
  )
  expect_true(
    unique(taxonomy_data$location_id)==2
  )
  
  #expect the time format is correct and falls in the correct time range
  taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=405,time_left=NULL,time_right=NULL,uids=c(20378))#location 405 is Kenya
  expect_true(
    min(as.Date(taxonomy_data$TL))==as.Date("2012-01-26")
  )
  expect_true(
    max(as.Date(taxonomy_data$TR))==as.Date("2012-12-09")
  )
  expect_true(
    all(taxonomy_data$TR>=axonomy_data$TL)
  )
  #expect all locations are correct under Kenya
  expect_true(
    all(stringr::str_detect(taxonomy_data$location_name,"AFR::KEN"))
  )
})