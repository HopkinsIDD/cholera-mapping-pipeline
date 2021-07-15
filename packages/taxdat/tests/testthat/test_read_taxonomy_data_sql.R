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
  
  taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=2,time_left=NULL,time_right=NULL,uids=301)
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
})