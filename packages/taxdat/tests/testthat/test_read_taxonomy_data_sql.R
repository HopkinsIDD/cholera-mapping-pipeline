dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

library(testthat)
#test read_taxonomy_data_sql function
test_that("read_taxonomy_data_sql works",{
  #1. Missing arguments
  ## missing user name
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=NULL,password=password,locations=14)
    )
  ## missing password
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=NULL,locations=14)
    )
  ## missing locations (for now, locations can't be null)
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=NULL,uids=314)#This OC has data from WHO annual reports.
    )
  #2. Arguments with invalid formats
  ## locations have to be numbers (unique location ids)
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations="AFR::ETH")
    )
  ## time_left and time_right have to be date format
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,time_left="200-01-01",time_right="2000-12-31")
  )
  ## uids have to be integer. (unique identifiers of observation collections)
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,uids=31.4)
  )  
  # 3. Arguments with values exceeding the valid ranges. 
  ## time_left is later than time_right
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,time_left="2000-01-01",time_right="1999-01-01")
  )  
  ## uids <0
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,uids=-314)
  )
  ## Locations is null
  expect_error(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=NULL,uids=314)
  )  
  # 4. All locations are either the locations specified in the arguments or their child locations. 
  taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,time_left=NULL,time_right=NULL,uids=21136)
  #expect all locations are correct under Ethiopia
  expect_true(
    all(stringr::str_detect(taxonomy_data$location_name,"AFR::ETH"))
  )
  # 5. All dates are within the range of time_left and time_right
  taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,time_left="2019-01-01",time_right="2020-08-02",uids=21136)
  expect_true(
    all(taxonomy_data$TL>=as.Date("2019-01-01")&(taxonomy_data$TR<=as.Date("2020-08-02")))
  )
  # 6. All uids are within the uids 
  taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,uids=c(314,21041,21136))
  expect_true(
    any(taxonomy_data$observation_collection_id==314)&any(taxonomy_data$observation_collection_id==21041)&any(taxonomy_data$observation_collection_id==21136)
  ) 
  # 7. No duplicate data
  taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14,uids=c(314,21041,21136))
  expect_false(
    any(duplicated(taxonomy_data))
  )
  # 8. Memory warnings: tell users that they are pulling all the data for a region/time period
  ## when time_right/time_left are null, they are pulling all the data for a region (including data linked to its child locations)
  expect_warning(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,locations=14),
    "No time filters."
  )
  ## when locations are null, they are pulling all the data for a time period (including data for its child locations)
  expect_warning(
    taxonomy_data=read_taxonomy_data_sql(username=username,password=password,location=NULL,time_left="2000-01-01",time_right="2000-12-31"),
    "No location filters."
  )
})