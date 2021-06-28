location_df <- data.frame(qualified_name = c("testlocation", "testlocation2"))
location_period_df <- data.frame(qualified_name = location_df$qualified_name, start_date = lubridate::ymd("2000-01-01", 
    "2001-01-01"), end_date = lubridate::ymd("2000-12-31", "2001-12-31"))
shapes_df <- sf::st_sf(qualified_name = location_df$qualified_name, start_date = location_period_df$start_date, 
    end_date = location_period_df$end_date, geom = sf::st_sfc(sf::st_polygon(list(matrix(c(0, 
        0, 0, 1, 1, 1, 1, 0, 0, 0), byrow = TRUE, ncol = 2))), sf::st_polygon(list(matrix(c(0, 
        0, 0, 1, 1, 1, 1, 0, 0, 0), byrow = TRUE, ncol = 2)))))
observations_df <- data.frame(observation_collection_id = c(1, 1, 2, 2), time_left = lubridate::ymd(c("2000-01-01", 
    "2000-06-15", "2001-01-01", "2001-06-15")), time_right = lubridate::ymd(c("2000-06-14", 
    "2000-12-31", "2001-06-14", "2001-12-31")), qualified_name = c("testlocation", 
    "testlocation", "testlocation2", "testlocation2"), primary = rep(TRUE, times = 4), 
    phantom = rep(FALSE, times = 4), suspected_cases = 9:12, confirmed_cases = 5:8, 
    deaths = 1:4)
