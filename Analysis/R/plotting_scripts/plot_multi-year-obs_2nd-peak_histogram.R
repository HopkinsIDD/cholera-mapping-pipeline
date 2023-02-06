#=============================================================================================
# This is the script used to draw histograms of tfrac and sfrac distributions 
# 7/26/2022
# updated on 10/7/2022
#=============================================================================================

# Load the packages
library(dplyr)
library(ggplot2)

# Where the data folder is
data_dir <- "/home/.../data"

# Find and load the model output 
fns <- list.files(data_dir, full.names = FALSE, recursive=FALSE)
country_list <- unique(stringr::str_extract(fns, "^[A-Z]{3}"))
country_list <- country_list[country_list != "ZAF" & !is.na(country_list)] #delete ZAF
# country_list <- c("COD", "ETH", "NGA")
fns_filtered <- fns[grepl("stan_input.rdata$", fns) | grepl("preprocess.rdata$", fns)]

# Loop through all the files
# tfrac_table <- tibble::tibble(iso = as.character(),  admin_level = as.numeric(), tfrac = as.numeric())
# loctime	OC_UID	locationPeriod_id	TL	TR	attributes.fields.suspected_cases	tfrac	TL_diff	TR_diff	one_week_indicator	two_week_indicator
for(country_code in country_list){

  load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)][1], sep = "/"))
  load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)][2], sep = "/"))
  
  a <- stan_input$sf_cases_resized %>%
    mutate(idx = 1:nrow(stan_input$sf_cases_resized)) 
  a$tfrac = a$idx %>% purrr::map_chr( ~ paste(stan_input$stan_data$tfrac[which(. == stan_input$stan_data$map_obs_loctime_obs)], collapse=", "))

  b <- a %>%
    filter(lubridate::year(TL) != lubridate::year(TR)) %>%
    sf::st_drop_geometry() %>%
    mutate(case = attributes.fields.suspected_cases) %>%
    select(-idx, -attributes.fields.suspected_cases) %>%
    rowwise() %>%
    mutate(TL_diff = as.numeric(lubridate::ceiling_date(TL, "year") - TL)) %>%
    mutate(TR_diff = as.numeric(TR - lubridate::floor_date(TR, "year"))) %>%
    mutate(one_week_indicator = case_when(TL_diff <= 7 | TR_diff <= 7 ~ "1", 
                                        TRUE ~ "")) %>%
    mutate(two_week_indicator = case_when((TL_diff > 7 & TL_diff <= 14) | (TR_diff > 7 & TR_diff <= 14) ~ "2", 
                                        TRUE ~ "")) %>%
    mutate(three_week_indicator = case_when((TL_diff > 14 & TL_diff <= 21) | (TR_diff > 14 & TR_diff <= 21) ~ "3", 
                                        TRUE ~ "")) %>%
    arrange(desc(one_week_indicator), desc(two_week_indicator), desc(three_week_indicator))
  readr::write_csv(b, paste0("Analysis/output/multi-year_second-peak_investigation/", country_code, "_multi-year_obs_with_tfrac.csv"))

  ### added: similar thing from preprocessed data
  if(add_preprocess){
    pre <- sf_cases %>%
      sf::st_drop_geometry() %>%
      filter(lubridate::year(TL) != lubridate::year(TR)) %>%
      group_by(id) %>%
      dplyr::group_modify(function(.x,.y){
        # .x<-pre[1, c("TL", "TR", "suspected_cases")]#tmp
        .x <- .x %>%
          mutate( year_span = lubridate::year(TR) - lubridate::year(TL) + 1, 
                  year1 = ifelse(year_span >= 1, 1, NA), year2 = ifelse(year_span >= 2, 2, NA), 
                  year3 = ifelse(year_span >= 3, 3, NA), year4 = ifelse(year_span >= 4, 4, NA), 
                  year5 = ifelse(year_span >= 5, 5, NA)) %>% 
          tidyr::gather(key = "year_key", value = "year_idx", year1:year5, na.rm = TRUE) %>%
          mutate( tfrac = case_when( year_span == 1 ~ as.numeric(TR-TL)/lubridate::yday(lubridate::ceiling_date(TR, "year")-lubridate::days(1)), 
                                    year_idx < year_span & year_idx != 1 ~ 1,
                                    year_idx == 1 ~ as.numeric(lubridate::ceiling_date(TL, "year") - TL)/lubridate::yday(lubridate::ceiling_date(TL, "year")-lubridate::days(1)),
                                    year_idx == year_span ~ as.numeric(TR - lubridate::floor_date(TR, "year"))/lubridate::yday(lubridate::ceiling_date(TR, "year")-lubridate::days(1))
                                  ), 
                  preprocess_tfrac = paste(tfrac, collapse=", ")
                )
        .x <- .x[1, ]

        return(.x)
      }) %>%
      ungroup() %>%
      rename(preprocess_case = suspected_cases) %>%
      select(OC_UID, locationPeriod_id, TL, TR, preprocess_case, preprocess_tfrac)

    combined <- left_join(pre, b, by = c("OC_UID", "locationPeriod_id", "TL", "TR"))
    readr::write_csv(combined, paste0("Analysis/output/multi-year_second-peak_investigation/", country_code, "_prepro_staninput_obs_tfracs.csv"))
  }

  tfrac_dist <- paste(b$tfrac, collapse=", ")
  tfrac_dist <- as.numeric(unlist(stringr::str_split(tfrac_dist, ", ")))

  if(exists("tfrac_vector")){
    tfrac_vector <- rbind(tfrac_vector, tibble::tibble(iso = country_code, tfrac = tfrac_dist))
  }else{
    tfrac_vector <- tibble::tibble(iso = country_code, tfrac = tfrac_dist)
  }
}

### Plotting
pdf("Analysis/output/multi-year_second-peak_investigation/ALLCOUNTRY_multi-year_obs_tfrac_distribution.pdf")
for(country_code in country_list){
  tfrac_dist <- tfrac_vector[tfrac_vector$iso == country_code, ]$tfrac
  if(length(tfrac_dist) <= 1){next}
  hist(tfrac_dist, breaks = 52, main = country_code, col = "lightblue")
}
dev.off()
