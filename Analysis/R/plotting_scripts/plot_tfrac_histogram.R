#=============================================================================================
# This is the script used to draw histograms of tfrac and sfrac distributions 
# 7/26/2022
# updated on 10/7/2022
# updated on 10/26/2022
# Note on 1/10/2023: no observation for GMB from 2016 to 2020
#           updates: how to add x axis while the free x scale is off 
#=============================================================================================

# Load the packages
library(dplyr)
library(ggplot2)

# Where the data folder is
data_dir <- "/home/.../data"

# Set parameters
start_year <- 2016
filter_multi_year <- TRUE

# Find and load the model output 
fns <- list.files(data_dir, full.names = FALSE, recursive=FALSE)
country_list <- unique(stringr::str_extract(fns, "^[A-Z]{3}"))
# country_list <- country_list[country_list != "ZAF" & !is.na(country_list)] #delete ZAF
country_list <- c(country_list, "Tanzania") #add TZA_mainland and remove NA
country_list <- country_list[!is.na(country_list)]
if(start_year == 2016){country_list <- country_list[country_list != "GMB"]}

# country_list <- c("COD", "ETH", "NGA")
fns_filtered <- fns[grepl("covar.rdata$", fns) | grepl("stan_input.rdata$", fns) | grepl("preprocess.rdata$", fns)]
fns_filtered <- fns_filtered[grepl(as.character(start_year), fns_filtered)]

# Loop through all the files
tfrac_table <- tibble::tibble(iso = as.character(),  admin_level = as.numeric(), tfrac = as.numeric())
# sfrac_table <- tibble::tibble(country = as.character(), sfrac = as.numeric())

for(country_code in country_list){
  load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)], sep = "/")[1])
  load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)], sep = "/")[2])
  load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)], sep = "/")[3])

  tfrac_tmp <- tibble::tibble(
    location_period_id = stan_input$stan_data$u_loctime[stan_input$stan_data$map_obs_loctime_loc] %>% 
      purrr::map_chr(~ covar_cube_output$location_periods_dict$location_period_id[
        covar_cube_output$location_periods_dict$loctime_id == .
      ] %>% first()),
    tfrac = stan_input$stan_data$tfrac
  ) 
  tfrac_tmp$location_name <- tfrac_tmp$location_period_id %>% 
    purrr::map_chr(~ sf_cases$location_name[sf_cases$locationPeriod_id == .] %>% 
    first())
  
  #if filter out the multi-year obs
  if(filter_multi_year){
    single_year_obs_idx <- which(lubridate::year(stan_input$sf_cases_resized$TL) == lubridate::year(stan_input$sf_cases_resized$TR))
    single_year_tfr_idx <- which(stan_input$stan_data$map_obs_loctime_obs %in% single_year_obs_idx)
    tfrac_tmp <- tfrac_tmp[single_year_tfr_idx, ]
  }
  tfrac_tmp <- tfrac_tmp %>% 
    mutate(admin_level = stringr::str_count(location_name, "::") - 1, iso = country_code) %>%
    select(iso, admin_level, tfrac)                                          
  
  tfrac_table <- rbind(tfrac_table, tfrac_tmp)
  rm(tfrac_tmp, stan_input, covar_cube_output, sf_cases)
  
  # sfrac_tmp <- tibble::tibble(sfrac = initial_values_data$stan_data$pop_weight, country = country_code)
  # sfrac_table <- rbind(sfrac_table, sfrac_tmp)
  
}

### Add empty admin level
for(country_code in country_list){
  empty_admin <- (0:4)[!(0:4) %in% unique(tfrac_table[tfrac_table$iso == country_code, ]$admin_level)]
  if(length(empty_admin) >= 1){tfrac_table <- tfrac_table %>% add_row(iso = rep(country_code, length(empty_admin)), 
                                                                      admin_level = empty_admin, 
                                                                      tfrac = rep(NA, length(empty_admin)))}
  if(country_code == "TZA" | country_code == "Tanzania"){
    empty_admin <- (0:9)[!(0:9) %in% unique(tfrac_table[tfrac_table$iso == country_code, ]$admin_level)]
    if(length(empty_admin) >= 1){tfrac_table <- tfrac_table %>% add_row(iso = rep(country_code, length(empty_admin)), 
                                                                        admin_level = empty_admin, 
                                                                        tfrac = rep(NA, length(empty_admin)))}
  }
}

### Plotting 
by_country <- tfrac_table %>%
  filter(!is.na(tfrac)) %>% 
  ggplot(aes(x = tfrac)) + 
  geom_histogram(fill = "#404080", alpha = 0.6) + 
  facet_wrap( ~ iso, scales = "free", ncol = 4) + 
  scale_x_continuous(limits=c(0, 1)) + 
  theme_minimal()
by_admin <- tfrac_table %>%
  ggplot(aes(x = tfrac)) + 
  geom_histogram(fill = "#404080", alpha = 0.6) + 
  facet_wrap(iso ~ admin_level, scales = "free", ncol = 5) + 
  scale_x_continuous(limits=c(0, 1)) + 
  theme_minimal()
fig_height <- length(country_list) 

pdf(paste("/home/kaiyuezou/mapping_pipeline", paste0(as.character(start_year), "-", as.character(start_year+4), "_", ifelse(filter_multi_year, "no-multi-year_histograms_by-country.pdf", "multi-year-included_histograms_by-country.pdf")), 
  sep = "/"), height = fig_height, width = 10)
by_country
dev.off()

pdf(paste("/home/kaiyuezou/mapping_pipeline", paste0(as.character(start_year), "-", as.character(start_year+4), "_", ifelse(filter_multi_year, "no-multi-year_histograms_by-admin.pdf", "multi-year-included_histograms_by-admin.pdf")), 
  sep = "/"), height = fig_height*3, width = 10)
by_admin
dev.off()


