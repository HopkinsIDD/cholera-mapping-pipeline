#' @include plot_cache_function.R
#' @include get_stan_output.R
#' @include get_genquant.R

#' @export
#' @name process_genquant
#' @title process_genquant
#' @description genquant processing function for the continent map 
#' @param config config file that contains the parameter information
#' @param country_cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return all output save in country_cache 
process_genquant <- function(config, country_cache, cholera_directory, ...) {
    # Get the config file 
    config_file<-yaml::read_yaml(paste0(cholera_directory, "/", config))

    # Get the genquant and others 
    get_stan_input(name="stan_input", config = config, cache = country_cache, cholera_directory = cholera_directory)
    get_genquant(name="genquant", config = config, cache = country_cache, cholera_directory = cholera_directory)
    get_output_shapefiles(name="output_shapefiles", config = config, cache = country_cache, cholera_directory = cholera_directory)

    # Get cases and rates summaries
    cases <- country_cache[["genquant"]]$summary(variables = "location_cases_output", 
                            c(posterior::default_summary_measures(),
                                posterior::default_convergence_measures()),
                            .cores = 4) %>% 
    mutate(id = str_extract(variable, "[0-9]+") %>% as.numeric(),
            location_period_id = country_cache[["stan_input"]]$fake_output_obs$locationPeriod_id[id],
            TL = country_cache[["stan_input"]]$fake_output_obs$TL[id])

    rates <- country_cache[["genquant"]]$summary(variables = "location_rates_output", 
                            c(posterior::default_summary_measures(),
                                posterior::default_convergence_measures()),
                            .cores = 4) %>% 
    mutate(id = str_extract(variable, "[0-9]+") %>% as.numeric(),
            location_period_id = country_cache[["stan_input"]]$fake_output_obs$locationPeriod_id[id],
            TL = country_cache[["stan_input"]]$fake_output_obs$TL[id])
    
    pop <- country_cache[["genquant"]]$summary(variables = "pop_loctimes_output",
                            c(posterior::default_summary_measures(),
                                posterior::default_convergence_measures()),
                            .cores = 4)%>% 
    mutate(id = str_extract(variable, "[0-9]+") %>% as.numeric(),
            location_period_id = country_cache[["stan_input"]]$fake_output_obs$locationPeriod_id[id],
            TL = country_cache[["stan_input"]]$fake_output_obs$TL[id])

    output_data <- country_cache[["output_shapefiles"]] %>% 
    inner_join(cases %>% dplyr::select(cases = mean, 
                                        cases_q5 = q5,
                                        cases_q95 = q95,
                                        location_period_id, TL)) %>% 
    inner_join(rates %>% dplyr::select(rate = mean, 
                                        rate_q5 = q5,
                                        rate_q95 = q95,
                                        location_period_id, TL)) %>%
    inner_join(pop %>% dplyr::select(pop = mean,
                                        location_period_id, TL))

    # Data over whole period
    # Sum of cases by location for whole period
    if(config_file$countries_name=="ZNZ"){
    country_cache[["output_shapefiles"]]<-country_cache[["output_shapefiles"]][str_detect(country_cache[["output_shapefiles"]]$location_period_id,"TZA-ADM0-TZA|TZA-ADM1-TZA.18_1|TZA-ADM1-TZA.19_1|TZA-ADM1-TZA.28_1|TZA-ADM1-TZA.29_1|TZA-ADM1-TZA.30_1|TZA-ADM2-TZA.19.1_1|TZA-ADM2-TZA.28.1_1|TZA-ADM2-TZA.28.2_1|TZA-ADM2-TZA.29.1_1|TZA-ADM2-TZA.29.2_1|TZA-ADM2-TZA.30.3_1|TZA-ADM2-TZA.30.4_1|TZA-ADM2-TZA.18.1_1|TZA-ADM2-TZA.30.2_1|TZA-ADM2-TZA.19.2_1|TZA-ADM2-TZA.18.2_1"),]
    }

    tot_cases <- country_cache[["genquant"]]$summary(variables = "location_total_cases_output", 
                                c(posterior::default_summary_measures(),
                                    posterior::default_convergence_measures()),
                                .cores = 4) %>% 
    mutate(id = str_extract(variable, "[0-9]+") %>% as.numeric(),
            location_period_id = country_cache[["output_shapefiles"]]$location_period_id[id],
            shapeName = country_cache[["output_shapefiles"]]$shapeName[id],
            admin_level = country_cache[["output_shapefiles"]]$admin_level[id])

    # Mean rates by location for whole period
    tot_rates <- country_cache[["genquant"]]$summary(variables = "location_total_rates_output", 
                                c(posterior::default_summary_measures(),
                                    posterior::default_convergence_measures()),
                                .cores = 4) %>% 
    mutate(id = str_extract(variable, "[0-9]+") %>% as.numeric(),
            location_period_id = country_cache[["output_shapefiles"]]$location_period_id[id],
            shapeName = country_cache[["output_shapefiles"]]$shapeName[id],
            admin_level = country_cache[["output_shapefiles"]]$admin_level[id])

    # Mean pop by location for whole period
    tot_pop <- country_cache[["genquant"]]$summary(variables = "pop_loctimes_output", 
                                c(posterior::default_summary_measures(),
                                    posterior::default_convergence_measures()),
                                .cores = 4) %>% 
    mutate(id = str_extract(variable, "[0-9]+") %>% as.numeric(),
            location_period_id = country_cache[["output_shapefiles"]]$location_period_id[id],
            shapeName = country_cache[["output_shapefiles"]]$shapeName[id],
            admin_level = country_cache[["output_shapefiles"]]$admin_level[id])

    # Save the case table 
    cases_summ <- output_data %>%
        st_drop_geometry() %>% 
        dplyr::select(shapeName, admin_level, TL, contains("cases")) %>% 
        mutate(TL = as.character(lubridate::year(TL))) %>% 
        bind_rows(tot_cases %>% 
                    rename(cases = mean,
                            cases_q5 = q5,
                            cases_q95 = q95) %>% 
                    dplyr::select(shapeName, admin_level, contains("cases")) %>% 
                    mutate(TL = "Total")) %>% 
        mutate(text = str_c(
            round(cases), " (",
            round(cases_q5), "-", 
            round(cases_q95), ")"
        )) %>% 
        dplyr::select(shapeName, admin_level, TL, text) %>% 
        pivot_wider(values_from = "text",
                    names_from = "TL") %>% 
        arrange(admin_level, shapeName)
    country_cache$case_table <- cases_summ

    # Save the rate table 
    rate_multiplier <- 1e4
    rates_summ <- output_data %>%
        st_drop_geometry() %>% 
        dplyr::select(shapeName, admin_level, TL, contains("rate")) %>% 
        mutate(TL = as.character(lubridate::year(TL))) %>% 
        bind_rows(tot_rates %>% 
                    rename(rate = mean,
                            rate_q5 = q5,
                            rate_q95 = q95) %>% 
                    dplyr::select(shapeName, admin_level, contains("rate")) %>% 
                    mutate(TL = "Total")) %>% 
        mutate(text = str_c(
        (rate * rate_multiplier) %>% formatC(digits = 1, big.mark = "'", format = "f"), " (",
        (rate_q5 * rate_multiplier) %>% formatC(digits = 1, big.mark = "'", format = "f"), "-", 
        (rate_q95 * rate_multiplier) %>% formatC(digits = 1, big.mark = "'", format = "f"), ")"
        )) %>% 
        dplyr::select(shapeName, admin_level, TL, text) %>% 
        pivot_wider(values_from = "text",
                    names_from = "TL") %>% 
        arrange(admin_level, shapeName)
    country_cache$rate_table <- rates_summ

    # Save the pop table 
    pop_summ <- output_data %>%
        st_drop_geometry() %>% 
        dplyr::select(shapeName, admin_level, TL, contains("pop")) %>% 
        mutate(TL = as.character(lubridate::year(TL))) %>% 
        bind_rows(tot_pop %>% 
                    rename(pop = mean) %>% 
                    dplyr::select(shapeName, admin_level, contains("pop")) %>% 
                    mutate(TL = "Total")) %>% 
        mutate(text = str_c(
        round(pop)
        )) %>% 
        dplyr::select(shapeName, admin_level, TL, text) %>% 
        subset(is.na(shapeName)==F) %>%
        pivot_wider(values_from = "text",
                    names_from = "TL") %>% 
        arrange(admin_level, shapeName)
    country_cache$pop_table <- pop_summ
}

#' @export
#' @name stitch_genquant
#' @title stitch_genquant
#' @description genquant stitching function for the continent map
#' @param country_cache config file that contains the parameter information
#' @param final_cache the cached environment
#' @param admin_level  the directory of cholera mapping pipeline folder
#' @return all output save in cache 
stitch_genquant <- function(country_cache, final_cache, admin_level = "ADM0") {

    if(!"case_table" %in% names(final_cache)){
        final_cache$case_table <- country_cache$case_table[country_cache$case_table$admin_level == admin_level, ]
    }else{
        final_cache$case_table <- rbind(final_cache$case_table, country_cache$case_table[country_cache$case_table$admin_level == admin_level, ])
    }

    if(!"rate_table" %in% names(final_cache)){
        final_cache$rate_table <- country_cache$rate_table[country_cache$rate_table$admin_level == admin_level, ]
    }else{
        final_cache$rate_table <- rbind(final_cache$rate_table, country_cache$rate_table[country_cache$rate_table$admin_level == admin_level, ])
    }

    if(!"pop_table" %in% names(final_cache)){
        final_cache$pop_table <- country_cache$pop_table[country_cache$pop_table$admin_level == admin_level, ]
    }else{
        final_cache$pop_table <- rbind(final_cache$pop_table, country_cache$pop_table[country_cache$pop_table$admin_level == admin_level, ])
    }

}
