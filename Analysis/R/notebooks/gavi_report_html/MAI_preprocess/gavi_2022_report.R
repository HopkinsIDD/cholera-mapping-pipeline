# This is a script of new functions to standardize model output

# Packages needed
library(stringr)
library(dplyr)
library(magrittr)
library(purrr)
library(readr)
library(ggplot2)
library(taxdat)
library(sf)
library(raster)
library(stars)

# Get the pipeline directory as a parameter
cholera_directory <- "/home/kaiyuezou/mapping_pipeline/9_23_dev/cholera-mapping-pipeline" #this should be given

# Get the summary csv as a parameter
summary_director <- "/home/kaiyuezou/mapping_pipeline/9_23_dev/cholera-mapping-pipeline/Analysis/R/" #now it's sharing the same directory as this script

# List the new countries for model output -- this is from country status
country_status <- read_csv("/home/kaiyuezou/mapping_pipeline/9_23_dev/cholera-mapping-pipeline/Analysis/R/gavi_report_country_status.csv")
Dec_2021_list <- unique(country_status[country_status$report_old_new_output == 'new'
                                       & country_status$rerun_approved == 'TRUE', ]$approved_country)
Dec_2021_list <- c(Dec_2021_list, c("RWA", "SEN")) #just change two countries for now
Dec_2021_list <- c(Dec_2021_list, c("NGA", "NER", "COD", "COG", "ETH", "AGO", "GIN", "ZNZ"))
Dec_2021_list <- c("MRT") #for the last two countries
Dec_2021_list <- c("TZA", "ZNZ") # TZA and ZNZ fix
# Dec_2021_list <- c("SDN", "ZNZ", "ZWE") #for final warp-up 

# From a GH repo to a certain local directory -- self-defined
config_data_finder <- c("https://github.com/HopkinsIDD/cholera-configs/tree/master/Dec_2021_runs" = "Analysis/configs/Dec_2021_runs", 
                        "https://github.com/HopkinsIDD/cholera-mapping-output/tree/2021_december_runs" = "Analysis/data/true_2021_Dec_runs_output_for_push/cholera-mapping-output", 
                        "https://github.com/HopkinsIDD/cholera-configs/tree/master/2015_2019_full_base" = "Analysis/configs/2015_2019_full_base", 
                        "https://github.com/HopkinsIDD/cholera-mapping-output/tree/gavi_submission" = "Analysis/data/old_runs") 

# Use a function to deal with the model output
lapply(Dec_2021_list, function(country_code){
    ## For the aggregated multi-year run output
    if(country_status[country_status$approved_country == country_code, ]$single_year_multiple_year == "single"){
        case_df <- readr::read_csv(paste0(summary_director, 'stan_aggregated_table_', country_code, '.csv'))
        case_df <- case_df[, -1]
        if(country_code == "SDN"){
            case_df <- case_df[-(1001:2000), ]
        }
        all_grid_sum <- apply(case_df, 1, sum, na.rm = TRUE)
        # matrix_form <- matrix(all_grid_sum, nrow = 1000)
        year_length <- ifelse(country_code == "SDN", 4, 5) #only SDN uses 0 for one year and totally drops another year without assuming it to be 0
        # aross_year_mean_sum <- apply(matrix_form, 1, function(each_year){sum(each_year, na.rm = TRUE) / year_length})
        
        case_distribution <- all_grid_sum
        if( (length(case_distribution)/1000) != 5 ){ #the approved years have already been included in the dataset for the single runs
            adding_row <- (year_length - (length(case_distribution)/1000)) * 1000
            case_distribution <- c(case_distribution, rep(0, adding_row))
            # case_distribution <- rbind(case_distribution, matrix(0, adding_row, 1))
        }
        TZA_case_distribution <- case_distribution

        case_mean <- mean(case_distribution, na.rm = TRUE)
        case_q_05 <- quantile(case_distribution, probs = c(0.05, 0.95), na.rm = TRUE)[1]
        case_q_95 <- quantile(case_distribution, probs = c(0.05, 0.95), na.rm = TRUE)[2]

        pop_2017 <- as.numeric(tidyr::world_bank_pop[tidyr::world_bank_pop$country == country_code & 
                                                     tidyr::world_bank_pop$indicator == "SP.POP.TOTL", c("2017")])
        if(country_code == "TZA"){
            pop_2017 <- pop_2017 -  683687
        }
        # pop_used_for_model <- pop_2017                    
        rate_mean <- case_mean / pop_2017
        rate_q_05 <- case_q_05 / pop_2017
        rate_q_95 <- case_q_95 / pop_2017
        
    }else{
        ## Get the model output ready -- this can be applied only when all the output files are under the "data" directory
        config_repo <- country_status[country_status$approved_country == country_code, ]$github_config_repo
        output_repo <- country_status[country_status$approved_country == country_code, ]$github_output_repo
        local_config_dir <- paste0(cholera_directory, '/', config_data_finder[config_repo])
        local_output_dir <- paste0(cholera_directory, '/', config_data_finder[output_repo])

        if(country_code == "GIN"){ #using the new model output for GIN
            config_repo <- country_status[country_status$approved_country == "BEN", ]$github_config_repo
            output_repo <- country_status[country_status$approved_country == "BEN", ]$github_output_repo
            local_config_dir <- paste0(cholera_directory, '/', config_data_finder[config_repo])
            local_output_dir <- paste0(cholera_directory, '/', config_data_finder[output_repo])
        }

        if(country_status[country_status$approved_country == country_code, ]$report_old_new_output == "old" & country_code != "GIN"){
            config_fn <- paste0(local_config_dir, '/', country_code, ".yml")
        }else{
            config_fn <- paste0(local_config_dir, '/', country_code, "_2015_2019/config_", country_code, "_2015_2019.yml")
        }
        
        config <- yaml::read_yaml(config_fn)
        file_names <- taxdat::get_filenames(config, cholera_directory)
        
        file_names <- unlist(lapply(names(file_names), function(file_name){
                                    gsub("^.*?/Analysis/data", local_output_dir, file_names[file_name])
                                    }))

        sf_cases <- NULL
        model.rand <- NULL
        stan_output <- NULL
        cases_chains <- NULL
        try({
            sf_cases <- taxdat::read_file_of_type(file_names["data"], "sf_cases")
            
            model.rand <- taxdat::read_file_of_type(file_names["stan_output"], "model.rand")
            niter_per_chain <- dim(MCMCvis::MCMCchains(model.rand, params='lp__', chain_num=1))[1]
            nchain <- dim(MCMCvis::MCMCchains(model.rand, params='lp__'))[1] / niter_per_chain
            stan_output <- lapply(rstan::extract(model.rand), function(x){array(x,c(niter_per_chain, nchain, dim(x)[-1]))})
            cases_chains <- apply(stan_output$grid_cases, c(3,2), mean)
        })

        if (is.null(stan_output)) {
            cat(file_names[['stan_output']])
            stop("Stan output not found")
        }
        
        ## Get the mean, 2.5%, and 97.5% estimates for annual cases
        year_range <- country_status[country_status$approved_country == country_code, ]$approved_years
        if(length(grep("^[0-9]{4}-[0-9]{4}$", year_range)) != 0){
            year_start <- as.numeric(gsub("-[0-9]{4}$", '', year_range))
            year_end   <- as.numeric(gsub("^[0-9]{4}-", '', year_range))
            year_length_real <- year_end - year_start + 1
            stan_output_grid_cases <- stan_output$grid_cases
        }else{
            year_list <- as.numeric(unlist(stringr::str_split(year_range, ",")))
            year_length_real <- length(year_list)
            year_index<- match(year_list, 2015:2019)
            num_per_year <- dim(stan_output$grid_cases)[3]/5

            third_dim_index <- c()
            third_dim_index <- unlist(lapply(year_index, function(index){third_dim_index <- c(third_dim_index, (1:num_per_year) + num_per_year*(index-1))}))
            stan_output_grid_cases <- stan_output$grid_cases[, , third_dim_index]
        }
        
        year_length <- 5 #all multiple year runs assume 0 for dropped years
        stan_output_grid_cases <- apply(stan_output_grid_cases, c(1, 3), mean) #eliminate the middle dimension 
        num_per_year <- dim(stan_output$grid_cases)[3]/5 
        if(year_length_real != 5){
            zero_matrix <- matrix(0, nrow(stan_output_grid_cases), (5-year_length_real)*num_per_year )
            stan_output_grid_cases <- cbind(stan_output_grid_cases, zero_matrix)
        }
        # stan_output_grid_cases <- matrix(stan_output_grid_cases, ncol = num_per_year)
        stan_output_grid_cases <- rbind(stan_output_grid_cases[, 1:num_per_year], 
                                        stan_output_grid_cases[, (1:num_per_year) + 1*num_per_year], 
                                        stan_output_grid_cases[, (1:num_per_year) + 2*num_per_year], 
                                        stan_output_grid_cases[, (1:num_per_year) + 3*num_per_year], 
                                        stan_output_grid_cases[, (1:num_per_year) + 4*num_per_year] )
        ZNZ_grid_cases <- stan_output_grid_cases

        case_distribution <- apply(stan_output_grid_cases, 1, sum)
        # annual_case <- apply(stan_output_grid_cases, c(1, 2), sum) / year_length
        # chain_mean_annual_case <- apply(annual_case, c(1), mean)
        case_mean <- mean(case_distribution)
        case_q_05 <- quantile(case_distribution, probs = c(0.05, 0.95))[1]
        case_q_95 <- quantile(case_distribution, probs = c(0.05, 0.95))[2]

        ## Calculate incidence rates
        covar_data_filename <- file_names["covar"]
        covar_cube_output <- taxdat::read_file_of_type(covar_data_filename, "covar_cube_output")
        covar_cube <- covar_cube_output$covar_cube
        sf_grid <- covar_cube_output$sf_grid
        pop_layer <- covar_cube[,,1, drop = F]
        mean_pop <- mean(apply(pop_layer, c(2), sum))
        # pop_used_for_model <- mean_pop
        
        pop_2017 <- as.numeric(tidyr::world_bank_pop[tidyr::world_bank_pop$country == country_code & 
                                                     tidyr::world_bank_pop$indicator == "SP.POP.TOTL", c("2017")])   
        if(country_code == "ZNZ"){
            pop_2017 <- 683687
        }                 
        rate_mean <- case_mean / pop_2017
        rate_q_05 <- case_q_05 / pop_2017
        rate_q_95 <- case_q_95 / pop_2017
        
    }
    

    
    ## Insert the data into the summary csv and save it
    summary_table <- readr::read_csv(paste0(summary_director, 'gavi_summary.csv'))

    if(!country_code %in% unique(summary_table$country)){
        summary_table <- summary_table %>%
            add_row(country = country_code)
    }

    summary_table[summary_table$country ==  country_code, ]$current_rates_mean <- rate_mean
    summary_table[summary_table$country ==  country_code, ]$current_cases_mean <- case_mean
    summary_table[summary_table$country ==  country_code, ]$current_rates_quantile_low <- rate_q_05
    summary_table[summary_table$country ==  country_code, ]$current_cases_quantile_low <- case_q_05
    summary_table[summary_table$country ==  country_code, ]$current_rates_quantile_high <- rate_q_95
    summary_table[summary_table$country ==  country_code, ]$current_cases_quantile_high <- case_q_95

    write.csv(summary_table, paste0(summary_director, 'gavi_summary.csv'), row.names = FALSE)

    # covar_pop <- readr::read_csv(paste0(summary_director, 'covar_pop.csv'))
    # covar_pop <- covar_pop %>%
    #     add_row(country = country_code, covar_pop = pop_used_for_model)
    # write.csv(covar_pop, paste0(summary_director, 'covar_pop.csv'), row.names = FALSE)
})



####### GIN incidence rate and number of cases estimates 
library(raster)

lambda <- raster::stack('gavi_vimc_cholera/input_data/incidence/afro_2010-2016_lambda_5k.tif')
#make lambda smaller
#lambda <- raster::subset(lambda, c(1, 2), drop = FALSE)

gin <- rgeoboundaries::gb_adm0("GIN")
lambda_gin <- raster::crop(lambda, gin)
lambda_gin <- raster::mask(lambda_gin, gin)

pop <- raster::raster("gavi_vimc_cholera/input_data/worldpop/ppp_2020_5km_Aggregated.tif")
pop_gin <- raster::crop(pop, gin)
pop_gin <- raster::mask(pop_gin, gin)

case_gin <- lambda_gin * pop_gin

total_pop <- sum(raster::getValues(pop_gin), na.rm=TRUE)
total_case <- apply(raster::getValues(case_gin), 2, sum, na.rm=TRUE)
total_inci <- total_case / total_pop

case_mean <- mean(total_case)
case_q_05 <- quantile(total_case, probs = c(0.05, 0.95))[1]
case_q_95 <- quantile(total_case, probs = c(0.05, 0.95))[2]

rate_mean <- mean(total_inci)
rate_q_05 <- quantile(total_inci, probs = c(0.05, 0.95))[1]
rate_q_95 <- quantile(total_inci, probs = c(0.05, 0.95))[2]


case_mean <- 2813.755
case_q_05 <- 2234.801
case_q_95 <- 3144.157

rate_mean <- 0.0002296059
rate_q_05 <- 0.0001823626
rate_q_95 <- 0.0002565671

summary_table <- readr::read_csv(paste0(summary_director, 'gavi_summary.csv'))
summary_table[summary_table$country == "GIN", ]$lancet_rates_mean <- rate_mean
summary_table[summary_table$country == "GIN", ]$lancet_cases_mean <- case_mean
summary_table[summary_table$country == "GIN", ]$lancet_rates_quantile_low <- rate_q_05
summary_table[summary_table$country == "GIN", ]$lancet_cases_quantile_low <- case_q_05
summary_table[summary_table$country == "GIN", ]$lancet_rates_quantile_high <- rate_q_95
summary_table[summary_table$country == "GIN", ]$lancet_cases_quantile_high <- case_q_95



####### TZA and ZNZ fix
country_code <- "TZA"
country_code <- "ZNZ"

TZA_case_distribution <- matrix(TZA_case_distribution, nrow = 5000, ncol = 1)
TZA_all <- cbind(TZA_case_distribution, ZNZ_grid_cases)
TZA_all <- apply(TZA_all, 1, sum)

case_mean <- mean(TZA_all)
case_q_05 <- quantile(TZA_all, probs = c(0.05, 0.95))[1]
case_q_95 <- quantile(TZA_all, probs = c(0.05, 0.95))[2]

pop_2017 <- as.numeric(tidyr::world_bank_pop[tidyr::world_bank_pop$country == "TZA" & 
                                             tidyr::world_bank_pop$indicator == "SP.POP.TOTL", c("2017")])   
               
rate_mean <- case_mean / pop_2017
rate_q_05 <- case_q_05 / pop_2017
rate_q_95 <- case_q_95 / pop_2017

# summary_table <- readr::read_csv(paste0(summary_director, 'gavi_summary.csv'))

if(!"TZA_M" %in% unique(summary_table$country)){
    summary_table <- summary_table %>%
        add_row(country = "TZA_M",  
                current_rates_mean = summary_table[summary_table$country ==  "TZA", ]$current_rates_mean, 
                current_cases_mean = summary_table[summary_table$country ==  "TZA", ]$current_cases_mean, 
                current_rates_quantile_low = summary_table[summary_table$country ==  "TZA", ]$current_rates_quantile_low, 
                current_cases_quantile_low = summary_table[summary_table$country ==  "TZA", ]$current_cases_quantile_low, 
                current_rates_quantile_high = summary_table[summary_table$country ==  "TZA", ]$current_rates_quantile_high, 
                current_cases_quantile_high = summary_table[summary_table$country ==  "TZA", ]$current_cases_quantile_high )
    
}

summary_table[summary_table$country == "TZA", ]$current_rates_mean <- rate_mean
summary_table[summary_table$country == "TZA", ]$current_cases_mean <- case_mean
summary_table[summary_table$country == "TZA", ]$current_rates_quantile_low <- rate_q_05
summary_table[summary_table$country == "TZA", ]$current_cases_quantile_low <- case_q_05
summary_table[summary_table$country == "TZA", ]$current_rates_quantile_high <- rate_q_95
summary_table[summary_table$country == "TZA", ]$current_cases_quantile_high <- case_q_95

write.csv(summary_table, paste0(summary_director, 'gavi_summary.csv'), row.names = FALSE)
