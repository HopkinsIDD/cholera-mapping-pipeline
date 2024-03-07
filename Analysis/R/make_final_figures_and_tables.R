# This script combines postprocessed outputs and makes final figures


# Preamble ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
library(cmdstanr)
library(optparse)
library(rmapshaper)
library(taxdat)
library(cowplot)

# User-supplied options
opt_list <- list(
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./Analysis/output/processed_outputs/", 
              help = "Output directory with postprocessed objects"),
  make_option(opt_str = c("-x", "--prefix_p1"), type = "character",
              default = "postprocessing_test_2011_2015", 
              help = "Prefix of output files for first period"),
  make_option(opt_str = c("-y", "--prefix_p2"), type = "character",
              default = "postprocessing_test_2016_2020", 
              help = "Prefix of output files for second period"),
  make_option(opt_str = c("-p", "--out_prefix"), type = "character",
              default = "postprocessing", 
              help = "Prefix for output figures and tables"),
  make_option(opt_str = c("-d", "--out_dir"), type = "character",
              default = "./Analysis/output/figures",
              help = "Output directory for figures"),
  make_option(opt_str = c("-f", "--bundle_filename"), type = "character",
              default = "./Analysis/output/data_bundle_for_figures_test.rdata", 
              help = "Data bundle to avoid re-processing"),
  make_option(opt_str = c("-r", "--redo"), type = "logical",
              default = FALSE, 
              help = "Redo re-processing")
)

opt <- parse_args(OptionParser(option_list = opt_list))


prefix_list <- list(
  "2011-2015" = opt$prefix_p1,
  "2016-2020" = opt$prefix_p2
)

# Functions ---------------------------------------------------------------

#' combine_period_output
#'
#' @param prefix_list 
#' @param output_name 
#' @param output_dir 
#'
#' @return
#' @export
#'
#' @examples
combine_period_output <- function(prefix_list,
                                  output_name,
                                  output_dir) {
  
  res <- map_df(1:length(prefix_list), function(j) {
    readRDS(str_glue("{output_dir}/{prefix_list[j]}_{output_name}.rds")) %>% 
      ungroup() %>% 
      mutate(period = names(prefix_list)[j])
  })
  
  res
}

compute_rate_changes <- function(df) {
  df %>% 
    st_drop_geometry() %>% 
    select(location_period_id, mean, period, country) %>% 
    pivot_wider(values_from = "mean",
                names_from = "period") %>% 
    mutate(rate_ratio = `2016-2020`/`2011-2015`,
           log10_rate_ratio = log10(rate_ratio),
           rate_diff = `2016-2020` - `2011-2015`)
}

# Compute change statistics (could package into function)
merge_ratio_draws <- function(df1, 
                              df2,
                              unit_col = "location_period_id") {
  
  
  if(!(unit_col %in% colnames(df1) & unit_col %in% colnames(df2))) {
    stop("Unit column name not in both dfs: ", unit_col)
  }
  
  u_units <- intersect(unique(df1[[unit_col]]), unique(df2[[unit_col]]))
  n_variables <- length(u_units)
  u_draws <- unique(df1$.draw)
  n_draws <- length(u_draws)
  ratios <- array(NA, dim = c(n_variables, n_draws, n_draws))
  
  # Compute ratio samples
  for (i in 1:n_variables) {
    ind2 <- df2[[unit_col]] == u_units[i]
    ind <- df1[[unit_col]] == u_units[i]
    for (j in 1:n_draws) {
      ratios[i, j, ] <- df2$value[ind2 & df2$.draw == u_draws[j]]/
        df1$value[ind]
    }
    cat("Done", i, "/", n_variables, "\n")
  }
  
  ratio_stats <- array(NA, dim = c(n_variables, 3))
  colnames(ratio_stats) <- c("mean", "q2.5", "q97.5")
  
  # Compute ratio stats
  for (i in 1:n_variables) {
    ratio_stats[i, "mean"] <- mean(ratios[i, , ])
    ratio_stats[i, "q2.5"] <- quantile(ratios[i, , ], 0.025)
    ratio_stats[i, "q97.5"] <- quantile(ratios[i, , ], 0.975)
  }
  
  ratio_stats <- as_tibble(ratio_stats)
  ratio_stats[[unit_col]] <- u_units
  
  ratio_stats
}


make_adm_case_table <- function(mai_adm_cases,
                                admin_levels = "ADM0") {
  
  dat <- mai_adm_cases %>% 
    filter(admin_level %in% admin_level) %>% 
    mutate(across(c("mean", "q2.5", "q97.5"), function(x) {
      om <- pmax(1, round(log10(x)) - 2)
      formatC(round(x/10^om)*10^om, format = "f", digits = 0, big.mark = ",")
    })) %>% 
    mutate(txt = str_c(mean, " (", q2.5, "-", q97.5, ")")) %>% 
    select(admin_level, country, shapeName, period, txt) %>% 
    pivot_wider(values_from = c("txt"),
                names_from = "period")
  
  if (length(admin_levels) > 1) {
    dat2 <- dat %>% 
      filter(admin_level %in% admin_levels) %>% 
      arrange(admin_level, country, shapeName)
    
    dat2 %>% 
      dplyr::select(-admin_level)
  } else {
    dat2 <- dat %>% 
      filter(admin_level == admin_levels) %>% 
      arrange(country, shapeName)
    
    dat2 %>% 
      {
        x <-.
        if (admin_levels == "ADM0")  {
          dplyr::select(x, -country, -admin_level) 
        } else {
          dplyr::select(x, -admin_level) 
        }
      }
  }
}

save_table_to_docx <- function(tbl, output_path) {
  tbl  %>% 
    flextable::as_flextable( max_row = Inf) %>%
    flextable::set_table_properties(width = 1, layout = "autofit") %>%
    flextable::save_as_docx(path = output_path)
}


get_mean_rate <- function(mai_adm_all,
                          this_unit,
                          this_period,
                          by_region = FALSE) {
  
  # Get the mean rate of the country
  if (!by_region) {
    mean_rate <- mai_adm_all %>% 
      filter(admin_level == "ADM0",
             country == this_unit,
             period == this_period) %>% 
      pull(mean)
  } else {
    mean_rate <- mai_adm_all %>% 
      st_drop_geometry() %>% 
      filter(admin_level == "ADM0",
             AFRO_region == this_unit,
             period == this_period) %>% 
      summarise(mean = weighted.mean(mean, pop)) %>% 
      pull(mean)
  }
  
  mean_rate
}

compute_irr <- function(target_cells_sf, 
                        grid_cases,
                        grid_rates,
                        mean_rate) {
  
  # Get population of these cells
  target_cells <- grid_cases %>% 
    st_drop_geometry() %>% 
    select(rid, x, y, cases = mean) %>% 
    inner_join(
      grid_rates %>% 
        st_drop_geometry() %>% 
        select(rid, x, y, rate = mean),
      by = c("rid", "x", "y")
    ) %>% 
    mutate(pop = cases/rate) %>% 
    inner_join(target_cells_sf %>% 
                 st_drop_geometry(),
               by = c("rid", "x", "y"))
  
  
  # Compute expected cases
  stats <- target_cells %>% 
    mutate(mean_rate = mean_rate,
           expected_cases = pop * mean_rate) %>% 
    summarise(observed = sum(mean),
              expected = sum(expected_cases),
              ratio = observed/expected)
  
  stats
}


#' Title
#'
#' @param target 
#' @param dist_vec 
#' @param mai_adm 
#' @param grid_cases 
#' @param afr_sf 
#' @param dist_definition 
#'
#' @return
#' @export
#'
#' @examples
irr_dist <- function(target,
                     dist_vec = seq(5e4, 50e4, by = 5e4),
                     mai_adm,
                     mai_adm_all,
                     grid_cases,
                     afr_sf,
                     dist_definition = "within",
                     by_region = TRUE) {
  
  if (by_region) {
    u_units <- unique(mai_adm$AFRO_region)
  } else {
    u_units <- unique(mai_adm$country)
  }
  
  res <- map_df(
    u_units, 
    function(this_unit) {
      map_df(
        unique(mai_adm$period), 
        function(this_period) {
          
          cat("Unit", this_unit, "period", this_period, "\n")
          
          # Get all cells for which we have at least case
          case_cells <- grid_cases %>% 
            filter(mean > 1, period == this_period)
          
          if (by_region) {
            in_unit <- st_intersects(case_cells, 
                                     afr_sf %>% 
                                       filter(AFRO_region== this_unit),
                                     sparse = FALSE) %>% 
              rowSums() %>% 
              {.>0}
          } else {
            in_unit <- st_intersects(case_cells, 
                                     afr_sf %>% 
                                       filter(country == this_unit),
                                     sparse = FALSE) %>% 
              rowSums() %>% 
              {.>0}
          }
          
          
          if (sum(in_unit) == 0) {
            res <- tibble(observed = NA,
                          expected = NA,
                          ratio = NA,
                          dist_thresh = NA)
          } else {
            
            this_case_cells <- case_cells[in_unit, ]
            
            # Make buffer
            centroids <- st_centroid(this_case_cells)
            centroids <- st_transform(centroids, st_crs(target))
            
            # Define whether it is within distance or bands
            if (dist_definition == "within") {
              dist_vec2 <- dist_vec
              len <- length(dist_vec2)
              mean_dist <- dist_vec
            } else {
              dist_vec2 <- cbind(c(0, dist_vec[-length(dist_vec)]), dist_vec)
              len <- nrow(dist_vec2)
              mean_dist <- apply(dist_vec2, 1, mean)
            }
            
            # Filter all grid cells within distance
            res <- map_df(
              seq_len(len), 
              function(x) {
                
                
                if (dist_definition == "within") {
                  # Text to save
                  dist_txt <- as.character(dist_vec[x]/1e3)
                  
                  within_dist <-  st_is_within_distance(centroids, target,
                                                        dist = dist_vec[x],
                                                        sparse = F) %>% 
                    rowSums() %>% 
                    {.>0} 
                  
                  target_cells_sf <- centroids[within_dist, ]
                  
                } else {
                  
                  # Text to save
                  dist_txt <- str_c(formatC(dist_vec2[x, ]/1e3, format = "f", digits = 0, big.mark = ","), collapse = "-")
                  
                  within_dist <- st_is_within_distance(centroids, target,
                                                       dist = dist_vec2[x, 2],
                                                       sparse = F) %>% 
                    rowSums() %>% 
                    {.>0}
                  
                  beyond_dist <- st_is_within_distance(centroids, target,
                                                       dist = dist_vec2[x, 1] + 1,
                                                       sparse = F) %>% 
                    rowSums() %>% 
                    {. > 0} %>% 
                    {!.}
                  
                  target_cells_sf <- centroids[within_dist & beyond_dist, ]
                  
                }
                
                if (sum(within_dist) > 0) {
                  
                  mean_rate <- get_mean_rate(mai_adm_all = mai_adm_all,
                                             this_unit = this_unit,
                                             this_period = this_period,
                                             by_region = by_region)
                  
                  
                  stats <- compute_irr(target_cells_sf = target_cells_sf, 
                                       grid_cases = grid_cases,
                                       grid_rates = grid_rates,
                                       mean_rate = mean_rate)
                  
                } else {
                  stats <- tibble(observed = NA,
                                  expected = NA,
                                  ratio = NA)
                }
                
                
                stats %>% 
                  mutate(dist = dist_txt,
                         dist_definition = dist_definition,
                         mean_dist = mean_dist[x])
              })
          }
          
          res %>% 
            mutate(unit = this_unit,
                   period = this_period)
        })
    })
  
  if (by_region) {
    res <- res %>% rename(AFRO_region = unit)
  } else {
    res <- res %>% rename(country = unit)
  }
  
  res %>% 
    rowwise() %>% 
    mutate(lo = exp(log(ratio) - 1.96 * sqrt(1/observed + 1/expected)),
           hi = exp(log(ratio) + 1.96 * sqrt(1/observed + 1/expected))) %>% 
    ungroup() 
}


unpack_pop_at_risk <- function(df) {
  risk_cat_map <- get_risk_cat_dict()
  names(risk_cat_map) <- janitor::make_clean_names(risk_cat_map) %>% 
    str_remove("x")
  
  
  df %>% 
    mutate(risk_cat = str_extract(variable, str_c(rev(names(risk_cat_map)), collapse = "|")),
           risk_cat = risk_cat_map[risk_cat] %>% factor(levels = risk_cat_map),
           admin_level = str_c("ADM", str_extract(variable, "(?<=adm)[0-9]+"))
    )
}

parse_AFRO_region <- function(df) {
  
  if (!("AFRO_region" %in% colnames(df))) {
    stop("AFRO_region needs to be defined to be parsed")
  }
  
  df %>% 
    mutate(AFRO_region = AFRO_region %>% 
             str_replace("_", " ") %>% 
             str_to_title(),
           AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels()))
}

unpack_region_draws <- function(df, 
                                value_col = "country_rates") {
  df %>% 
    pivot_longer(cols = contains(value_col),
                 names_to = "AFRO_region",
                 values_to = "value") %>% 
    mutate(AFRO_region = str_remove(AFRO_region, str_c(value_col, "_"))) %>% 
    parse_AFRO_region()
}

# Second post-processing step ---------------------------------------------

if (opt$redo | !file.exists(opt$bundle_filename)) {
  
  ## Cases by region and continent ---------------------------------------
  cases_continent <- combine_period_output(prefix_list = prefix_list,
                                           output_name = "mai_simulated_cases_all",
                                           output_dir = opt$output_dir)
  
  cases_by_region <- combine_period_output(prefix_list = prefix_list,
                                           output_name = "mai_cases_by_region",
                                           output_dir = opt$output_dir) %>% 
    mutate(AFRO_region = str_remove(variable, "country_cases_")) %>% 
    parse_AFRO_region()
  
  
  cases_by_region_draws <- combine_period_output(prefix_list = prefix_list,
                                                 output_name = "mai_cases_by_region_draws",
                                                 output_dir = opt$output_dir) %>% 
    unpack_region_draws(value_col = "country_cases")
  
  # Compute percentage of cases in Eastern and Central Africa
  frac_east_central_stats <- cases_by_region_draws %>% 
    group_by(.draw, period) %>% 
    summarise(
      east_central = sum(value[AFRO_region %in% c("Eastern Africa", "Central Africa")]),
      tot = sum(value),
      frac_east_central = east_central/tot
    ) %>% 
    group_by(period) %>% 
    summarise(
      mean = mean(frac_east_central),
      q025 = quantile(frac_east_central, 0.025),
      q975 = quantile(frac_east_central, 0.975)
    )
  
  saveRDS(frac_east_central_stats, file = str_glue("{opt$output_dir}/frac_cases_east_central_stats.rds"))
  
  ## Rates by region and continent ---------------------------------------
  rates_by_region <- combine_period_output(prefix_list = prefix_list,
                                           output_name = "mai_rates_by_region",
                                           output_dir = opt$output_dir) %>% 
    mutate(AFRO_region = str_remove(variable, "country_cases_")) %>%
    parse_AFRO_region()
  
  
  # Overall rates
  rates_overall <- combine_period_output(prefix_list = prefix_list,
                                         output_name = "mai_rates_all",
                                         output_dir = opt$output_dir)
  
  ## Gridded cases ---------------------------------------
  grid_cases <- combine_period_output(prefix_list = prefix_list,
                                      output_name = "mai_grid_cases",
                                      output_dir = opt$output_dir)
  
  grid_rates <- combine_period_output(prefix_list = prefix_list,
                                      output_name = "mai_grid_rates",
                                      output_dir = opt$output_dir)
  ## Population at risk ---------------------------------------
  
  # Risk categories for 50% cufoff
  risk_pop_50_adm2 <- combine_period_output(prefix_list = prefix_list,
                                            output_name = "risk_categories_50",
                                            output_dir = opt$output_dir) %>% 
    filter(admin_level == "ADM2"|(admin_level == "ADM1" & country == "LSO")) %>% 
    get_AFRO_region(ctry_col = "country")  %>% 
    mutate(AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels())) %>% 
    st_drop_geometry()
  
  # Risk categories for 95% cutoff
  risk_pop_95_adm2 <- combine_period_output(prefix_list = prefix_list,
                                            output_name = "risk_categories_95",
                                            output_dir = opt$output_dir) %>% 
    filter(admin_level == "ADM2"|(admin_level == "ADM1" & country == "LSO")) %>% 
    get_AFRO_region(ctry_col = "country")  %>% 
    mutate(AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels())) %>% 
    st_drop_geometry()
  
  
  
  # Number of people living in different risk categories using the "Lancet method"
  pop_at_risk <- combine_period_output(prefix_list = prefix_list,
                                       output_name = "pop_at_risk",
                                       output_dir = opt$output_dir) %>% 
    filter(admin_level == "ADM2") %>% 
    get_AFRO_region(ctry_col = "country")  %>% 
    mutate(AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels()))
  # Population at risk by WHO-AFRO region
  pop_at_risk_regions <- combine_period_output(prefix_list = prefix_list,
                                               output_name = "pop_at_risk_by_region",
                                               output_dir = opt$output_dir) %>% 
    mutate(AFRO_region = str_extract(variable, "(?<=tot_pop_risk_)(.)*(?=_adm)")) %>%
    parse_AFRO_region() %>% 
    unpack_pop_at_risk()
  
  # Population at risk by WHO-AFRO region
  pop_at_risk_regions_draws <- combine_period_output(prefix_list = prefix_list,
                                                     output_name = "pop_at_risk_by_region_draws",
                                                     output_dir = opt$output_dir) %>% 
    # Keep only adm2 data
    select(-contains("adm0"), -contains("adm1")) %>% 
    pivot_longer(cols = contains("tot"),
                 values_to = "value",
                 names_to = "variable") %>% 
    mutate(AFRO_region = str_extract(variable, "(?<=tot_pop_risk_)(.)*(?=_adm)")) %>%
    parse_AFRO_region() %>% 
    unpack_pop_at_risk()
  
  # Compute population in high risk categories (> 10/100,000 cases) at ADM2 level by region
  pop_high_risk_region_stats <- pop_at_risk_regions_draws %>% 
    group_by(period, AFRO_region, .draw, admin_level) %>% 
    summarise(n_high_risk = sum(value[!(risk_cat %in% get_risk_cat_dict()[1:2])])) %>% 
    group_by(period, AFRO_region, admin_level) %>% 
    summarise(mean = mean(n_high_risk),
              q025 = quantile(n_high_risk, 0.025),
              q975 = quantile(n_high_risk, 0.975)) %>% 
    ungroup()
  
  saveRDS(pop_high_risk_region_stats, file = str_glue("{opt$output_dir}/pop_high_risk_region_stats.rds"))
  
  
  # Compute population in high risk categories (> 10/100,000 cases) at ADM2 level overall
  pop_high_risk_all_stats <- pop_at_risk_regions_draws %>% 
    group_by(period, .draw, admin_level) %>% 
    summarise(n_high_risk = sum(value[!(risk_cat %in% get_risk_cat_dict()[1:2])])) %>% 
    group_by(period, admin_level) %>% 
    summarise(mean = mean(n_high_risk),
              q025 = quantile(n_high_risk, 0.025),
              q975 = quantile(n_high_risk, 0.975)) %>% 
    ungroup()
  
  saveRDS(pop_high_risk_all_stats, file = str_glue("{opt$output_dir}/pop_high_risk_all_stats.rds"))
  
  
  # Total population at risk
  pop_at_risk_all <- combine_period_output(prefix_list = prefix_list,
                                           output_name = "pop_at_risk_all",
                                           output_dir = opt$output_dir)  %>% 
    unpack_pop_at_risk()
  
  ## ADM Mean annual cases ---------------------------------------
  mai_adm_cases <- combine_period_output(prefix_list = prefix_list,
                                         output_name = "mai_cases_adm",
                                         output_dir = opt$output_dir)
  
  ## ADM2 level stats ---------------------------------------
  # Mean annual incidence rates at ADM2 level
  mai_adm_all <- combine_period_output(prefix_list = prefix_list,
                                       output_name = "mai",
                                       output_dir = opt$output_dir) %>% 
    mutate(run_id = str_c(country, period, sep = "-"),
           log10_rate_per_1e5 = log10(mean * 1e5)) %>% 
    get_AFRO_region("country")
  
  # Get unique spatial locations
  u_space_sf <- mai_adm_all %>% 
    group_by(location_period_id) %>%
    slice(1) %>% 
    ungroup() %>% 
    select(shapeName, admin_level, country, shp_id, location_period_id)
  
  mai_adm_all <- mai_adm_all %>% 
    st_drop_geometry() %>% 
    as_tibble()
  
  # Combine results with no-w runs
  mai_adm <- bind_rows(
    mai_adm_all %>% filter(admin_level == "ADM0", run_id %in% get_no_w_runs()),
    mai_adm_all %>% filter(admin_level == "ADM2", !(run_id %in% get_no_w_runs())),
    mai_adm_all %>% filter(admin_level == "ADM1" & country == "LSO", !(run_id %in% get_no_w_runs())),
    
  ) %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    get_AFRO_region("country")
  
  # Compute change map
  mai_change_adm <- compute_rate_changes(mai_adm)
  
  ## ADM2 mai ratio stats ---------------------------------------
  # Random draws
  random_draws <- sample(1:4000, 4000)
  
  mai_draws_p1 <- readRDS(str_glue("{opt$output_dir}/{prefix_list[1]}_mai_draws.rds")) %>%
    ungroup() %>% 
    select(.draw, location_period_id, value, country) %>% 
    filter(.draw %in% random_draws)
  
  mai_draws_p2 <- readRDS(str_glue("{opt$output_dir}/{prefix_list[2]}_mai_draws.rds")) %>%
    ungroup() %>% 
    select(.draw, location_period_id, value, country) %>% 
    filter(.draw %in% random_draws)
  
  mai_change_stats <- map_df(unique(mai_draws_p1$country), function(x) {
    cat("--- ", x, "\n")
    
    merge_ratio_draws(
      df1 = filter(mai_draws_p1, country == x),
      df2 = filter(mai_draws_p2, country == x)
    ) %>% 
      mutate(country = x)
  })
  
  mai_change_stats <- mai_change_stats %>%
    inner_join(u_space_sf %>% 
                 select(country, location_period_id, shp_id, admin_level), .)
  
  saveRDS(mai_change_stats, file = str_glue("{opt$output_dir}/mai_ratio_stats.rds"))
  
  
  mai_change_stats <- mai_change_stats %>% 
    st_drop_geometry() %>% 
    as_tibble()
  
  ## Region ration stats ----
  
  mai_region_draws_p1 <- readRDS(str_glue("{opt$output_dir}/{prefix_list[1]}_mai_rates_by_region_draws.rds")) %>%
    unpack_region_draws() %>% 
    filter(.draw %in% random_draws)
  
  mai_region_draws_p2 <- readRDS(str_glue("{opt$output_dir}/{prefix_list[2]}_mai_rates_by_region_draws.rds")) %>%
    unpack_region_draws() %>% 
    filter(.draw %in% random_draws)
  
  mai_region_change_stats <- map_df(unique(mai_region_draws_p2$AFRO_region), function(x) {
    cat("--- ", x, "\n")
    
    merge_ratio_draws(
      df1 = filter(mai_region_draws_p1, AFRO_region == x),
      df2 = filter(mai_region_draws_p2, AFRO_region == x),
      unit_col = "AFRO_region"
    )
  })
  
  saveRDS(mai_region_change_stats, file = str_glue("{opt$output_dir}/mai_region_ratio_stats.rds"))
  
  ## Overall stats ----
  mai_afr_draws_p1 <- readRDS(str_glue("{opt$output_dir}/{prefix_list[1]}_mai_rates_all_draws.rds")) %>%
    mutate(value = tot,
           unit = "AFR") %>% 
    filter(.draw %in% random_draws)
  
  mai_afr_draws_p2 <- readRDS(str_glue("{opt$output_dir}/{prefix_list[2]}_mai_rates_all_draws.rds")) %>%
    mutate(value = tot,
           unit = "AFR") %>% 
    filter(.draw %in% random_draws)
  
  mai_afr_change_stats <- merge_ratio_draws(
    df1 = mai_afr_draws_p1,
    df2 = mai_afr_draws_p2,
    unit_col = "unit"
  ) 
  
  saveRDS(mai_afr_change_stats, file = str_glue("{opt$output_dir}/mai_Africa_ratio_stats.rds"))
  
  
  ## Changes between periods ---------------------------------------
  # Compute changes at ADM0 level
  mai_adm0_changes <-  mai_adm_all %>% 
    filter(admin_level == "ADM0") %>% 
    compute_rate_changes() %>% 
    ungroup() %>% 
    get_AFRO_region(ctry_col = "country") %>% 
    mutate(AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels()))
  
  # Compute changes for regions as well
  mai_region_changes <- rates_by_region %>% 
    mutate(country = variable) %>% 
    rename(location_period_id = variable) %>% 
    compute_rate_changes() %>% 
    mutate(AFRO_region = str_remove(country, "country_rates_")) %>% 
    parse_AFRO_region()
  
  # Changes at the continent level
  mai_all_changes <- rates_overall %>% 
    mutate(country = variable) %>% 
    rename(location_period_id = variable) %>% 
    compute_rate_changes() %>% 
    mutate(country = "SSA")
  
  
  # Combine mai change data
  combined_mai_changes <- bind_rows(
    # AFRO regions
    mai_region_changes %>% 
      mutate(country = AFRO_region,
             region = AFRO_region), 
    # ADM2 level
    mai_adm0_changes %>% 
      mutate(region = AFRO_region),
    # Continent level
    mai_all_changes %>% 
      mutate(country = "SSA", 
             region = "SSA")
  ) %>% 
    mutate(
      # !! change rate values to 1e-1/100'000 for display
      across(
        c("2011-2015", "2016-2020"), 
        function(x) {
          x[x < 1e-6] <- 1e-6
          x
        }),
      admin_level = case_when(
        str_detect(location_period_id, "country") ~ "region",
        location_period_id == "tot" ~ "continent",
        TRUE ~ "ADM0"),
      admin_level = factor(admin_level, levels = c("continent", "region", "ADM2")),
      region = factor(region, levels = c("SSA", get_AFRO_region_levels()))
    )
  
  
  ## Get intended runs ---------------------------------------
  intended_runs <- get_intended_runs()
  
  ## Spatial data  ---------------------------------------
  data(afr_sf, package = "taxdat")
  
  afr_sf <- afr_sf %>% 
    janitor::clean_names() %>% 
    mutate(country_name = country,
           intended_run = country %in% intended_runs$isocode) %>% 
    filter(country_name != "YEM") %>% 
    st_crop(st_bbox(st_sfc(
      st_point(c(-18.8, 36.6)), 
      st_point(c(52.2, -36.5)),
      crs = 4326))) %>% 
    get_AFRO_region(ctry_col = "country") %>% 
    mutate(AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels())) %>% 
    st_make_valid()
  
  afr_sf_ch <- st_union(afr_sf) %>% 
    nngeo::st_remove_holes() %>% 
    st_make_valid()
  
  # Lakes and rivers for plots
  lakes_sf <- get_lakes() %>% 
    st_make_valid() %>% 
    st_filter(afr_sf_ch, .predicate = st_within)
  rivers_sf <- get_rivers() %>% 
    st_make_valid() %>% 
    st_filter(afr_sf_ch, .predicate = st_within)
  
  coasts_sf <- st_cast(afr_sf_ch, "MULTILINESTRING") %>% 
    st_as_sf()
  
  ## Generated observations ------
  gen_obs <- combine_period_output(prefix_list = prefix_list,
                                   output_name = "gen_obs",
                                   output_dir = opt$output_dir) %>% 
    mutate(admin_level = str_c("ADM", admin_level)) %>% 
    get_AFRO_region(ctry_col = "country") %>% 
    mutate(AFRO_region = factor(AFRO_region, levels = get_AFRO_region_levels()))
  
  
  ## Mean ADM population by time period ------
  population <- combine_period_output(prefix_list = prefix_list,
                                      output_name = "population",
                                      output_dir = opt$output_dir) %>% 
    get_AFRO_region(ctry_col = "country") %>% 
    mutate(AFRO_region = factor(AFRO_region, levels = get_AFRO_region_levels()))
  
  # Add popultation to mai
  mai_adm <- mai_adm %>% 
    inner_join(population %>% select(location_period_id, pop = mean, period))
  
  mai_adm_all <- mai_adm_all  %>% 
    inner_join(population %>% select(location_period_id, pop = mean, period)) 
  
  # ## Incidence ratios around rivers and lakes -----
  # 
  # dist_sf <- list("rivers" = rivers_sf,
  #                 "lakes" = lakes_sf,
  #                 "coasts" = coasts_sf,
  #                 "freshwater" = bind_rows(
  #                   rivers_sf,
  #                   lakes_sf
  #                 ),
  #                 "water" = bind_rows(
  #                   rivers_sf,
  #                   lakes_sf,
  #                   coasts_sf
  #                 ))
  # 
  # 
  # irr_dat <- map_df(seq_along(dist_sf), function(x) {
  #   map_df(c("within", "category"), function(y) {
  #     cat("---- ", names(dist_sf)[x], y, "\n")
  #     
  #     irr_dist(dist_sf[[x]],
  #              dist_vec = seq(5e3, 100e3, by = 10e3),
  #              mai_adm = mai_adm,
  #              mai_adm_all = mai_adm_all,
  #              grid_cases = grid_cases,
  #              afr_sf = afr_sf,
  #              dist_definition = y,
  #              by_region = T) %>% 
  #       mutate(what = names(dist_sf)[x])
  #   })
  # })
  
  
  ## Save data  ---------------------------------------
  save(list = ls(), file = opt$bundle_filename)
  
} else {
  cat("---- Loading pre-computed data. \n")
  load(opt$bundle_filename)
  
  #load mai_change_stats (4000 draws a separate file)
  mai_change_stats <- readRDS(str_glue("{opt$output_dir}/mai_ratio_stats.rds")) %>% 
    st_drop_geometry() %>% 
    as_tibble()
  
  # Number of people living in different risk categories
  # threshold is 50%
  risk_pop_50_adm2 <- combine_period_output(prefix_list = prefix_list,
                                            output_name = "risk_categories_50",
                                            output_dir = opt$output_dir) %>% 
    filter(admin_level == "ADM2"|(admin_level == "ADM1" & country == "LSO")) %>% 
    get_AFRO_region(ctry_col = "country")  %>% 
    mutate(AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels())) %>% 
    st_drop_geometry()
  
  # threshold is 95%
  risk_pop_95_adm2 <- combine_period_output(prefix_list = prefix_list,
                                            output_name = "risk_categories_95",
                                            output_dir = opt$output_dir) %>% 
    filter(admin_level == "ADM2"|(admin_level == "ADM1" & country == "LSO")) %>% 
    get_AFRO_region(ctry_col = "country")  %>% 
    mutate(AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels())) %>% 
    st_drop_geometry()
}

# Figure 1: cases ---------------------------------------------------------

## Figure 1A: gridded cases by time period --------------------------------

# Gridded maps of cases
p_fig1A <- output_plot_map(sf_obj = grid_cases %>% 
                             mutate(log10_cases = log10(mean),
                                    period = factor(period, levels = c("2016-2020", "2011-2015"))),
                           lakes_sf = lakes_sf,
                           rivers_sf = rivers_sf,
                           all_countries_sf = afr_sf,
                           fill_var = "log10_cases",
                           fill_color_scale_type = "cases",
                           cholera_dir = opt$cholera_dir) +
  facet_wrap(~ period) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.1, .3),
        panel.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_colorbar("Mean annual incidence \n[cases/year]"))

# Save
ggsave(p_fig1A,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_1A.png"),
       width = 10,
       height = 6, 
       dpi = 300)

## Figure 1B: cases by region and time period --------------------------------
# Horizontal barplot of cases by region
p_fig1B <- cases_by_region %>% 
  mutate(AFRO_region = factor(AFRO_region, levels = get_AFRO_region_levels()),
         period = factor(period, levels = rev(c("2016-2020", "2011-2015")))) %>% 
  ggplot(aes(x = mean, y = period, fill = AFRO_region)) +
  geom_bar(stat = "identity")  +
  geom_errorbarh(data = cases_continent,
                 inherit.aes = F,
                 aes(xmin = q2.5, xmax = q97.5, y = period), 
                 height = .2) +
  scale_x_continuous(
    labels = function(x) {
      formatC(x, digits = 0, big.mark = "'", format = "f")
    }) +
  scale_fill_manual(values = colors_afro_regions()) +
  theme_bw() +
  theme(legend.key.size = unit(.2, units = "in"),
        legend.title=element_blank()) +
  labs(x = "Mean annual cholera incidence [cases/year]", 
       y = "Time period")

# Save figure
ggsave(plot = p_fig1B,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_1B.png"),
       width = 8,
       height = 2.5,
       dpi = 300)

# Map of WHO regions
p_fig_BC_regions <- ggplot(afr_sf, aes(fill = AFRO_region)) +
  geom_sf() +
  taxdat::map_theme() +
  guides(fill = "none") +
  scale_fill_manual(values = colors_afro_regions()) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank())


## Assemble Figure 1 ---------

p_fig1 <- cowplot::plot_grid(
  p_fig1B +
    theme(plot.margin = unit(c(2, 9, 1, 3), "lines")),
  p_fig1A,
  nrow = 2,
  labels = "auto",
  rel_heights = c(.35, 1)
)  + theme(panel.background = element_rect(fill = "white", color = "white"))

p_fig1_v2 <- ggdraw() +
  draw_plot(p_fig1) +
  draw_plot(p_fig_BC_regions, x = 0.78, y = .77, width = .23, height = .23)

# Save
ggsave(plot = p_fig1_v2,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_1.png"),
       width = 10,
       height = 7.5,
       dpi = 300)


# Figure 2: changes between periods ------------------------------------------

## load mai region change stats 
mai_region_change_stats<-readRDS(str_glue("{opt$output_dir}/mai_region_ratio_stats.rds"))

# Combine incidence rate ratio stats across countries and regions
irr_periods <- bind_rows(
  mai_change_stats %>% 
    filter(admin_level == "ADM0") %>% 
    select(unit = country, q2.5, q97.5),
  mai_region_change_stats %>% 
    rename(unit = AFRO_region),
  mai_afr_change_stats %>% 
    mutate(unit = "SSA")) %>% 
  select(unit, irr_low = q2.5, irr_high = q97.5) %>% 
  mutate(sigificant_irr = ifelse(irr_low > 1 | irr_high < 1, 
                                 "Bayesian p-value <= 0.05",
                                 "Bayesian p-value > 0.05") %>% 
           factor(levels = c("Bayesian p-value <= 0.05", "Bayesian p-value > 0.05")))

# Alternative figure 2A
dat_for_incid_dotplot <-  combined_mai_changes %>% 
  mutate(
    # Incidence rates per 100,000
    p1 = log10(`2011-2015`*1e5), 
    p2 = log10(`2016-2020`*1e5),
    AFRO_region = case_when(admin_level != "ADM2" ~ admin_level,
                            TRUE ~ region),
    admin_level = ifelse(admin_level == "ADM2", "country", admin_level),
    country = factor(country) %>% 
      forcats::fct_reorder(p2),
    direction = ifelse(rate_ratio > 1, "increase", "decrease") %>% 
      factor(levels = c("increase", "decrease"))) %>% 
  inner_join(irr_periods, by = c("country" = "unit")) %>% 
  select(admin_level, AFRO_region, country, p1, p2, direction, sigificant_irr) %>% 
  mutate(country = factor(country) %>% 
           forcats::fct_reorder(p2))

make_dotlineplot <- function(df) {
  df %>% 
    pivot_longer(cols = c("p1", "p2"),
                 names_to = "period")  %>% 
    group_by(country) %>% 
    mutate(p2_value = value[period == "p2"]) %>% 
    ungroup() %>% 
    mutate(country = factor(country) %>% 
             forcats::fct_reorder(p2_value)) %>%
    mutate(period = ifelse(period == "p1", "2011-2015", "2016-2020")) %>% 
    ggplot(aes(y = country)) +
    geom_point(aes(x = value, pch = period), size = 2)  +
    geom_segment(data = df, 
                 aes(x = p1, y = country, xend = p2, yend = country,
                     #alpha = sigificant_irr, 
                     color = direction#,linewidth = sigificant_irr
                     ),
                 arrow = arrow(length = unit(0.15, "cm"), 
                               type="closed")#, 
                 #lwd = .3
                 ) +
    # scale_linetype_manual(values = c(4, 1))  +
    #scale_alpha_manual(values = c(1, .3)) +
    #scale_linewidth_manual(values = c(1,0.3)) +
    scale_x_continuous(limits = c(-1.1, 2.3),
                       breaks = seq(-1, 2),
                       labels = formatC(10^(seq(-1, 2)),
                                        digits = 1,
                                        format = "fg", 
                                        big.mark = ",") %>% 
                         str_replace("0.1", "<= 0.1")) +
    scale_color_manual(values = c("red", "blue")) +
    scale_shape_manual(values = c(1, 16)) +
    # ggh4x::facet_nested(admin_level + AFRO_region ~ ., scale = "free", 
    # space = "free", switch = "y") +
    theme_bw() +
    theme(strip.placement = "out") +
    labs(y = NULL, 
         x = "Cholera incidence rate \n[reported cases per 100,000/year]",
         alpha = "Statististically-significant\nchange",
         color = "Change direction",
         shape = "Time period") +
    theme(panel.grid.major.y = element_blank())
}

# Solution for strip colors in https://stackoverflow.com/questions/19440069/ggplot2-facet-wrap-strip-color-based-on-variable-in-data-set
strip <- ggh4x::strip_themed(
  background_y = ggh4x::elem_list_rect(fill = colors_afro_regions()[c(2, 3, 4, 1)]),
  text_y = ggh4x::elem_list_text(color = c("white", "white", "white", "white"))
)

p_fig2A <- plot_grid(
  # SSA
  make_dotlineplot(dat_for_incid_dotplot %>% 
                     filter(country == "SSA")) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    guides(shape = "none", color = "none", alpha = "none"),
  # Regions
  make_dotlineplot(dat_for_incid_dotplot %>% 
                     filter(str_detect(country, "Africa"))) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())  +
    guides(shape = "none", color = "none", alpha = "none"),
  # Countries
  make_dotlineplot(dat_for_incid_dotplot %>% 
                     filter(str_detect(country, "Africa|SSA", negate = T)) %>% 
                     # Reoder in order of 2016-2020 incidence, this may be done
                     # automatically based on the data, being lazy here
                     mutate(AFRO_region = factor(
                       AFRO_region, 
                       levels = c("Central Africa", "Eastern Africa",
                                  "Southern Africa", "Western Africa")))) +
    ggh4x::facet_grid2(AFRO_region ~ ., switch = "y", scales = "free_y", space = "free_y",
                       strip = strip),
  # facet_grid(AFRO_region ~ ., switch = "y", scales = "free_y", space = "free_y"),
  ncol = 1,
  rel_heights = c(.15, .25, 1),
  align = "v",
  axis = "lr"
)

p_fig2A

# Save
ggsave(p_fig2A,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2A.png"),
       width = 10,
       height = 10, 
       dpi = 300)

## Figure 2A: national-level scatterplot  ---------
# p_fig2A <- combined_mai_changes %>% 
#   ggplot(aes(x = log10(`2011-2015`*1e5), 
#              y =  log10(`2016-2020`*1e5), 
#              col = region)) +
#   geom_abline(lty = 2, lwd = .2) +
#   geom_point(aes(pch = admin_level,
#                  # alpha = admin_level
#                  size = admin_level)) +
#   ggrepel::geom_label_repel(
#     aes(label = country, size = admin_level, alpha = admin_level),
#     min.segment.length = 0,
#     nudge_x = 0,
#     # nudge_y = .1,
#     max.overlaps = Inf, 
#     xlim = c(-1, 2.2),
#     ylim = c(-1, 2.2)) +
#   scale_size_manual(values = c(6, 4, 2.5)) +
#   scale_shape_manual(values = c(15, 17, 16)) +
#   # scale_alpha_manual(values = c(1, 1, .75)) +
#   scale_color_manual(values = c("black", colors_afro_regions())) +
#   theme_bw() +
#   guides(color = guide_legend("WHO regions")) +
#   scale_x_continuous(limits = c(-1.1, 2.3),
#                      breaks = seq(-1, 2),
#                      labels = formatC(10^(seq(-1, 2)),
#                                       digits = 1,
#                                       format = "fg", 
#                                       big.mark = ",")) +
#   scale_y_continuous(limits = c(-1.1, 2.3),
#                      breaks = seq(-1, 2),
#                      labels = formatC(10^(seq(-1, 2)),
#                                       digits = 1,
#                                       format = "fg", 
#                                       big.mark = ",")) +
#   labs(x = "Incidence rate 2011-2015\n[cases per 100,000/year]",
#        y = "Incidence rate 2016-2020\n[cases per 100,000/year]") +
#   guides(color = "none", size = "none", shape = "none", alpha = "none") +
#   scale_alpha_manual(values = c(1, 1, 0)) #+
# # scale_size_manual(values = c(3, 2, .7))
# 
# 
# # Save
# ggsave(p_fig2A,
#        file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2A.png"),
#        width = 8,
#        height = 7, 
#        dpi = 150)


## Figure 2B: rate ratio maps ---------

# Compute significant changes in terms of quantiles of rate ratios
mai_adm2_change_stats <- mai_change_stats %>% 
  filter(admin_level == "ADM2"|(admin_level == "ADM1" & country == "LSO")) %>% 
  mutate(change_direction = case_when(
    q2.5 > 1 ~ "increase",
    q97.5 < 1 ~ "decrease",
    T ~ "no change"
  ))#,
# change_direction = factor(change_direction, levels = c("increase", "no change", "decrease")))

# Get the model runs by country
# Copy data from https://livejohnshopkins.sharepoint.com/:x:/r/sites/CholeraMappingGrant/Shared%20Documents/Mapping%20Pipeline/Documentation/final_models_by_country.csv?d=w11e3a81fd229423681237bc63bc530ec&csf=1&web=1&e=7DV68a
# into Analysis/output
model_runs_by_country <- read_csv("Analysis/output/final_models_by_country.csv") %>% 
  janitor::clean_names() %>% 
  rename(reason = reason_for_deviation_from_standard_model_and_data_processing,
         country = country_code)

# Select countries for which at least one period had a no-w run and non-0 cases
no_w_case_runs <- model_runs_by_country %>% 
  filter(model == "no_w", str_detect(reason, "zero", negate = T))

#  Rate change map
p_fig2B <- mai_change_adm %>% 
  inner_join(u_space_sf, .) %>%
  output_plot_map(sf_obj = .,
                  lakes_sf = lakes_sf,
                  rivers_sf = rivers_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "log10_rate_ratio",
                  fill_color_scale_type = "ratio") +
  # Add the significance information
  geom_sf(data = mai_adm2_change_stats  %>% 
            inner_join(u_space_sf, .) %>% 
            filter(change_direction == "no change"),
          inherit.aes = F,
          aes(color = change_direction),
          alpha = 0,
          lwd = .05) +
  geom_sf(data = mai_adm2_change_stats  %>% 
            inner_join(u_space_sf, .) %>% 
            filter(change_direction != "no change",
                   # Remove no_w no-zero runs
                   !(country %in% no_w_case_runs$country)),
          inherit.aes = F,
          aes(color = change_direction),
          alpha = 0,
          lwd = .05) +
  theme(legend.position = "right") +
  scale_color_manual(values = c("blue", "red", "darkgray")) +
  guides(fill = guide_colorbar("Ratio of incidence rates\n[2016-2020/2011-2015]"),
         color = guide_legend("Change significance")) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.2, .3))

# Save
ggsave(p_fig2B,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2B.png"),
       width = 7,
       height = 6, 
       dpi = 150)


## Assemble Figure 2 ------

# p_fig2 <- plot_grid(
#   p_fig2A +
#     theme(plot.margin = unit(c(2.5, 1.5, 2.5, 1.5), "lines")),
#   p_fig2B +
#     theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
#   nrow = 1,
#   labels = "auto"#,
# ) +
#   theme(panel.background = element_rect(fill = "white", color = "white"))
# 
# # Save
# ggsave(plot = p_fig2,
#        filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2.png"),
#        width = 12,
#        height = 6,
#        dpi = 300)


p_fig2 <- plot_grid(
  p_fig2A +
    theme(plot.margin = unit(c(1, -1, 1, 1), "lines")),
  p_fig2B,
  ncol = 2,
  nrow = 1,
  labels = c("a", "b"),
  rel_widths = c(1, 1.5)
) +
  theme(panel.background = element_rect(fill = "white", color = "white"))

# Save
ggsave(plot = p_fig2,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2.png"),
       width = 18,
       height = 9,
       dpi = 300)


# Figure 3: population at risk --------------------------------------------

## Fig. 3A: People per risk category --------

# Uncertainty bounds for totals
risk_pop_all <- pop_at_risk_all %>% 
  filter(period == "2016-2020", 
         admin_level == "ADM2",
         risk_cat != "<1") %>% 
  select(risk_cat, mean, q2.5, q97.5)

# Values by AFRO region
risk_pop_regions <- pop_at_risk_regions %>% 
  filter(period == "2016-2020", 
         admin_level == "ADM2",
         risk_cat != "<1") %>% 
  select(AFRO_region, risk_cat, mean, q2.5, q97.5)

p_fig3A <- risk_pop_regions %>%
  ggplot(aes(y = risk_cat, x = mean)) +
  geom_bar(aes(fill = AFRO_region), stat = "identity", width = .5) +
  geom_errorbar(data = risk_pop_all,
                inherit.aes = F,
                aes(xmin = q2.5, xmax = q97.5, y = risk_cat), width = 0.2) +
  geom_point(data = risk_pop_all, aes(x = mean)) +
  theme_bw() +
  scale_fill_manual(values = colors_afro_regions()) +
  scale_x_continuous(labels = function(x) {formatC(x/1e6)}) +
  labs(y = "Incidence risk category", x = "ADM2 population at risk [millions]") +
  theme(legend.title=element_blank()) 

# Save
ggsave(plot = p_fig3A,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3A.png"),
       width = 12,
       height = 7,
       dpi = 300)

## Fig. 3B: ADM2 level risk category map --------

# Use 50% cutoff for main figure
p_fig3B <- risk_pop_50_adm2 %>% 
  select(-shp_id) %>% 
  filter(period == "2016-2020") %>% 
  inner_join(u_space_sf, .) %>% 
  output_plot_map(sf_obj = ., 
                  lakes_sf = lakes_sf,
                  rivers_sf = rivers_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "risk_cat",
                  fill_color_scale_type = "risk category") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.2, .3),
        panel.background = element_rect(fill = "white", color = "white"))+
  guides(fill = guide_legend("Risk category"))


# Save
ggsave(p_fig3B,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3B.png"),
       width = 7,
       height = 6, 
       dpi = 300)

## Assemble Figure 3 ----

p_fig3 <- plot_grid(
  p_fig3A +
    theme(plot.margin = unit(c(2, 1, 2, 2), units = "lines"),
          legend.position = c(.75, .6)),
  p_fig3B +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines")),
  # theme(plot.margin = unit(c(-5, -5, -5, -3), units = "lines")),
  nrow = 1,
  labels = "auto"#,
  # rel_widths = c(1, 1),
  # align = "v",
  # axis = "lr"
) +
  theme(panel.background = element_rect(fill = "white", color = "white"))

# Save
ggsave(plot = p_fig3,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3.png"),
       width = 12,
       height = 6,
       dpi = 600)


# Figure 4 ----------------------------------------------------------------

## Fig. 4A: Change in risk categories (50% cutoff) ----
endemicity_df_50_v2 <- risk_pop_50_adm2 %>% 
  mutate(high_risk = risk_cat %in% get_risk_cat_dict()[3:6],
         low_risk = risk_cat %in% get_risk_cat_dict()[1]) %>% 
  group_by(country, location_period_id) %>% 
  summarise(
    endemicity = case_when(
      sum(high_risk) == 2 ~ "high-both",
      sum(low_risk) == 2 ~ "low-both",
      sum(high_risk) == 1 ~ "high-either",
      T ~ "mix"
    ),
    pop = max(pop)
  ) %>% 
  mutate(endemicity = factor(endemicity, 
                             levels = c("high-both", "high-either",
                                        "mix", "low-both"),
                             labels = c("sustained high risk", 
                                        "history of high risk",
                                        "history of moderate risk",
                                        "sustained low risk")))  

saveRDS(endemicity_df_50_v2, file = str_glue("{opt$output_dir}/endemicity_50.rds"))

# 95% cutoff ----
endemicity_df_95_v2 <- risk_pop_95_adm2 %>% 
  mutate(high_risk = risk_cat %in% get_risk_cat_dict()[3:6],
         low_risk = risk_cat %in% get_risk_cat_dict()[1]) %>% 
  group_by(country, location_period_id) %>% 
  summarise(
    endemicity = case_when(
      sum(high_risk) == 2 ~ "high-both",
      sum(low_risk) == 2 ~ "low-both",
      sum(high_risk) == 1 ~ "high-either",
      T ~ "mix"
    ),
    pop = max(pop)
  ) %>% 
  mutate(endemicity = factor(endemicity, 
                             levels = c("high-both", "high-either",
                                        "mix", "low-both"),
                             labels = c("sustained high risk", 
                                        "history of high risk",
                                        "history of moderate risk",
                                        "sustained low risk")))  

saveRDS(endemicity_df_95_v2, file = str_glue("{opt$output_dir}/endemicity_95.rds"))

# Figure 4A
p_fig4A <- endemicity_df_50_v2 %>% 
  inner_join(u_space_sf, .) %>% 
  output_plot_map(sf_obj = .,
                  lakes_sf = lakes_sf,
                  rivers_sf = rivers_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "endemicity",
                  fill_color_scale_type = "endemicity",
                  border_width = .03) +
  theme(legend.position = c(.2, .3),
        panel.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend("10-year risk\ncategory"))

# Save
ggsave(p_fig4A,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_4A_risk_cat.png"),
       width = 12,
       height = 6, 
       dpi = 150)

# Tile for legend
hrisk_cat <- taxdat::get_risk_cat_dict()[-c(1:2)]
tile_dat <- expand.grid(x = taxdat::get_risk_cat_dict(), 
                        y = taxdat::get_risk_cat_dict()) %>% 
  as_tibble() %>% 
  mutate(endemicity = case_when(x == "<1" & y == "<1" ~ "sustained low risk",
                                x %in% hrisk_cat & y %in% hrisk_cat ~ "sustained high risk",
                                x %in% hrisk_cat | y %in% hrisk_cat~ "history of high risk",
                                T ~ "history of moderate risk"),
         endemicity = factor(endemicity, levels = c("sustained high risk", 
                                                    "history of high risk",
                                                    "history of moderate risk",
                                                    "sustained low risk")))

endemicity_legend <- tile_dat %>% 
  ggplot(aes(x = x, y = y, fill = endemicity)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = taxdat:::colors_endemicity()) +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text = element_text(size = 4.5),
        axis.title = element_text(size = 5.5),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 7),
        legend.key.size = unit(.5, units = "lines"),
        legend.box.spacing = unit(.1, units = "lines")) +
  labs(x = "Risk category in 2011-2015", y = "Risk category in 2016-2020",
       fill = "10-year risk\ncategory")


## Fig. 4B: Fraction by categories for supplement ----
p_fig4B <- endemicity_df_50_v2  %>%
  group_by(country) %>% 
  complete(endemicity = unique(endemicity_df_50_v2$endemicity)) %>% 
  get_AFRO_region(ctry_col = "country") %>% 
  group_by(AFRO_region, country, endemicity) %>%
  summarise(n = sum(!is.na(location_period_id)),
            pop = sum(pop[!is.na(location_period_id)])) %>% 
  group_by(AFRO_region, country) %>%
  mutate(frac = pop/sum(pop)) %>% 
  group_by(country) %>% 
  mutate(
    frac_other = frac[endemicity == "history of moderate risk"],
    frac_high = sum(frac[endemicity %in% c("sustained high risk", "history of high risk")]),
    frac_low = sum(frac[endemicity %in% c("sustained low risk")])
  ) %>% 
  ungroup() %>% 
  mutate(endemicity = forcats::fct_rev(endemicity),
         country = factor(country, levels = unique(country[order(frac_high, frac_other)])),
  ) %>% 
  ggplot(aes(y = country, x = frac, fill = endemicity)) +
  geom_bar(stat = "identity") +
  facet_grid(AFRO_region ~., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = rev(taxdat:::colors_endemicity())) +
  theme_bw() +
  labs(x = "fraction of population\n per 10-year risk category")

ggsave(p_fig4B,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_4B.png"),
       width = 6,
       height = 7, 
       dpi = 150)

p_fig4A_legend <- ggdraw(
  p_fig4A +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(1, 0, 1, 0), units = "lines")) +
    guides(fill = "none")
) +
  draw_plot(endemicity_legend, .075, .24, .35, .25)

p_fig4 <- plot_grid(
  p_fig4A_legend +
    theme(panel.background = element_rect(fill = "white", color = "white")),
  p_fig4B +
    guides(fill = "none") +
    theme(panel.background = element_rect(fill = "white", color = "white"),
          plot.margin = unit(c(2, 1.5, 1.5, 1.5), units = "lines")),
  nrow = 1,
  labels = "auto",
  rel_widths = c(1, .5)
) +
  theme(panel.background = element_rect(fill = "white", color = "white"))


# Save
ggsave(plot = p_fig4,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_4.png"),
       width = 12,
       height = 7,
       dpi = 300)

# Figure 5: cholera occurrence -----------------------------------------------------

# Load outbreak data and results (50%)
load(str_c(opt$output_dir, "/outbreak_analysis_data.rdata"))
load(str_c(opt$output_dir, "/recent_cholera_outbreaks_res.rdata"))

# Load endemicity data for 50% cutoff and 95% cutoff 
endemicity_df_v2_50 <- readRDS(str_glue("{opt$output_dir}/endemicity_50.rds"))
endemicity_df_v2_95 <- readRDS(str_glue("{opt$output_dir}/endemicity_95.rds"))

# Fix admin level naming
final_joins <- final_joins %>% 
  mutate(admin_level = str_c("ADM", admin_level))

# Map of cholera occurrence locations
p_ob_map2 <- endemicity_df_v2_50 %>% 
  inner_join(u_space_sf, .) %>% 
  select(-admin_level) %>% 
  ggplot() +
  geom_sf(data = afr_sf %>% 
            select(-admin_level),
          inherit.aes = FALSE,
          lwd = 0.15,
          color = "darkgray",
          alpha = 0) +
  geom_sf(aes(fill = endemicity), alpha = .5, lwd = .005, color = "white") +
  geom_sf(inherit.aes = FALSE,
          data = final_joins,
          aes(color = "locations with\nreported cholera\nin 2022-2023"),
          alpha = 0,
          lwd = .35) +
  geom_sf(inherit.aes = FALSE,
          data = st_centroid(final_joins %>% select(-geom.y)),
          alpha = 1, col = "purple",
          fill = "white",
          pch = 21,
          size = .6,
          stroke = .2) +
  taxdat::map_theme() +
  scale_color_manual(values = c("purple")) +
  theme(panel.border = element_blank()) +
  theme(legend.position = c(.23, .4)) +
  scale_fill_manual(values = taxdat:::colors_endemicity()) +
  labs(color = NULL) +
  guides(fill = guide_legend("10-year risk\ncategory", override.aes = list(alpha = 1))) +
  scale_shape_manual(values = c(1, 3, 4))

ggsave(plot = p_ob_map2,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_5_a_map.png"),
       width = 12,
       height = 5,
       dpi = 100)

# Distribution of 10-year risk categories among ADM2 locations
ob_count_dat <-  obs_outbreaks %>% 
  mutate(occurrence = "       cholera\n       observed") %>% 
  bind_rows(non_obs_outbreaks %>% 
              mutate(occurrence = "no cholera       \nobserved       ")) %>% 
  mutate(occurrence = factor(occurrence, 
                             levels = c("no cholera       \nobserved       ",
                                        "       cholera\n       observed"))) %>% 
  mutate(endemicity = factor(endemicity, 
                             levels = levels(endemicity_df_v2_50$endemicity)),
         AFRO_region = factor(AFRO_region %>% 
                                str_replace(" ", "\n"),
                              levels = rev(c("overall", get_AFRO_region_levels() %>% 
                                               str_replace(" ", "\n"))),
                              labels = rev(c("overall", get_AFRO_region_levels() %>% 
                                               str_replace(" ", "\n")))))

p_frac_regions <- ob_count_dat %>% 
  filter(AFRO_region != "overall")  %>%
  mutate(occurrence = str_remove_all(occurrence, "       ")) %>% 
  ggplot(aes(x = frac, y = AFRO_region, fill = endemicity)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = taxdat:::colors_endemicity()) +
  labs(x = "proportion of locations (ADM2 or lower)", y = "") +
  guides(fill = "none") +
  facet_grid(factor(occurrence, levels = c("cholera\nobserved","no cholera\nobserved")) ~ ., switch = "y") +
  theme(strip.placement = "outer")


p_frac_overall <- ob_count_dat %>% 
  mutate(occurrence = str_remove_all(occurrence, "  ")) %>% 
  filter(AFRO_region == "overall")  %>% 
  ggplot(aes(x = factor(occurrence, levels = c("no cholera \nobserved "," cholera\n observed")), y = frac, fill = endemicity)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = taxdat:::colors_endemicity()) +
  labs(y = "proportion of locations", x = "") +
  guides(fill = "none") +
  coord_flip()

p_ob_frac_comb <- cowplot::plot_grid(
  p_frac_overall +
    theme(plot.margin = unit(c(1, .3, 0, 1), units = "lines")) +
    theme(axis.text = element_text(size = 8, hjust = .5),
          axis.title = element_text(size = 10)),
  p_frac_regions +
    theme(plot.margin = unit(c(1, 1, 0, 1), units = "lines"),
          strip.switch.pad.grid = unit(.7, units = "line")) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          strip.text = element_text(size = 8)), 
  nrow = 1,
  labels = c("b", "c"),
  rel_widths = c(.6, 1),
  align = "h",
  axis = "tb"
)

# Model estimates
pd1 <- position_dodge(.4)
pd2 <- position_dodge(.3)

p_ob_1 <- baseline_prob_stats %>% 
  mutate(what = "reference",
         param = case_when(str_detect(param, "baseline") ~ "sustained low risk",
                           TRUE ~ param)) %>% 
  ggplot(aes(x = param, y = mean, ymin = q2.5, ymax = q97.5, color = AFRO_region)) +
  geom_point(position = pd1) +
  geom_errorbar(width = 0, position = pd1) +
  theme_bw() +
  facet_grid(. ~ what, scales = "free", space = "free") +
  scale_color_manual(values = c("overall" = "black", colors_afro_regions())) +
  labs(x = "", y = "probability of cholera occurrence") +
  guides(color = "none") +
  coord_cartesian(ylim = c(0, 1))  +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

p_ob_2 <- logOR_stats %>% 
  mutate(what = str_replace(what, "outbreak", "cholera")) %>% 
  ggplot(aes(x = param, y = mean, ymin = q2.5, ymax = q97.5, color = AFRO_region)) +
  geom_point(position = pd2) +
  geom_errorbar(width = 0, position = pd2) +
  geom_hline(aes(yintercept = 0), lty = 3, lwd = .6) +
  facet_grid(. ~ what, scales = "free", space = "free") +
  theme_bw() +
  scale_color_manual(values = c("overall" = "black", colors_afro_regions())) +
  labs(x = "10-year cholera risk category", y = "log-Odds ratio", color = NULL) +
  theme(legend.position = c(.145, .84),
        legend.key.height = unit(.75, units = "lines"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 8))


p_fig5 <- plot_grid(
  plot_grid(
    p_ob_map2, 
    plot_grid(
      p_frac_overall  +
        theme(plot.margin = unit(c(2, 3, 0, 1), units = "lines"),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank()),
      p_frac_regions +
        theme(plot.margin = unit(c(.5, 3, 1, 1), units = "lines")), 
      labels = c("b", "c"),
      align = "v",
      axis = "lr",
      rel_heights = c(.4, 1),
      ncol = 1
    ),
    nrow = 1,
    rel_widths = c(1.2, 1),
    align = "h",
    axis = "tb",
    labels = c("a", NA_character_)
  ),
  plot_grid(
    p_ob_1 +
      guides(color = guide_legend("region")) +
      theme(legend.position = "right") +
      theme(plot.margin = unit(c(1, 3, 1, 1), units = "lines")), 
    p_ob_2 +
      guides(color = "none"),
    nrow = 1,
    rel_widths = c(.7, 1),
    align = "h",
    axis = "tb",
    labels = c("d", "e")
  ) +
    theme(plot.margin = unit(c(1, 3, 1, 3), units = "lines")),
  ncol = 1,
  rel_heights = c(1.5, 1),
  # labels = c(NA_character_, "e"),
  align = "h",
  axis = "lr"
) +
  theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave(plot = p_fig5,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_5.png"),
       width = 13,
       height = 12,
       dpi = 300)


# Supplementary figures ---------------------------------------------------


## Model fit ----
# Scatter plot of ADM0 level units
# Scatter plot of all admin units for mean of observations
p_data_scatter <- gen_obs %>%
  filter(censoring == "full") %>% 
  group_by(country, period, loctime_comb) %>% 
  mutate(mean_obs = mean(observation)) %>% 
  slice(1) %>% 
  ggplot(aes(x = mean_obs+1, y = mean+1)) +
  geom_abline(lty = 2, lwd = .5, col = "red") +
  geom_point(alpha = .5, aes(color = period)) +
  geom_errorbar(aes(ymin = q2.5+1, ymax = q97.5+1, color = period), alpha = .25) +
  facet_grid(country ~ admin_level) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Observed number of cases", y = "Modeled") +
  scale_color_manual(values = taxdat:::colors_periods())

ggsave(p_data_scatter,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_validation_scatter.png"),
       width = 15,
       height = 15, 
       dpi = 300)


# Scatter plot all censored admin units
p_data_scatter_censored <- gen_obs %>%
  filter(censoring == "right-censored") %>% 
  ggplot(aes(x = observation+1, y = mean+1)) +
  geom_abline(lty = 2, lwd = .5, col = "red") +
  geom_point(alpha = .5, aes(color = period)) +
  geom_errorbar(aes(ymin = q2.5+1, ymax = q97.5+1, color = period), alpha = .25) +
  facet_grid(country ~ admin_level) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Observed number of cases", y = "Modeled") +
  scale_color_manual(values = taxdat:::colors_periods())

ggsave(p_data_scatter_censored,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_validation_censored_scatter.png"),
       width = 15,
       height = 15, 
       dpi = 300)


# Coverage plot
p_coverage <- gen_obs %>%
  group_by(period, country, loctime_comb) %>% 
  mutate(observation = mean(observation)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(admin_level = str_extract(admin_level, "[0-9]") %>% as.numeric()) %>% 
  plot_posterior_coverage(with_period = TRUE) +
  facet_grid(country ~ period)

ggsave(p_coverage,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_validation_coverage.png"),
       width = 10,
       height = 15, 
       dpi = 300)

## Mean case tables by amdin level -----

# Save for admin levels 0 and 1
walk(c("ADM0", "ADM1"), function(x) {
  
  tbl <- make_adm_case_table(mai_adm_cases = mai_adm_cases, 
                             admin_levels = x)
  save_table_to_docx(tbl,
                     output_path =  str_glue("{opt$out_dir}/{opt$out_prefix}_cases_{x}.docx"))
})


## Recent outbreaks country-level estimates ----
p_country_coef <- param_by_country %>%
  filter(param != "(Intercept)") %>%
  ggplot(aes(x = mean, xmin = q2.5, xmax = q97.5, y = country)) +
  geom_vline(data = tibble(param = levels(param_by_country$param)[-1] %>%
                             factor(levels = levels(param_by_country$param)),
                           x = 0),
             aes(xintercept = x),
             lty = 2) +
  geom_errorbarh(height = 0, alpha = .7, aes(color = AFRO_region)) +
  geom_point(aes(color = AFRO_region), pch = 21, fill = "white") +
  facet_grid(AFRO_region~param, scales = "free_y", space = "free_y") +
  theme_bw() +
  scale_color_manual(values = colors_afro_regions()) +
  labs(x = "log-OR", color = NULL, fill = NULL)


ggsave(p_country_coef,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_country_outbreak_coef.png"),
       width = 10,
       height = 8,
       dpi = 300)

## Risk categories for 95% cutoff ----

p_risk_cat_95 <- risk_pop_95_adm2 %>% 
  select(-shp_id) %>% 
  filter(period == "2016-2020") %>% 
  inner_join(u_space_sf, .) %>% 
  output_plot_map(sf_obj = ., 
                  lakes_sf = lakes_sf,
                  rivers_sf = rivers_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "risk_cat",
                  fill_color_scale_type = "risk category") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.2, .3),
        panel.background = element_rect(fill = "white", color = "white"))+
  guides(fill = guide_legend("Risk category"))


# Save
ggsave(p_risk_cat_95,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_risk_categories_95.png"),
       width = 7,
       height = 6, 
       dpi = 300)

## 95% cutoff related figures ----
### Figure 3 supplement figures (95% cutoff) ----
p_fig3B_95 <- risk_pop_95_adm2 %>% 
  select(-shp_id) %>% 
  filter(period == "2016-2020") %>% 
  inner_join(u_space_sf, .) %>% 
  output_plot_map(sf_obj = ., 
                  lakes_sf = lakes_sf,
                  rivers_sf = rivers_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "risk_cat",
                  fill_color_scale_type = "risk category") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.2, .3),
        panel.background = element_rect(fill = "white", color = "white"))+
  guides(fill = guide_legend("Risk category"))

# Save
ggsave(p_fig3B_95,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3B_95.png"),
       width = 7,
       height = 6, 
       dpi = 300)

p_fig3_95 <- plot_grid(
  p_fig3A +
    theme(plot.margin = unit(c(2, 1, 2, 2), units = "lines"),
          legend.position = c(.75, .6)),
  p_fig3B_95 +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines")),
  # theme(plot.margin = unit(c(-5, -5, -5, -3), units = "lines")),
  nrow = 1,
  labels = "auto"#,
  # rel_widths = c(1, 1),
  # align = "v",
  # axis = "lr"
) +
  theme(panel.background = element_rect(fill = "white", color = "white"))

# Save
ggsave(plot = p_fig3_95,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3_95.png"),
       width = 12,
       height = 6,
       dpi = 600)

### Figure 4 supplement figures (95% cutoff) ----
p_fig4A_95 <- endemicity_df_95_v2 %>% 
  inner_join(u_space_sf, .) %>% 
  output_plot_map(sf_obj = .,
                  lakes_sf = lakes_sf,
                  rivers_sf = rivers_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "endemicity",
                  fill_color_scale_type = "endemicity",
                  border_width = .03) +
  theme(legend.position = c(.2, .3),
        panel.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend("10-year risk\ncategory"))

# Save
ggsave(p_fig4A_95,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_4A_risk_cat_95.png"),
       width = 12,
       height = 6, 
       dpi = 150)

p_fig4A_legend_95 <- ggdraw(
  p_fig4A_95 +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(1, 0, 1, 0), units = "lines")) +
    guides(fill = "none")
) +
  draw_plot(endemicity_legend, .075, .24, .35, .25)

p_fig4B_95 <- endemicity_df_95_v2  %>%
  group_by(country) %>% 
  complete(endemicity = unique(endemicity_df_95_v2$endemicity)) %>% 
  get_AFRO_region(ctry_col = "country") %>% 
  group_by(AFRO_region, country, endemicity) %>%
  summarise(n = sum(!is.na(location_period_id)),
            pop = sum(pop[!is.na(location_period_id)])) %>% 
  group_by(AFRO_region, country) %>%
  mutate(frac = pop/sum(pop)) %>% 
  group_by(country) %>% 
  mutate(
    frac_other = frac[endemicity == "history of moderate risk"],
    frac_high = sum(frac[endemicity %in% c("sustained high risk", "history of high risk")]),
    frac_low = sum(frac[endemicity %in% c("sustained low risk")])
  ) %>% 
  ungroup() %>% 
  mutate(endemicity = forcats::fct_rev(endemicity),
         country = factor(country, levels = unique(country[order(frac_high, frac_other)])),
  ) %>% 
  ggplot(aes(y = country, x = frac, fill = endemicity)) +
  geom_bar(stat = "identity") +
  facet_grid(AFRO_region ~., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = rev(taxdat:::colors_endemicity())) +
  theme_bw() +
  labs(x = "fraction of population\n per 10-year risk category")

ggsave(p_fig4B_95,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_4B_95.png"),
       width = 6,
       height = 7, 
       dpi = 150)

p_fig4_95 <- plot_grid(
  p_fig4A_legend_95 +
    theme(panel.background = element_rect(fill = "white", color = "white")),
  p_fig4B_95 +
    guides(fill = "none") +
    theme(panel.background = element_rect(fill = "white", color = "white"),
          plot.margin = unit(c(2, 1.5, 1.5, 1.5), units = "lines")),
  nrow = 1,
  labels = "auto",
  rel_widths = c(1, .5)
) +
  theme(panel.background = element_rect(fill = "white", color = "white"))

# Save
ggsave(plot = p_fig4_95,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_4_95.png"),
       width = 12,
       height = 7,
       dpi = 300)

### Figure 5 supplement figures (95% cutoff) ----
# Map of cholera occurrence locations
p_ob_map2_95 <- endemicity_df_v2_95 %>% 
  inner_join(u_space_sf, .) %>% 
  select(-admin_level) %>% 
  ggplot() +
  geom_sf(data = afr_sf %>% 
            select(-admin_level),
          inherit.aes = FALSE,
          lwd = 0.15,
          color = "darkgray",
          alpha = 0) +
  geom_sf(aes(fill = endemicity), alpha = .5, lwd = .005, color = "white") +
  geom_sf(inherit.aes = FALSE,
          data = final_joins,
          aes(color = "locations with\nreported cholera\nin 2022-2023"),
          alpha = 0,
          lwd = .35) +
  geom_sf(inherit.aes = FALSE,
          data = st_centroid(final_joins %>% select(-geom.y)),
          alpha = 1, col = "purple",
          fill = "white",
          pch = 21,
          size = .6,
          stroke = .2) +
  taxdat::map_theme() +
  scale_color_manual(values = c("purple")) +
  theme(panel.border = element_blank()) +
  theme(legend.position = c(.23, .4)) +
  scale_fill_manual(values = taxdat:::colors_endemicity()) +
  labs(color = NULL) +
  guides(fill = guide_legend("10-year risk\ncategory", override.aes = list(alpha = 1))) +
  scale_shape_manual(values = c(1, 3, 4))

ggsave(plot = p_ob_map2_95,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_5_a_map_95.png"),
       width = 12,
       height = 5,
       dpi = 100)

# reload outbreak analysis output with 95% cutoff
load(str_c(opt$output_dir, "/outbreak_analysis_data_95.rdata"))
load(str_c(opt$output_dir, "/recent_cholera_outbreaks_res_95.rdata"))

# Distribution of 10-year risk categories among ADM2 locations
ob_count_dat_95 <-  obs_outbreaks %>% 
  mutate(occurrence = "       cholera\n       observed") %>% 
  bind_rows(non_obs_outbreaks %>% 
              mutate(occurrence = "no cholera       \nobserved       ")) %>% 
  mutate(occurrence = factor(occurrence, 
                             levels = c("no cholera       \nobserved       ",
                                        "       cholera\n       observed"))) %>% 
  mutate(endemicity = factor(endemicity, 
                             levels = levels(endemicity_df_v2_95$endemicity)),
         AFRO_region = factor(AFRO_region %>% 
                                str_replace(" ", "\n"),
                              levels = rev(c("overall", get_AFRO_region_levels() %>% 
                                               str_replace(" ", "\n"))),
                              labels = rev(c("overall", get_AFRO_region_levels() %>% 
                                               str_replace(" ", "\n")))))

p_frac_regions_95 <- ob_count_dat_95 %>% 
  filter(AFRO_region != "overall")  %>%
  mutate(occurrence = str_remove_all(occurrence, "       ")) %>% 
  ggplot(aes(x = frac, y = AFRO_region, fill = endemicity)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = taxdat:::colors_endemicity()) +
  labs(x = "proportion of locations (ADM2 or lower)", y = "") +
  guides(fill = "none") +
  facet_grid(factor(occurrence, levels = c("cholera\nobserved","no cholera\nobserved")) ~ ., switch = "y") +
  theme(strip.placement = "outer")


p_frac_overall_95 <- ob_count_dat_95 %>% 
  mutate(occurrence = str_remove_all(occurrence, "  ")) %>% 
  filter(AFRO_region == "overall")  %>% 
  ggplot(aes(x = factor(occurrence, levels = c("no cholera \nobserved "," cholera\n observed")), y = frac, fill = endemicity)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = taxdat:::colors_endemicity()) +
  labs(y = "proportion of locations", x = "") +
  guides(fill = "none") +
  coord_flip()

p_ob_frac_comb_95 <- cowplot::plot_grid(
  p_frac_overall_95 +
    theme(plot.margin = unit(c(1, .3, 0, 1), units = "lines")) +
    theme(axis.text = element_text(size = 8, hjust = .5),
          axis.title = element_text(size = 10)),
  p_frac_regions_95 +
    theme(plot.margin = unit(c(1, 1, 0, 1), units = "lines"),
          strip.switch.pad.grid = unit(.7, units = "line")) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          strip.text = element_text(size = 8)), 
  nrow = 1,
  labels = c("b", "c"),
  rel_widths = c(.6, 1),
  align = "h",
  axis = "tb"
)


p_ob_1_95 <- baseline_prob_stats %>% 
  mutate(what = "reference",
         param = case_when(str_detect(param, "baseline") ~ "sustained low risk",
                           TRUE ~ param)) %>% 
  ggplot(aes(x = param, y = mean, ymin = q2.5, ymax = q97.5, color = AFRO_region)) +
  geom_point(position = pd1) +
  geom_errorbar(width = 0, position = pd1) +
  theme_bw() +
  facet_grid(. ~ what, scales = "free", space = "free") +
  scale_color_manual(values = c("overall" = "black", colors_afro_regions())) +
  labs(x = "", y = "probability of cholera occurrence") +
  guides(color = "none") +
  coord_cartesian(ylim = c(0, 1))  +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

p_ob_2_95 <- logOR_stats %>% 
  mutate(what = str_replace(what, "outbreak", "cholera")) %>% 
  ggplot(aes(x = param, y = mean, ymin = q2.5, ymax = q97.5, color = AFRO_region)) +
  geom_point(position = pd2) +
  geom_errorbar(width = 0, position = pd2) +
  geom_hline(aes(yintercept = 0), lty = 3, lwd = .6) +
  facet_grid(. ~ what, scales = "free", space = "free") +
  theme_bw() +
  scale_color_manual(values = c("overall" = "black", colors_afro_regions())) +
  labs(x = "10-year cholera risk category", y = "log-Odds ratio", color = NULL) +
  theme(legend.position = c(.145, .84),
        legend.key.height = unit(.75, units = "lines"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 8))



p_fig5_95 <- plot_grid(
  plot_grid(
    p_ob_map2_95, 
    plot_grid(
      p_frac_overall_95  +
        theme(plot.margin = unit(c(2, 3, 0, 1), units = "lines"),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank()),
      p_frac_regions_95 +
        theme(plot.margin = unit(c(.5, 3, 1, 1), units = "lines")), 
      labels = c("b", "c"),
      align = "v",
      axis = "lr",
      rel_heights = c(.4, 1),
      ncol = 1
    ),
    nrow = 1,
    rel_widths = c(1.2, 1),
    align = "h",
    axis = "tb",
    labels = c("a", NA_character_)
  ),
  plot_grid(
    p_ob_1_95 +
      guides(color = guide_legend("region")) +
      theme(legend.position = "right") +
      theme(plot.margin = unit(c(1, 3, 1, 1), units = "lines")), 
    p_ob_2_95 +
      guides(color = "none"),
    nrow = 1,
    rel_widths = c(.7, 1),
    align = "h",
    axis = "tb",
    labels = c("d", "e")
  ) +
    theme(plot.margin = unit(c(1, 3, 1, 3), units = "lines")),
  ncol = 1,
  rel_heights = c(1.5, 1),
  # labels = c(NA_character_, "e"),
  align = "h",
  axis = "lr"
) +
  theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave(plot = p_fig5_95,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_5_95.png"),
       width = 13,
       height = 12,
       dpi = 300)

# Scraps ------------------------------------------------------------------


# 
# ## Incidence rate ratios --
# 
# p_irr <- irr_dat %>%
#   filter(!is.na(dist_definition)) %>%
#   mutate(dist = factor(dist),
#          dist = fct_reorder(dist, mean_dist)) %>%
#   ggplot(aes(x = dist, y = ratio, color = AFRO_region)) +
#   geom_point(alpha = 1) +
#   geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, alpha = .5) +
#   geom_hline(aes(yintercept = 1), lty = 2, lwd = .5) +
#   theme_bw() +
#   facet_grid(what ~ period + dist_definition, scales =  "free_x") +
#   labs(x = "distance to waterbody [km]", y = "IRR [observed/null]") +
#   coord_cartesian(ylim = c(0, 15)) +
#   theme(axis.text.x = element_text(angle = 45, hjust=  1, vjust = 1)) +
#   scale_color_manual(values = colors_afro_regions())
# 
# 
# ggsave(p_irr,
#        file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_irr_dist.png"),
#        width = 12,
#        height = 8,
#        dpi = 300)
# 
# saveRDS(irr_dat, file = str_glue("{opt$output_dir}/irr_dist.rds"))

## Fig. 4B: Change in risk categories barplot 
# 
# p_fig4B <- endemicity_df_v2 %>% 
#   ungroup() %>% 
#   get_AFRO_region(ctry_col = "country") %>% 
#   mutate(AFRO_region = factor(AFRO_region, 
#                               levels = get_AFRO_region_levels())) %>% 
#   group_by(AFRO_region, endemicity) %>% 
#   summarise(pop = sum(pop)) %>% 
#   ggplot(aes(y = endemicity, x = pop, fill = AFRO_region)) +
#   geom_bar(stat = "identity", width = .5) +
#   theme_bw() +
#   scale_fill_manual(values = colors_afro_regions()) +
#   scale_x_continuous(labels = function(x) {formatC(x/1e6)}) +
#   scale_y_discrete(limits = rev) +
#   labs(y = "Change in risk category", x = "ADM2 population at risk [millions]")
# 
# 
# # Save
# ggsave(plot = p_fig4B,
#        filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_4B.png"),
#        width = 12,
#        height = 7,
#        dpi = 300)
#         
# ## Scatterplots of MAI against covariates 
# 
# ### Distance to water --
# adm2_sf <- u_space_sf %>% 
#   filter(admin_level == "ADM2") %>% 
#   st_make_valid()
# 
# adm2_centroids_sf <- st_centroid(adm2_sf)
# 
# dist_to_wb <- map_df(seq_along(dist_sf), function(y) {
#   
#   cat("--", names(dist_sf)[y], "\n")
#   
#   # Get index of nearest object
#   nearest_ind <- adm2_centroids_sf %>% 
#     st_nearest_feature(dist_sf[[y]], check_crs = TRUE)
#   
#   # Compute distances
#   distances <- st_distance(adm2_centroids_sf, dist_sf[[y]][nearest_ind, ], by_element = TRUE) %>% 
#     as.numeric()
#   
#   tibble(
#     location_period_id = adm2_centroids_sf$location_period_id,
#     country = adm2_centroids_sf$country,
#     dist = distances/1e3,    # in km
#     to_what = names(dist_sf)[y]
#   )
# })
# 
# p_dist <- mai_adm_all %>% 
#   filter(admin_level == "ADM2") %>% 
#   inner_join(dist_to_wb) %>% 
#   ggplot(aes(x = dist, y = mean, color = period)) +
#   # geom_hex() +
#   geom_point(alpha = .15) +
#   geom_smooth() +
#   facet_grid(.~to_what, scales = "free_x") +
#   scale_y_log10() +
#   theme_bw() +
#   scale_color_manual(values = taxdat:::colors_periods()) +
#   labs(x = "distance [km]", y = "mean annula incidence rate")
# 
# 
# ggsave(p_dist,
#        file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_scatterplot_mai_dist_water.png"),
#        width = 12,
#        height = 5, 
#        dpi = 300)
# 
# saveRDS(dist_to_wb, file = str_glue("{opt$output_dir}/dist_to_wb.rds"))
# 
# 
# ### Populatio Denstity --
# 
# adm2_sf <- sf::st_make_valid(adm2_sf)
# # Compute density
# pop_density <- adm2_sf %>% 
#   mutate(area = st_area(geom) %>% 
#            as.numeric() %>% 
#            {./1e6}    # in sqkm
#   ) %>% 
#   st_drop_geometry() %>% 
#   inner_join(population %>% 
#                select(location_period_id, pop = mean)) %>% 
#   mutate(pop_density = pop/area)    # in pop/sqkm
# 
# p_density <- mai_adm %>% 
#   filter(admin_level == "ADM2") %>% 
#   inner_join(pop_density) %>% 
#   ggplot(aes(x = pop_density, y = mean, color = period)) +
#   geom_point(alpha = .15) +
#   geom_smooth() +
#   scale_y_log10() +
#   scale_x_log10() +
#   theme_bw() +
#   scale_color_manual(values = taxdat:::colors_periods()) +
#   labs(x = "population density [peopl/sqkm]", y = "mean annula incidence rate")
# 
# ggsave(p_density,
#        file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_scatterplot_mai_pop_density.png"),
#        width = 12,
#        height = 8, 
#        dpi = 300)
# 
# saveRDS(pop_density, file = str_glue("{opt$output_dir}/pop_density.rds"))
# 
# ### WASH --
# 
# # Try reading in th wash data, this should be stored in a better place in the future
# wash_dat <- st_read("Analysis/output/adm2_sf_wash_prop_clean.gpkg")
# 
# p_wash <- wash_dat %>% 
#   st_drop_geometry() %>% 
#   as_tibble() %>% 
#   select(location_period_id, contains("prop")) %>% 
#   pivot_longer(cols = contains("prop")) %>% 
#   mutate(period = case_when(str_detect(name, "2012") ~ "2011-2015",
#                             T ~ "2016-2020"),
#          what = str_remove(name, "prop_Y2012_|prop_Y2017_"),
#          category = str_extract(what, "W|S")) %>% 
#   select(-name) %>% 
#   inner_join(mai_adm) %>% 
#   ggplot(aes(x = value, y = mean)) +
#   geom_point(aes(color = period), alpha = .3) +
#   geom_smooth(aes(color = period)) +
#   geom_smooth(color = "black") +
#   facet_wrap(~what) +
#   scale_y_log10() +
#   theme_bw() +
#   scale_color_manual(values = taxdat:::colors_periods()) +
#   labs(x = "Proportion of population", y = "mean annula incidence rate")
# 
# 
# ggsave(p_wash,
#        file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_scatterplot_mai_WASH.png"),
#        width = 12,
#        height = 8, 
#        dpi = 300)
# 
# 

# p_fig5 <- cowplot::plot_grid(
#   p_ob_map2 +
#     theme(plot.margin = unit(c(0, .5, -1, .5), units = "lines")),
#   p_fig5_bcd,
#   nrow = 1,
#   rel_widths = c(1, 1),
#   align = "h",
#   axis = "tb",
#   labels = c("a", NA_character_)
# ) +
#   theme(plot.background = element_rect(fill = "white", color = "white"))
# 
# 
# # Save
# ggsave(plot = p_fig5,
#        filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_5.png"),
#        width = 13,
#        height = 7,
#        dpi = 300)


# p_fig5_bcd <- cowplot::plot_grid(
#   p_ob_frac_comb,
#   cowplot::plot_grid(
#     p_ob_1, 
#     p_ob_2,
#     nrow = 1,
#     rel_widths = c(.3, 1),
#     align = "v",
#     axis = "tb"
#   ) +
#     theme(plot.margin = unit(c(.5, .5, .5, .5), units = "lines")),
#   ncol = 1,
#   rel_heights = c(.95, 1),
#   labels = c(NA, "d"),
#   align = "h",
#   axis = "lr"
# ) +
#   theme(panel.background = element_rect(fill = "white", color = "white"))

# p_frac_overall <- ob_count_dat %>% 
#   filter(AFRO_region == "overall")  %>% 
#   ggplot(aes(x = occurrence, y = frac, fill = endemicity)) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   scale_fill_manual(values = taxdat:::colors_endemicity()) +
#   labs(y = "proportion of locations (ADM2 or lower)", x = "") +
#   guides(fill = "none")
