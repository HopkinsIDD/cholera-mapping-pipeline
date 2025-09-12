# function to convert the values into std versions in age-related columns ----
manual_fix_age <- function(x, max_age_years = 100) {
  
  library(dplyr)
  library(stringr)
  
  raw <- x
  
  out <- case_when(
    raw %in% c("", "No data", "no data", "NA", "N/A", "s", ";") ~ NA_real_,
    
    raw == "5O" ~ 50,
    raw == "60Â " ~ 60,
    raw == "40Â " ~ 40,
    raw == "Â 20" ~ 20,
    raw == "Â 50" ~ 50,
    raw == "Â 68" ~ 68,
    raw == "Â 55" ~ 55,
    raw == "Â 20Â " ~ 20,
    
    # fractions that mean years
    raw == "16/12" ~ 16/12,
    raw == "13/12" ~ 13/12,
    raw == "2 1/2," ~ 2.5,
    
    # months (convert to years)
    str_detect(tolower(gsub(" ", "", raw)), "[0-9]+m") ~ as.numeric(str_extract(raw, "[0-9]+"))/12
    
    raw == "1 year & 8 months" ~ 1 + 8/12,
    
    str_detect(raw, regex("(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)", ignore_case = TRUE)) ~ NA_real_,
    
    TRUE ~ suppressWarnings(as.numeric(
      str_replace_all(raw, fixed(","), ".")
    ))
  )
  
  out[!is.na(out) & (out < 0 | out > max_age_years)] <- NA_real_
  
  out
}

# function to clean age columns ----
clean_age_cols <- function(data, max_age_years = 100) {
  
 dplyr::mutate(
      data,
      dplyr::across(
        .cols = dplyr::any_of(c("age", "age_l", "age_r")),
        .fns  = ~ manual_fix_age(.x, max_age_years = max_age_years)
      )
    ) %>% 
    dplyr::filter(dplyr::if_any(dplyr::all_of(c("age", "age_l", "age_r")), ~ !is.na(.x)))

}
# functions to standardize age groups ----
standardize_age_group <- function(data, age_splitter = c(5,14,50,70)){
  
  library(dplyr)
  
  data %>% 
    mutate(
      std_age = case_when(
        is.na(age) & !is.na(age_l) & !is.na(age_r) ~ (age_l+age_r)/2, 
        is.na(age) & !is.na(age_l) & is.na(age_r) ~ (age_l+100)/2,
        is.na(age) & is.na(age_l) & !is.na(age_r) ~ (age_r+0)/2,
        TRUE ~ age
      ),
      
      age_group = cut(
        std_age,
        breaks = c(-Inf, age_splitter, Inf),
        labels = c(
          paste0("0-", age_splitter[1]),
          paste0((age_splitter[-length(age_splitter)] + 1), "-", age_splitter[-1]),
          paste0(age_splitter[length(age_splitter)] + 1, "+")
        ),
        right = TRUE
      )
    )
}
# functions to clean tr and tl time format
parse_date <- function(x) {
  out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  out2 <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
  ifelse(is.na(out), out2, out)
}

# functions to summarize the observations by age groups
summary_obs_table <- function(clean_data){
  library(gt)
  library(lubridate)
  
  clean_data %>% 
    OutbreakExtractR::clean_location_names() %>% 
    mutate(
      spatial_scale = dplyr::case_when(
        stringr::str_count(location, pattern = "::") == 1 ~ "country",
        stringr::str_count(location, pattern = "::") == 2 ~ "admin1",
        stringr::str_count(location, pattern = "::") == 3 ~ "admin2",
        stringr::str_count(location, pattern = "::") == 4 ~ "admin3",
        stringr::str_count(location, pattern = "::") >= 5 ~ "admin4 or lower"),
      public = case_when(
        public == 'f' ~ 0,
        TRUE ~ 1
      )
    ) %>% 
    group_by(age_group) %>% 
    summarize(
      minTL = min(tl),
      maxTR = max(tr),
      num_years = length(unique(c(lubridate::year(tl),lubridate::year(tr)))),
      num_obs = n(),
      num_continent = n_distinct(who_region),
      num_country = n_distinct(country),
      num_loc = n_distinct(location),
      prop_public = round(100*mean(public, na.rm = T),1)
    ) %>% 
    gt() %>% 
    cols_label(
      minTL = 'Earliest TL',
      maxTR = 'Latest TR',
      num_years =  'Years covered',
      num_obs = 'Observations',
      num_continent = 'Continents',
      num_country = 'Countries',
      num_loc = 'Locations',
      prop_public = '% of public obs'
    )
}

# functions to summarize the observations by country for each age group
tables_by_age_group <- function(clean_data) {
  library(dplyr)
  library(lubridate)
  library(gt)
  library(purrr)
  library(scales)

  by_country <- clean_data %>%
    OutbreakExtractR::clean_location_names() %>% 
    mutate(
      public = case_when(
        public == 'f' ~ 0,
        TRUE ~ 1
      )
    ) %>% 
    mutate(
      tl = as.Date(tl),
      tr = as.Date(tr)
    ) %>%
    group_by(age_group, country) %>%
    summarise(
      who_region = unique(who_region),
      minTL = min(tl),
      maxTR = max(tr),
      num_observations = n(),
      num_locations = n_distinct(location),
      num_years = length(unique(c(lubridate::year(tl),lubridate::year(tr)))),
      prop_public = round(100*mean(public, na.rm = T),1),
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    arrange(who_region,age_group, country)
  
  # assign color scales
  region_levels <- unique(by_country$who_region)
  pal_fn <- scales::col_factor(palette = scales::hue_pal()(max(3, length(region_levels))),
                               domain  = region_levels,
                               na.color = "transparent")
  
  grp <- by_country %>% group_by(age_group)
  split_list <- group_split(grp)
  keys <- group_keys(grp) %>% dplyr::pull(age_group)
  names(split_list) <- as.character(keys)
  
  gt_tables <- imap(split_list, function(df_group, key) {
    gt(df_group %>% select(who_region, country, minTL, maxTR, num_observations, num_locations, num_years,prop_public)) %>%
      cols_label(
        country = "Country",
        minTL = "TL",
        maxTR = "TR",
        num_observations = "Observations",
        num_locations = "Locations",
        num_years = "Years covered",
        prop_public = '% of public obs'
      ) %>% 
    data_color(
    columns = who_region,
    colors = pal_fn,
    apply_to = "fill",
    autocolor_text = TRUE
  ) %>% 
      cols_move_to_start(c(who_region, country)) %>%
      tab_header(title = paste0("Summary by Country — Age Group ", key))
})
  
  gt_tables
}

# functions to aggregate sCh, cCh, and deaths (scd columns)
summarize_scd <- function(clean_data, keep_age_group = FALSE) {
  library(dplyr)
  library(lubridate)
  library(stringr)
  
  df <- clean_data %>%
    OutbreakExtractR::clean_location_names() %>% 
    mutate(
      tl = as.Date(tl),
      tr = as.Date(tr),
      year = year(tl), # year is from TL
      sCh = suppressWarnings(as.numeric(sch)),
      cCh = suppressWarnings(as.numeric(cch)),
      deaths = suppressWarnings(as.numeric(deaths)),
      spatial_scale = case_when(
        str_count(location, "::") == 1 ~ "country",
        str_count(location, "::") == 2 ~ "admin1",
        str_count(location, "::") == 3 ~ "admin2",
        str_count(location, "::") == 4 ~ "admin3",
        str_count(location, "::") >= 5 ~ "admin4 or lower",
        TRUE ~ "unknown"
      )
    )
  
  # --- Step 1: Per location, age_group and year, merge overlapping intervals into blocks -----------
  blocks <- df %>%
    arrange(location, age_group, year, tl, tr) %>%
    group_by(location, age_group, year) %>%
    mutate(
      tl_num = as.numeric(tl),
      tr_num = as.numeric(tr),
      prev_cummax_tr = dplyr::lag(cummax(tr_num), default = -Inf),
      
      new_block = tl_num < prev_cummax_tr,
  
      block_id = cumsum(new_block) + 1L
    ) %>%
    group_by(location, age_group, year, block_id, country, spatial_scale) %>%
    summarise(
      block_TL = min(tl, na.rm = TRUE),
      block_TR = max(tr, na.rm = TRUE),
      block_duration = as.integer(block_TR - block_TL) + 1L,
      sCh = sum(sCh, na.rm = TRUE),
      cCh = sum(cCh, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # --- Step 2: choose the longest block per (location, age_group, year) ----
  chosen <- blocks %>%
    group_by(location, age_group, year) %>%
    arrange(desc(block_duration), desc(sCh + cCh + deaths), block_TL) %>%
    slice(1L) %>%
    ungroup()
  
  # --- Step 3: aggregate to country x spatial_scale x year x age group ----------------
  final <- chosen %>%
    group_by(country, age_group, spatial_scale, year) %>%
    summarise(
      sCh_total = sum(sCh, na.rm = TRUE),
      cCh_total = sum(cCh, na.rm = TRUE),
      deaths_total= sum(deaths, na.rm = TRUE),
      .groups = "drop"
    )
  
  final
}

# function to make heatmaps by spatial scales over time by country ----
make_heatmaps_by_age_scale <- function(
    df,
    metric = c("sCh", "cCh", "deaths"),
    viridis_option = "C"
) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(scales)
  library(rlang)

  candidate_cols <- c(metric, paste0(metric, "_total"))
  val_col <- candidate_cols[candidate_cols %in% names(df)][1]
  val_sym <- sym(val_col)
  
  grp <- df %>% group_by(age_group, spatial_scale)
  pieces <- group_split(grp)
  keys <- group_keys(grp)
  
  plots <- imap(pieces, function(piece, i) {
    ag <- keys$age_group[i]
    sc <- keys$spatial_scale[i]
    
    piece <- piece %>%
      select(country, year, !!val_sym) %>%
      mutate(
        year = as.integer(year),
        country = as.character(country)
      )
    
    ord <- piece %>%
      group_by(country) %>%
      summarise(tot = sum(!!val_sym, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(tot), country) %>%
      pull(country)

    piece <- piece %>%
      mutate(
        country = factor(country, levels = rev(ord)),
        year = factor(year)
      )

    fill_scale <- scale_fill_viridis_c(option = viridis_option, trans = "log10", na.value = "grey95")
    
    ggplot(piece, aes(x = year, y = country, fill = !!val_sym)) +
      geom_tile() +
      fill_scale +
      labs(
        title = paste0("Heatmap of ", metric, " — age group:", ag, " • ", sc),
        x = "Year", y = "",
        fill = metric
      ) +
      theme_bw(base_size = 12) +
      theme(
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1)
      )
  })
  
  names(plots) <- paste(keys$age_group,keys$spatial_scale, sep = " | ")
  plots
}

