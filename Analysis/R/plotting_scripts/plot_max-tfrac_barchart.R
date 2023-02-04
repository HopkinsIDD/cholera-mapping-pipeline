#=============================================================================================
# This is the script used to draw barplots of max tfrac for each country-year
# 1/2/2023
# 
#=============================================================================================

# Load the packages
library(dplyr)
library(ggplot2)

# Where the data folder is
data_dir <- "/home/.../data"

# Set parameter
start_year <- 2016

# Find and load the model output 
fns <- list.files(data_dir, full.names = FALSE, recursive=FALSE)
country_list <- unique(stringr::str_extract(fns, "^[A-Z]{3}"))
# country_list <- country_list[country_list != "ZAF" & !is.na(country_list)] #delete ZAF
# country_list <- c("COD", "ETH", "NGA")
# country_list <- c(country_list, "Tanzania", "Zanzibar") #add TZA_mainland and remove NA
country_list <- country_list[!is.na(country_list)]
if(start_year == 2016){country_list <- country_list[country_list != "GMB"]}
fns_filtered <- fns[grepl("stan_input.rdata$", fns)]
fns_filtered <- fns_filtered[grepl(as.character(start_year), fns_filtered)]

# Loop through all the files
tfrac_table <- tibble::tibble(iso = as.character(),  year = as.numeric(), max_tfrac = as.numeric(), zero_case = as.logical())
NA_zero_table <- tibble::tibble(iso = as.character(),  year = as.numeric(), category = as.character())

for(country_code in country_list){
  load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)], sep = "/"))
  
  tmp <- stan_input$sf_cases_resized %>%
    sf::st_drop_geometry() %>%
    mutate(idx = 1:n(), nyears = lubridate::year(TR) - lubridate::year(TL) + 1)
  tmp <- as.data.frame(lapply(tmp, rep, tmp$nyears)) 
  tmp <- tmp %>%
    group_by(idx) %>%
    mutate(year = lubridate::year(TL) + (1:n()) - 1) %>%
    rename(suspected_cases = attributes.fields.suspected_cases)
  tmp$tfrac <- stan_input$stan_data$tfrac

  tfrac_table_tmp <- tmp %>%
    group_by(year) %>%
    summarise(max_tfrac = max(tfrac), iso = country_code, zero_case = (sum(suspected_cases) == 0))

  # the table
  for(i in (start_year):(start_year+4)){
    if(nrow(tfrac_table_tmp[tfrac_table_tmp$year == i & tfrac_table_tmp$iso == country_code, ]) == 0){
      NA_zero_table <- NA_zero_table %>%
        add_row(iso = country_code, year = i, category = "NA-tfrac")
    }else if(tfrac_table_tmp[tfrac_table_tmp$year == i & tfrac_table_tmp$iso == country_code, ]$max_tfrac == 0){
      NA_zero_table <- NA_zero_table %>%
        add_row(iso = country_code, year = i, category = "zero-tfrac")
    }
  }
  

  if(nrow(tfrac_table_tmp) < 5){
    empty_year <- ((start_year):(start_year+4))[!((start_year):(start_year+4)) %in% tfrac_table_tmp$year]
    tfrac_table_tmp <- tfrac_table_tmp %>% add_row(iso = country_code, year = empty_year, max_tfrac = 0, zero_case = FALSE)
  }

  tfrac_table <- rbind(tfrac_table, tfrac_table_tmp)
}

# Plotting
plt <- tfrac_table %>%
  ggplot(aes(x=year, y=max_tfrac)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(aes(label = ifelse(zero_case, as.character("zero-case"), "")), fontface = "bold", size = 1.0) + 
  theme_minimal() + 
  facet_wrap( ~ iso, scales = "free", ncol = 5) + 
  scale_x_continuous(breaks=seq(start_year,start_year+4,1)) + 
  scale_y_continuous(limits=c(0, 1)) + 
  theme_minimal()
fig_height <- length(country_list) 
pdf(paste0("/home/kaiyuezou/mapping_pipeline/2016_2020_no_covariate_run/cholera-mapping-pipeline/Analysis/output/", 
          as.character(start_year), "-", as.character(start_year+4), "_max_tfrac_barplot.pdf"), 
    height = fig_height/2, width = 10)
plt
dev.off()
readr::write_csv(NA_zero_table, paste0("/home/kaiyuezou/mapping_pipeline/2016_2020_no_covariate_run/cholera-mapping-pipeline/Analysis/output/", 
                                      as.character(start_year), "-", as.character(start_year+4), "_max_tfrac_NA_zero_table.csv"))


