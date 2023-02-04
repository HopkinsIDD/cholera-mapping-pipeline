## Set up
library(dplyr)
library(ggplot2)

## Copied for loop
# Where the data folder is
data_dir <- "/.../data"

# Find and load the model output 
fns <- list.files(data_dir, full.names = FALSE, recursive=FALSE)
country_list <- unique(stringr::str_extract(fns, "^[A-Z]{3}"))
country_list <- country_list[country_list != "ZAF" & !is.na(country_list)] #delete ZAF
country_list <- c(country_list, "Tanzania", "Zanzibar")
fns_filtered <- fns[grepl("stan_input.rdata$", fns) | grepl("preprocess.rdata$", fns)]

# Loop through all the files
pre_table <- NULL
stan_table <- NULL

for(country_code in country_list){
    load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)], sep = "/")[1])
    load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)], sep = "/")[2])

    preprocessed <- sf_cases %>%
        sf::st_drop_geometry() %>%
        select(OC_UID, locationPeriod_id, TL, TR, suspected_cases) %>% 
        mutate(obs_span = case_when(
            lubridate::year(TL) != lubridate::year(TR) ~ "multi-year", 
            lubridate::year(TL) == lubridate::year(TR) ~ "single-year"
            ), 
            country = country_code
        )

    stan <- stan_input$sf_cases_resized %>%
        sf::st_drop_geometry() %>%
        mutate(censoring = stan_input$stan_data$censoring_inds) %>%
        rename(suspected_cases = attributes.fields.suspected_cases) %>%
        select(OC_UID, locationPeriod_id, TL, TR, suspected_cases, censoring) %>% 
        mutate(obs_span = case_when(
            lubridate::year(TL) != lubridate::year(TR) ~ "multi-year", 
            lubridate::year(TL) == lubridate::year(TR) ~ "single-year"
            ), 
            country = country_code
        )

    if(is.null(pre_table)){pre_table <- preprocessed}else{pre_table <- rbind(pre_table, preprocessed)}
    if(is.null(stan_table)){stan_table <- stan}else{stan_table <- rbind(stan_table, stan)}
}

## Plotting
plt_p_c <- pre_table %>%
    ggplot(aes(x=obs_span, fill=obs_span)) + 
    geom_bar() + 
    theme_minimal() + 
    geom_text(stat='count', aes(label=..count..), vjust=+1) + 
    facet_wrap(~country, scales = "free", ncol = 5) + 
    ggtitle("Pre-processed Data")

plt_p_p <- pre_table %>%
    group_by(country) %>%
    dplyr::group_modify(function(.x,.y){
        .x %>%
            group_by(obs_span) %>%
            summarize(Proportion = n() / nrow(.x))
    }) %>%
    ggplot(aes(x=country, y=Proportion, fill=obs_span)) + 
    geom_bar(position="fill", stat="identity") + 
    theme_minimal() + 
    geom_text(aes(label = paste0(Proportion*100,"%")), 
            position = position_stack(vjust = 0.5), size = 2) + 
    facet_wrap(~country, scales = "free", ncol = 5) + 
    ggtitle("Pre-processed Data")


plt_s_c <- stan_table %>%
    ggplot(aes(x=obs_span, fill=obs_span)) + 
    geom_bar() + 
    theme_minimal() + 
    geom_text(stat='count', aes(label=..count..), vjust=+1) + 
    facet_wrap(~country, scales = "free", ncol = 5) + 
    ggtitle("Stan Input")

plt_s_p <- stan_table %>%
    group_by(country) %>%
    dplyr::group_modify(function(.x,.y){
        .x %>%
            group_by(obs_span) %>%
            summarize(Proportion = n() / nrow(.x))
    }) %>%
    ggplot(aes(x=country, y=Proportion, fill=obs_span)) + 
    geom_bar(position="fill", stat="identity") + 
    theme_minimal() + 
    geom_text(aes(label = paste0(Proportion*100,"%")), 
            position = position_stack(vjust = 0.5), size = 2) + 
    facet_wrap(~country, scales = "free", ncol = 5) + 
    ggtitle("Stan Input")


plt_sc_c <- stan_table %>%
    group_by(country, censoring, obs_span) %>%
    summarize(Counts = n()) %>%
    ggplot(aes(fill=obs_span, y=Counts, x=censoring)) + 
    geom_bar(position="dodge", stat="identity") +
    theme_minimal() + 
    geom_text(aes(label = paste0(Counts)), 
            position = position_dodge(1), vjust = - 0.5, size = 2) + 
    facet_wrap(~country, scales = "free", ncol = 5) + 
    ggtitle("Stan Input")

plt_sc_p <- stan_table %>%
    group_by(country, censoring) %>%
    dplyr::group_modify(function(.x,.y){
        .x %>%
            group_by(obs_span) %>%
            summarize(Proportion = n() / nrow(.x))
    }) %>%
    ggplot(aes(x=censoring, y=Proportion, fill=obs_span)) + 
    geom_bar(position="fill", stat="identity") + 
    theme_minimal() + 
    geom_text(aes(label = paste0(Proportion*100,"%")), 
            position = position_stack(vjust = 0.5), size = 2) + 
    facet_wrap(~country, scales = "free", ncol = 5) + 
    ggtitle("Stan Input")

pdf("Analysis/output/multi-year_plt.pdf", width = 5, height = 7.5)
plt_p_c
plt_p_p
plt_s_c
plt_s_p
plt_sc_c
plt_sc_p
dev.off()


