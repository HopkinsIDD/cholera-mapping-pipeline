library(tidyverse)
library(sf)
library(optparse)
library(taxdat)

# User-supplied options
opt_list <- list(
  make_option(opt_str = c("-f", "--bundle_filename"), type = "character",
              default = "data_bundle_for_figures_test.rdata", 
              help = "Data bundle to avoid re-processing")
)

opt <- parse_args(OptionParser(option_list = opt_list))

load(opt$bundle_filename)

risk_pop_all_1115 <- readRDS("2011_2015_pop_at_risk.rds") %>% 
  filter(admin_level == "ADM2" | (admin_level == "ADM1" & country == "LSO")) %>% 
  select(country,risk_cat, mean, q2.5, q97.5) %>%
  mutate(time_period = '2011-2015')

risk_pop_all_1620 <- readRDS("2016_2020_pop_at_risk.rds") %>% 
  filter(admin_level == "ADM2" | (admin_level == "ADM1" & country == "LSO")) %>% 
  select(country,risk_cat, mean, q2.5, q97.5) %>%
  mutate(time_period = '2016-2020')

risk_pop_all <- bind_rows(risk_pop_all_1620,risk_pop_all_1115)

pop <- readRDS("2011_2015_population.rds") %>% 
  subset(admin_level == "ADM0") %>% 
  mutate(pop = mean,time_period = "2011-2015") %>% 
  select(shapeName,country,pop,time_period) %>% 
  rbind(
    readRDS("2016_2020_population.rds") %>% 
      subset(admin_level == "ADM0") %>% 
      mutate(pop = mean,time_period = "2016-2020") %>% 
      select(shapeName,country,pop,time_period)
  ) %>% 
  select(!pop)

table_5year <- 
risk_pop_all %>% 
  inner_join(pop,by=c("country","time_period")) %>% 
  group_by(country,time_period) %>% 
  mutate(pop = sum(mean)) %>% 
  ungroup()%>% 
  select(!country) %>% 
  arrange(factor(risk_cat,levels=c(">100","50-100","20-50","10-20","1-10","<1"))) %>% 
  mutate(
    people_mn = round(mean/10^6,1),
    people_lb = round(q2.5/10^6,1),
    people_ub = round(q97.5/10^6,1),
    proportion = round(mean/pop,2)
  ) %>% 
  rename(
    period = time_period,
    country = shapeName,
    incidence_category = risk_cat
  ) %>% 
  select(
    country,
    period,
    incidence_category,
    people_mn,
    people_lb,
    people_ub,
    proportion
  )

table_10year <-
endemicity_df_v2 %>% 
  group_by(country,endemicity) %>%
  summarize(mean= sum(pop),time_period = "2011-2020") %>% 
  ungroup() %>%
  group_by(country,time_period) %>% 
  mutate(pop = sum(mean)) %>% 
  ungroup() %>% 
  inner_join(pop %>% subset(time_period == "2016-2020") %>% select(!time_period),by=c("country")) %>% 
  select(!country) %>% 
  rename(country = shapeName,incidence_category = endemicity) %>% 
  mutate(
    people_mn = round(mean/10^6,1),
    people_lb = NA,
    people_ub = NA,
    proportion = round(mean/pop,2)
  )  %>% 
  rename(
    period = time_period
  ) %>% 
  select(
    country,
    period,
    incidence_category,
    people_mn,
    people_lb,
    people_ub,
    proportion
  )

table <- rbind(table_5year,table_10year)%>% 
  mutate(incidence_category = as.character(incidence_category)) %>% 
  mutate(
    incidence_category = ifelse(
      incidence_category == "<1", "<1 per 100K",
      ifelse(
        incidence_category == "1-10", "1-10 per 100K",
        ifelse(
          incidence_category == "10-20", "10-20 per 100K",
          ifelse(
            incidence_category == "20-50", "20-50 per 100K",
            ifelse(incidence_category == "50-100", "50-100 per 100K",
                   ifelse(
                     incidence_category == ">100", ">100 per 100K", 
                     incidence_category
                   ))
          )
        )
      )
    )
  )

write.csv(table,"pop_incidence_category_GAVI.csv",row.names = F)