library(tidyverse)
library(sf)
library(optparse)

# User-supplied options
opt_list <- list(
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "cholera-mapping-pipeline/Analysis/output/processed_outputs/", help = "Output directory") # the directory where the processed output are saved
)

opt <- parse_args(OptionParser(option_list = opt_list))

# Figure to compare 2011-2015 and 2016-2020 periods
sf::sf_use_s2(FALSE)
mai_adm2_1115 <- readRDS(paste0(opt$output_dir,"/2011_2015_mai.rds")) %>%
  filter(!country %in% c("AGO","MLI","MOZ","ETH","KEN","NGA","SDN","SSD","GMB")) %>% #filter out countries which we haven't decided the runs for the two time periods.
  select(country,shapeName,mean) %>% 
  mutate(time_period="2011-2015") 
head(mai_adm2_1115)

mai_adm2_1620 <- readRDS(paste0(opt$output_dir,"/2016_2020_mai.rds")) %>% 
  filter(!country %in% c("AGO","MLI","MOZ","ETH","KEN","NGA","SDN","SSD","GMB")) %>% #filter out countries which we haven't decided the runs for the two time periods.
  select(country,shapeName,mean) %>% 
  mutate(time_period="2016-2020") 

mai_sum <- mai %>% 
  mutate(
    mean_cat =ifelse(mean*1e5<= 1e5 * 0.0000001, "<=0.01 per 100,000",
                     ifelse(mean*1e5<= 1e5 * 0.0000005, "<=0.05 per 100,000",
                            ifelse(mean*1e5<= 1e5 * 0.000001, "<=0.1 per 100,000",
                                   ifelse(mean*1e5<= 1e5 * 0.0000015, "<=0.15 per 100,000",
                                          ifelse(mean*1e5<= 1e5 * 0.00001, "<=1 per 100,000",
                                                 ifelse(mean*1e5<= 1e5 * 0.000015, "<=1.5 per 100,000",
                                                        ifelse(mean*1e5<= 1e5 * 0.0001, "<=10 per 100,000",
                                                               ifelse(mean*1e5<= 1e5 * 0.00015, "<=15 per 100,000",
                                                                      ifelse(mean*1e5<= 1e5 * 0.001, "<=100 per 100,000",
                                                                             ifelse(mean*1e5<= 1e5 * 0.0015, "<=150 per 100,000",
                                                                                    ifelse(mean*1e5<= 1e5 * 0.01, "<=1,000 per 100,000",
                                                                                           ifelse(mean*1e5<= 1e5 * 0.015, "<=1,500 per 100,000",
                                                                                                  ">1,500 per 100,000"))))))))))))
  ) %>% 
  mutate(
    mean_cat = factor(mean_cat,levels=c("0.05 per 100,000",
                                        "0.1 per 100,000",
                                        "0.15 per 100,000",
                                        "1 per 100,000",
                                        "1.5 per 100,000",
                                        "10 per 100,000",
                                        "15 per 100,000",
                                        "100 per 100,000",
                                        "150 per 100,000",
                                        "1,000 per 100,000",
                                        "1,500 per 100,000",
                                        ">1,500 per 100,000"))
  ) %>% 
  group_by(mean_cat,time_period) %>% 
  summarize(`Number of districts` = n())

p_fig <- ggplot2::ggplot(data=mai_sum) + 
  geom_bar(aes(x=mean_cat,
               y=`Number of districts`,
               fill=time_period),
           stat = "identity",
           position = position_dodge())+ 
  xlab("Mai category") +
  ylab("Total number of districs")

ggsave(p_fig,
       file = str_glue("{opt$output_dir}/figure_mai_time_period_comparison.png"),
       width = 15,
       height = 8, 
       dpi = 300)