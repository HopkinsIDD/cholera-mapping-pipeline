library(tidyverse)
library(sf)
library(ggplot2)

params <- list(
  taxdir="cholera-mapping-pipeline",
  config="Analysis/configs/config.yml",
  loc_lp ="Analysis/R/loc_lp.csv",
  unified_oc="Analysis/R/unified_OC.csv",
  output_dir="cholera-mapping-output"
)

#load config file
config <- yaml::read_yaml(file.path(params$taxdir,params$config))

location_lp <- file.path(params$taxdir,params$loc_lp) %>% read.csv() #get location_peiod_location_id_file
unified_oc <- file.path(params$taxdir,params$unified_oc) %>% read.csv() #get unified OC file
load(file.path(params$output_dir,config$file_names$stan_input_filename))

#get aggregated annual observations from subnational data
tmp1<-data.frame(stan_input$sf_cases_resized %>% 
                  subset(!admin_level == 0 & TR-TL+1>335 & TR-TL+1<395) %>% 
                  mutate(country=config$countries_name,year=substr(TL,1,4)))%>%
  group_by(country,OC_UID,year,locationPeriod_id)%>%
  mutate(sCh=mean(attributes.fields.suspected_cases)) %>%
  select(country,OC_UID,year,admin_level,locationPeriod_id,sCh) %>% 
  group_by(country,OC_UID,year,admin_level) %>% 
  summarize(total_sCh=aggregated_cases<-sum( sCh ))

#get who annual report
tmp2<-data.frame(stan_input$sf_cases_resized)%>%
  subset(OC_UID%in%unified_oc$oc[unified_oc$who_annual_report==T])%>%
  mutate(country=config$countries_name,year=substr(TL,1,4))%>%
  group_by(country,OC_UID,year,admin_level,locationPeriod_id)%>%
  summarize(total_sCh=attributes.fields.suspected_cases)%>%
  select(country,OC_UID,year,total_sCh,admin_level,locationPeriod_id)

#get annual observations from national data
tmp3<-data.frame(stan_input$sf_cases_resized %>% 
                  subset(admin_level == 0 & TR-TL+1>335 & TR-TL+1<395) %>% 
                  mutate(country=config$countries_name,year=substr(TL,1,4),
                         admin_level = 0)) %>% 
  group_by(country,OC_UID,year,locationPeriod_id,admin_level) %>% 
  mutate(sCh=mean(attributes.fields.suspected_cases)) %>% 
  reframe(total_sCh= sCh)

total_data<-bind_rows(tmp1,tmp2,tmp3)%>%
  mutate(who_annual_report=ifelse(OC_UID%in%unified_oc$oc[unified_oc$who_annual_report==T],TRUE,FALSE),
         admin_level = factor(admin_level, levels=c(0,1,2,3,4,5,6)))
ggplot2::ggplot()+
  geom_point(data=total_data,aes(y=total_sCh,x=year,color=admin_level,shape=who_annual_report),size=5)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ggtitle(label=config$countries_name)