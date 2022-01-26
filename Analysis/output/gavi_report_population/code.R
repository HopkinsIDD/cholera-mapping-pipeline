library(tidyverse)
library(dplyr)
all_paths <-
  list.files(path = "C:/IDD/Cholera/gavi_report/pop_final_hopefully",
             pattern = "*.csv",
             full.names = TRUE)
all_content <-
  all_paths %>%
  lapply(read.csv,
         header = TRUE,
         encoding = "UTF-8")
head(all_content)
all_filenames <- all_paths %>%
  basename() %>%
  as.list()

data_list<-mapply(cbind, all_content, all_filenames, SIMPLIFY=F)

for (i in seq_along(data_list)){
  colnames(data_list[[i]])<- c("cat",">0.001",">0.0001",">0.00001","country")
  data_list[[1]][,1]<-c("mean_values","2.5%","50%","97.5%")
}

data<-do.call(rbind, data_list)%>%mutate(country_iso=substr(country,12,14),
                                             year=paste0(parse_number(.$country)))
#data[which(data$country_iso%in%c("AGO","COD","COG","NER","NGA","ETH","SDN")),]$year="2015_2019"
nrow(data)
head(data)
new_data=data.frame()
for (iso in unique(data$country_iso)) {
  for (year_idx in unique(data[data$country_iso==iso,]$year)){
    tmp=data%>%subset(country_iso==iso&!cat%in%"mean_values"&year%in%year_idx)
    tmp[,1]=as.character(c("2.5%","50%","97.5%"))
    tmp=tmp%>%
      mutate(
        ">=0.001"=paste0(round(tmp[tmp$cat=="50%",]$">0.001",2),"(",round(tmp[tmp$cat=="2.5%",]$">0.001",2),"-",round(tmp[tmp$cat=="97.5%",]$">0.001",2),")"),
        ">=0.0001"=paste0(round(tmp[tmp$cat=="50%",]$">0.0001",2),"(",round(tmp[tmp$cat=="2.5%",]$">0.0001",2),"-",round(tmp[tmp$cat=="97.5%",]$">0.0001",2),")"),
        ">=0.00001"=paste0(round(tmp[tmp$cat=="50%",]$">0.00001",2),"(",round(tmp[tmp$cat=="2.5%",]$">0.00001",2),"-",round(tmp[tmp$cat=="97.5%",]$">0.00001",2),")")
      )
    new_data=rbind(new_data,tmp[2,])
  }
}
new_data=new_data[,colnames(new_data)%in%c("country_iso","year",">=0.001",">=0.0001",">=0.00001")]
head(new_data)

mean_value=data%>%filter(cat=="mean_values")%>%rename(
  ">=0.001_mean_value"=">0.001",
  ">=0.0001_mean_value"=">0.0001",
  ">=0.00001_mean_value"=">0.00001"
)
mean_value$`>=0.001_mean_value`=round(mean_value$`>=0.001_mean_value`,2)
mean_value$`>=0.0001_mean_value`=round(mean_value$`>=0.0001_mean_value`,2)
mean_value$`>=0.00001_mean_value`=round(mean_value$`>=0.00001_mean_value`,2)

mean_value=mean_value[,colnames(mean_value)%in%c("country_iso","year",">=0.001_mean_value",">=0.0001_mean_value",">=0.00001_mean_value")]




final_data <- merge(new_data,mean_value,by=c("country_iso","year"))
head(final_data)
write.csv(final_data,"C:/IDD/Cholera/gavi_report/pop_final_hopefully/gavi_pop.csv",row.names = F)

setwd("C:/IDD/Cholera/gavi_report")
pop<-raster::raster("2017_1km_Aggregated.tif")
sum(raster::values(pop)[1:100])


znz<-sf::st_read("tza_non_znz.shp")
plot(znz$geometry)
shapefile<-shapefile <- rgeoboundaries::gb_adm0("TZA")
plot(shapefile$geometry,add=T)
