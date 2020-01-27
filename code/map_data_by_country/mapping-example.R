## Mapping Georeferenced Zika Data Example
## MV Evans 2020-01-27

#### Packages and Set Up ####

library(tidyr)
library(sp)
library(rgdal)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(viridis)

#set wd to source file location
#get columns of interest
variables <- c("report_date", "location", "location_type", "data_field", "data_field_code", 
               "time_period", "time_period_type", "value", "unit")

#### Ecuador ####

#read key
ecu.key <- read.csv("../../Ecuador/EC_GADM_Key.csv", stringsAsFactors = F)

#rbind all the zika data together
zika.files <- list.files("../../Ecuador/GACETA-ZIKA/data", full.names = T)
tables <- lapply(zika.files, readr::read_csv)
ecu.zika  <- do.call(rbind , lapply(tables, subset, select = variables)) %>%
  filter(data_field == "total_zika_confirmed_autochthonous")

#join with GADM keys
ecu.zika <- ecu.zika %>%
  filter(location_type == "county") %>%
  left_join(ecu.key, by = "location") %>%
  #get max per province bc data is cumulative
  group_by(GID_2) %>%
  summarise(cumulative.zika = max(value)) %>%
  ungroup()

#join to gadm data (downloaded from https://gadm.org/download_country_v3.html)
ecu.geo <- readRDS("gadm36_ECU_2_sf.rds") %>%
  left_join(ecu.zika, by = "GID_2") 

#fill in NAs with zeros
ecu.geo$cumulative.zika[is.na(ecu.geo$cumulative.zika)] <- 0
#create breaks
ecu.geo$zika.break <- cut(ecu.geo$cumulative.zika, breaks = c(0,0.5,1,5,10,100,1000), include.lowest = T)


png("map-ex-ecuador.png", width = 1000, height = 500, res = 200)
ggplot(data = ecu.geo) +
  geom_sf(color = "gray10", size = 0.1, aes(fill = zika.break)) +
  scale_fill_manual(values = c("white", "#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c"), 
                    na.value = "gray",
                    labels = c("No cases", "1", "<5", "<10","<100", "<1000")) +
  theme_bw()
dev.off()

#### Mexico ####

#read key
mex.key <- read.csv("../../Mexico/MX_GADM_Key.csv", stringsAsFactors = F)



#get annual cumulative cases in females from 2017 & join to key
mex.zika <- read.csv("../../Mexico/DGE_Zika/data/DGE_Zika-2017-12-30.csv", stringsAsFactors = F) %>%
  filter(data_field == "yearly_cumulative_female") %>%
  left_join(mex.key, by = "location")


#join with spatial data (downloaded from https://gadm.org/download_country_v3.html)
mex.geo <- readRDS("gadm36_MEX_1_sf.rds") %>%
  left_join(mex.zika, by = "GID_1")

#create breaks
mex.geo$zika.break <- cut(mex.geo$value, breaks = c(0,0.5,1,10,100,1000), include.lowest = T)

#map
png("map-ex-mexico.png", width = 1000, height = 500, res = 200)
ggplot(data = mex.geo) +
  geom_sf(color = "gray10", size = 0.1, aes(fill = zika.break)) +
  scale_fill_brewer("Total Female Cases in 2017", palette = "Reds", 
                    na.value = "gray",
                    labels = c("No cases", "<1", "<10", "<100","<1000")) +
  theme_bw()
dev.off()
