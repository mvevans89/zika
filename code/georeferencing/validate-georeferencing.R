# Validate GADM_Keys with Places
## MV Evans, Feb 2020

#' This script goes through and checks that all the places have a matching GADM_Key.
#' 

options(stringsAsFactors = F)
#set wd to file location

#### Argentina ####

ar.places <- read.csv("../../Argentina/AR_Places.csv")
ar.keys <- read.csv("../../Argentina/AR_GADM_Key.csv")

sum(!(ar.places$location %in% ar.keys$location))  #Zero means all the locations are in the keys

#### Brazil ####

br.places <- read.csv("../../Brazil/BR_Places.csv")
br.keys <- read.csv("../../Brazil/BR_GADM_Key.csv")

sum(!(br.places$location %in% br.keys$location))  #Zero means all the locations are in the keys
#investigate missing
br.places[!(br.places$location %in% br.keys$location),] #regions are larger than the ADM1 area of a state and so aren't included

#### Colombia ####

col.places <- read.csv("../../Colombia/CO_Places.csv")
col.keys <- read.csv("../../Colombia/CO_GADM_Key.csv")

sum(!(col.places$location %in% col.keys$location))  #Zero means all the locations are in the keys
#investigate missing
col.places[!(col.places$location %in% col.keys$location),] #unknown locations do not have a GID associated with them

#### Dominican Republic ####

dr.places <- read.csv("../../Dominican_Republic/DO_Places.csv")
dr.keys <- read.csv("../../Dominican_Republic/DO_GADM_Key.csv")

sum(!(dr.places$location %in% dr.keys$location))  #Zero means all the locations are in the keys
#investigate missing
dr.places[!(dr.places$location %in% dr.keys$location),] #unknown locations do not have a GID associated with them

#### Ecuador ####

ec.places <- read.csv("../../Ecuador/EC_Places.csv")
ec.keys <- read.csv("../../Ecuador/EC_GADM_Key.csv")

sum(!(ec.places$location %in% ec.keys$location))  #Zero means all the locations are in the keys
#investigate missing
ec.places[!(ec.places$location %in% ec.keys$location),] #only country level data that is missing

#### El Salvador ####

sv.places <- read.csv("../../El_Salvador/SV_Places.csv")
sv.keys <- read.csv("../../El_Salvador/SV_GADM_Key.csv")

sum(!(sv.places$location %in% sv.keys$location))  #Zero means all the locations are in the keys
#investigate missing
sv.places[!(sv.places$location %in% sv.keys$location),] #only country level data that is missing

#### France ####

fr.places <- read.csv("../../France/FR_Places.csv")
fr.keys <- read.csv("../../France/FR_GADM_Key.csv")

sum(!(fr.places$location %in% fr.keys$location))  #Zero means all the locations are in the keys
#investigate missing
fr.places[!(fr.places$location %in% fr.keys$location),] #only country level data that is missing

#### Guatemala ####

gt.places <- read.csv("../../Guatemala/GT_Places.csv")
gt.keys <- read.csv("../../Guatemala/GT_GADM_Key.csv")

sum(!(gt.places$location %in% gt.keys$location))  #Zero means all the locations are in the keys
#investigate missing
gt.places[!(gt.places$location %in% gt.keys$location),] #the missing data are regions that are larger than ADM1 in the GADM data

#### Haiti ####

ha.places <- read.csv("../../Haiti/HA_Places.csv")
ha.keys <- read.csv("../../Haiti/HA_GADM_Key.csv")

sum(!(ha.places$location %in% ha.keys$location))  #Zero means all the locations are in the keys
#investigate missing
ha.places[!(ha.places$location %in% ha.keys$location),] #the missing data are regions that are larger than ADM1 in the GADM data

#### Mexico ####
mx.places <- read.csv("../../Mexico/MX_Places.csv")
mx.keys <- read.csv("../../Mexico/MX_GADM_Key.csv")

sum(!(mx.places$location %in% mx.keys$location))  #Zero means all the locations are in the keys
#investigate missing
mx.places[!(mx.places$location %in% mx.keys$location),] #the missing data are regions that are larger than ADM1 in the GADM data

#### Nicaragua ####
ni.places <- read.csv("../../Nicaragua/NI_Places.csv")
ni.keys <- read.csv("../../Nicaragua/NI_GADM_Key.csv")

sum(!(ni.places$location %in% ni.keys$location))  #Zero means all the locations are in the keys
#investigate missing
ni.places[!(ni.places$location %in% ni.keys$location),] # county and other are missing but shouldn't be included

#### Panama ####
pa.places <- read.csv("../../Panama/PA_Places.csv")
pa.keys <- read.csv("../../Panama/PA_GADM_Key.csv")

sum(!(pa.places$location %in% pa.keys$location))  #Zero means all the locations are in the keys
#investigate missing
pa.places[!(pa.places$location %in% pa.keys$location),] # missing unknowns and imported

#### Puerto Rico ####
pr.places <- read.csv("../../Puerto_Rico/PR_Places.csv")
pr.keys <- read.csv("../../Puerto_Rico/PR_GADM_Key.csv")

sum(!(pr.places$location %in% pr.keys$location))  #Zero means all the locations are in the keys
#investigate missing
pr.places[!(pr.places$location %in% pr.keys$location),] 

#### United States ####
us.places <- read.csv("../../United_States/US_Places.csv")
us.keys <- read.csv("../../United_States/US_GADM_Key.csv")

sum(!(us.places$location %in% us.keys$location))  #Zero means all the locations are in the keys
#investigate missing
us.places[!(us.places$location %in% us.keys$location),] # missing unknowns and imported
