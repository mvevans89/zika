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
