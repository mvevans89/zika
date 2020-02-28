## Georeference CDC Zika Data
## MV Evans, Jan 2020

# This document joins the CDC Zika data downloaded from https://github.com/cdcepi/zika 
# with a GADM shapefile for the finest administrative sub-level possible.

# Each country section will output a csv that has the necessary data to join the zika data 
# to the GADM v. 36 data via the GID code

#### Packages and Set Up ####

library(tidyr)
library(sp)
library(rgdal)
library(maps)
library(stringr)

library(dplyr)

#set wd to source file location

# # load gadm table (Version 3.6, original GADM_maptable.txt is out of date)
#  gadm.data <- read.csv("../../../code/gadm-data-36/gadm36-table.csv")
# # #save smaller version of only countries in zika epi
# gadm.zika <- filter(gadm.data, GID_0 %in% c("ARG","ECU", "BRA", "COL", "DOM", "SLV",
#                                             "GTM", "HTI", "MAF", "BLM", "MTQ", "GUF", 
#                                             "GLP", "MEX", "PAN", "NIC", "VIR",
#                                             "USA", "PRI", "ASM"))
# write.csv(gadm.zika, "../../../code/gadm-data-36/gadm36-table-subset.csv", row.names = F)
# 
gadm.data <- read.csv("gadm36-table-subset.csv")

#leaflet function to investigate
mapLeaflet <- function(shapefile){
  require(leaflet)
  
  #create labels
  labels <- sprintf(
    "NAME_1: %s <br/> NAME_2 : %s",
    shapefile$NAME_1, shapefile$NAME_2
  ) %>% lapply(htmltools::HTML)
  
  leaflet(shapefile) %>%
    addTiles() %>%
    addPolygons(weight = 1, color = "#444444",
    #get popup with info
    label = labels)
}



#### Argentina ####

# Cases are by province (NAME_1)

zika.arg <- read.csv("../../Argentina/AR_Places.csv") %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1"), sep = "-", remove = F) %>%
  filter(location_type == "province") %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  #adjust names to match gadm
  mutate(NAME_1 = case_when(
    NAME_1 == "Sgo Del Estero" ~ "SANTIAGO DEL ESTERO",
    NAME_1 == "CABA" ~ "CIUDAD DE BUENOS AIRES",
    TRUE ~ NAME_1
  )) %>%
  mutate(NAME_1 = toupper(NAME_1))

# read in gadm (v 3.6) 
gadm.arg <- gadm.data %>%
  filter(GID_0 == "ARG") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("\\~", "", NAME_1)) 

# join
zika.arg.join <- left_join(zika.arg, gadm.arg , 
                        by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1")) 
  
# check it worked
sum(is.na(zika.arg.join$GID_1)) # success == 0

# save join key
key.arg <- zika.arg.join %>%
  select(location, NAME_0, NAME_1, GID_1) %>%
  distinct()

write.csv(key.arg, "../../Argentina/AR_GADM_Key.csv", row.names = F)

#### Brazil ####

# Case data is at level of the state (NAME_1)
# Note there is also data at the level of the region for Norte, Nordest, Sudeste,
# Sul, & Centro (incorrectly categorized as NAME_0). These are being ignored 
# becuase they are coarser than states and do not correspond to a GADM level.

zika.bra <- read.csv("../../Brazil/BR_Places.csv") %>%
  tidyr::separate(location, into = c("NAME_0", "NAME_1", "NAME_2"), 
                  sep = "-", remove = F) %>% 
  filter(location_type == "state") %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_1 = toupper(NAME_1))

#read in gadm (v 3.6) 
gadm.bra <- gadm.data %>%
  filter(GID_0 == "BRA") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("\\~", "", NAME_1)) %>%
  mutate(NAME_1 = gsub("\\^", "", NAME_1))

#join
zika.bra.join <- left_join(zika.bra, gadm.bra, by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1"))

#check it worked
sum(is.na(zika.bra.join$GID_1)) # success == 0

#save key
key.bra <- zika.bra.join %>%
  select(location, NAME_0, NAME_1, GID_1) %>%
  distinct()

write.csv(key.bra, "../../Brazil/BR_GADM_Key.csv", row.names = F)

#### Colombia ####

#' Cases are reported at municipality level (NAME_2)
#' Many of these did not match the GADM data. Much of this was because the NAME_1 was 
#' incorrectly spelled, but some were because not all municpalities are in the GADM
#' data. They will be aggregated into whatever spatial unit they fall into through 
#' georeferencing the municipalities.

zika.col <- read.csv("../../Colombia/CO_Places.csv") %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1", "NAME_2"), 
                  sep = "-", remove = F) %>% 
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_1 = toupper(NAME_1)) %>%
  mutate(NAME_2 = gsub("_", " ", NAME_2)) %>%
  mutate(NAME_2 = toupper(NAME_2)) %>%
  #manual adjustments
  #drop "Unknowns"
  filter(NAME_2 != "UNKNOWN") %>%
  mutate(NAME_1 = case_when(
    NAME_1 == "GUAJIRA" ~ "LA GUAJIRA",
    TRUE ~ NAME_1
  )) %>%
  mutate(NAME_2 = case_when(
    NAME_2 == "MIRITI" ~ "MIRITI-PANAMA",
    TRUE ~ NAME_2
  ))

#read in gadm (v 3.6) 
gadm.col <- gadm.data %>%
  filter(GID_0 == "COL") %>%
  select(NAME_0, NAME_1, NAME_2, GID_2) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("\\~", "", NAME_1)) %>%
  mutate(NAME_1 = gsub("\\^", "", NAME_1)) %>%
  mutate(NAME_2 = toupper(gsub("`|\\'", "", iconv(NAME_2, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_2 = gsub("\\~", "", NAME_2)) %>%
  mutate(NAME_2 = gsub("\\^", "", NAME_2))


#join
zika.col.join <- left_join(zika.col, gadm.col, by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1", "NAME_2" = "NAME_2"))

#check it worked
sum(is.na(zika.col.join$GID_2)) # success == 0

#troubleshoot
zika.names <- unique(zika.col$NAME_2)
gadm.names <- unique(gadm.col$NAME_2)
zika.names[(!(zika.names %in% gadm.names))]
#to.fix was used to georeference missing ones manually (by MVEvans)
to.fix <- zika.col.join %>%
  filter(is.na(GID_2)) %>%
  select(location, location_type,NAME_0, NAME_1, NAME_2, GID_2) %>%
  distinct()

#subset those that are fine
gid.correct <- zika.col.join %>%
  filter(!is.na(GID_2)) %>%
  select(location, location_type,NAME_0, NAME_1, NAME_2, GID_2) %>%
  distinct()

# load manual join file with coordinates for missing
# this file was created by MV Evans by georeferencing the points
col.coords <- read.csv("zika-colombia-manual.csv", stringsAsFactors = F)
#georeference missing ones
col.missing.gid <- filter(col.coords, is.na(GID_2) | GID_2 == "") %>%
  filter(!is.na(lat))

col.missing.gid.shp <- SpatialPointsDataFrame(coords = col.missing.gid[,c("long", "lat")], data = col.missing.gid,
                                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#load colombia shapefile
col.shp <- readOGR("shapefiles/gadm36_COL_2.shp")

#find GID2 codes for those missing it based on spatial location
georef.gid <- do.call(rbind.data.frame, over(col.missing.gid.shp, col.shp, returnList = T)) %>%
  select(NAME_0, NAME_1, NAME_2,GID_2) %>%
  mutate(location = col.missing.gid.shp$location) 
# join with those that were just misspelled
col.corrected <- col.coords %>%
  filter(!(is.na(GID_2)|GID_2 == "")) %>%
  select(location, NAME_0, NAME_1, NAME_2, GID_2) %>%
  bind_rows(georef.gid) %>%
  select(location, NAME_0, NAME_1, NAME_2, GID_2)

#now join with the 900-ish that were already correct
col.key2 <- gid.correct %>%
  select(location,  NAME_0, NAME_1, NAME_2, GID_2) %>%
  bind_rows(col.corrected) %>%
  distinct() 

#go through and do this for the ADM1 level in colombia to join with this later
zika.col.1 <- read.csv("../../Colombia/CO_Places.csv") %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1", "NAME_2"), 
                  sep = "-", remove = F) %>% 
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_1 = toupper(NAME_1)) %>%
  filter(location_type == "province") %>%
  #drop unknowns & imported
  filter(NAME_1 != "UNKNOWN") %>%
  filter(NAME_1 != "DESCONOCIDO") %>%
  filter(NAME_1 != "EXTERIOR") %>%
  #adjust for some which are mislabeled
  mutate(NAME_1 = case_when(
    NAME_1 == "BARRANQUILLA" ~ "ATLANTICO", #this is the capital of atlantico
    NAME_1 == "BOGOTA" ~ "CUNDINAMARCA", #in this Cundinamarca dept
    NAME_1 == "BUENAVENTURA" ~ "VALLE DEL CAUCA",
    NAME_1 == "CARTAGENA" ~ "BOLIVAR",
    NAME_1 == "GUAJIRA" ~ "LA GUAJIRA",
    NAME_1 == "NORTE SANTANDER" ~ "NORTE DE SANTANDER",
    NAME_1 == "SAN ANDRES" ~ "SAN ANDRES Y PROVIDENCIA",
    NAME_1 == "SANTA MARTA" ~ "MAGDALENA",
    NAME_1 == "VALLE" ~ "VALLE DEL CAUCA",
    TRUE ~ NAME_1
  ))

#join with GID1
gadm.col.1 <- gadm.data %>%
  filter(GID_0 == "COL") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("\\~", "", NAME_1)) %>%
  mutate(NAME_1 = gsub("\\^", "", NAME_1))

#join
zika.col.join.1 <- left_join(zika.col.1, gadm.col.1, by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1")) %>%
  select(location,  NAME_0, NAME_1, GID_1)

#combine with gadm level 2 data
col.key.all <- bind_rows(col.key2, zika.col.join.1)

#there are some extras so bind this back with the initial locations from CO_Places
col.location.key <- read.csv("../../Colombia/CO_Places.csv") %>%
  select(location) %>%
  left_join(col.key.all, by  = "location") %>%
  #manually fix the last couple
  mutate(GID_2 = case_when(
    location == "Colombia-Cundinamarca-Rafael_Reyes_Apulo" ~ "COL.14.5_1",
    location  == "Colombia-Sta_Marta_DE-Santa_Marta" ~ "COL.19.19_1",
    location == "Colombia-Magdalena-Sitio_Nuevo" ~ "COL.19.17_1", #actually in San Zenon
    location == "Colombia-Tolima-Armero_Guayabal" ~ "COL.29.5_1",
    location == "Colombia-Bogota-Bogota" ~ "COL.14.79_1",
    location == "Colombia-Bogota-Bogota_DC" ~ "COL.14.79_1",
    TRUE ~ GID_2
  ))

#how many are missing
missing <- filter(col.location.key, is.na(GID_1) & is.na(GID_2)) #all unknowns

#' note that three are still missing. One was mistakenly at the country level,
#' and the others were not found through georeferencing.

col.location.key <- filter(col.location.key, !(is.na(GID_1) & is.na(GID_2)))

#save key
write.csv(col.location.key, "../../Colombia/CO_GADM_Key.csv", row.names = F)

#### Dominican Republic ####

# Finest scale data are at the municipality level (NAME_2)
# There are also data at the province level (NAME_1)
# However most data are from Santiago Rodriguez which is ADM level 1 (GID_1 = DOM.29)

zika.dom <- read.csv("../../Dominican_Republic/DO_Places.csv") %>%
  tidyr::separate(location, into = c("NAME_0", "NAME_1", "NAME_2"), 
                  sep = "-", remove = F) %>%
  mutate(NAME_0 = "Dominican Republic") %>%
  filter(location_type == "municipality") %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_1 = toupper(NAME_1)) %>%
  mutate(NAME_2 = gsub("_", " ", NAME_2)) %>%
  mutate(NAME_2 = toupper(NAME_2)) %>%
  #manually adjust
  mutate(NAME_2 = case_when(
    NAME_1 == "DISTRITO NACIONAL" ~ "DISTRITO NACIONAL",
    NAME_1 == "JIMANI DE INDEPENDENCIA" ~ "JIMANI",
    NAME_1 == "SANTO DOMINGO NORTE" ~ "SANTO DOMINGO NORTE",
    NAME_1 == "SANTO DOMINGO ESTE" ~ "SANTO DOMINGO ESTE",
    NAME_1 == "SANTO DOMINGO OESTE" ~ "SANTO DOMINGO OESTE",
    NAME_2 == "BARAHONA" ~ "SANTA CRUZ DE BARAHONA",
    NAME_2 == "CABALLEROS" ~ "SANTIAGO DE LOS CABALLEROS",
    NAME_2 == "ENSUENO" ~ "SANTIAGO DE LOS CABALLEROS",
    NAME_2 == "PUERTO PLATA" ~ "LUPERON",
    NAME_2 == "COMPOSTELA" ~ "AZUA DE COMPOSTELA",
    NAME_1 == "ENSANCHE OZAMA" ~ "SANTO DOMINGO ESTE",
    NAME_1 == "LAS CANAS" ~ "CONCEPCION DE LA VEGA",
    NAME_1 == "LAS MINAS NORTE" ~ "SANTO DOMINGO ESTE",
    TRUE ~ NAME_2
  )) %>%
  mutate(NAME_1 = case_when(
    NAME_1 == "JIMANI DE INDEPENDENCIA" ~ "INDEPENDENCIA",
    NAME_1 %in% c("SANTO DOMINGO NORTE", "SANTO DOMINGO ESTE", "SANTO DOMINGO OESTE", "ENSANCHE OZAMA", "LAS MINAS NORTE") ~ "SANTO DOMINGO",
    NAME_1 == "SANTA CRUZ" ~ "BARAHONA",
    NAME_1 == "LUPERON" ~ "PUERTO PLATA",
    NAME_1 == "LAS CANAS" ~ "LA VEGA",
    TRUE ~ NAME_1
  )) %>%
  #drop provinces
  filter(NAME_2 != "RODRIGUEZ") %>%
  filter(!is.na(NAME_2)) #drops Santo Domingo province

gadm.dom <- gadm.data %>%
  filter(GID_0 == "DOM") %>%
  select(NAME_0, NAME_1, NAME_2, GID_2) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1)) %>%
  mutate(NAME_2 = toupper(gsub("`|\\'", "", iconv(NAME_2, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_2 = gsub("\\~", "", NAME_2)) 

#join
zika.dom.join <- zika.dom %>%
  #dplyr::filter(NAME_2 != "RODRIGUEZ") %>%
  left_join(gadm.dom, by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1", "NAME_2" = "NAME_2"))

#check it worked
sum(is.na(zika.dom.join$GID_2)) # success == 0

# Now do the same for province level case data
zika.dom1 <- read.csv("../../Dominican_Republic/DO_Places.csv") %>%
  filter(location_type == "province") %>%
  tidyr::separate(location, into = c("NAME_0", "NAME_1", "NAME_2"), 
                  sep = "-", remove = F) %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_1 = toupper(NAME_1)) %>%
  mutate(NAME_1 = case_when(
    NAME_1 == "JIMANI DE INDEPENDENCIA" ~ "INDEPENDENCIA",
    NAME_1 %in% c("SANTO DOMINGO NORTE", "SANTO DOMINGO ESTE", "SANTO DOMINGO OESTE") ~ "SANTO DOMINGO",
    NAME_1 == "SANTA CRUZ" ~ "BARAHONA",
    NAME_1 == "LUPERON" ~ "PUERTO PLATA",
    NAME_1 == "BAORUCO" ~ "BAHORUCO",
    NAME_1 == "EL SEIBO" ~ "EL SEYBO",
    NAME_1 == "ELIAS PINA" ~ "LA ESTRELLETA",
    NAME_1 == "HERMANAS MIRABAL" ~ "SALCEDO",
    TRUE ~ NAME_1
  )) %>%
  mutate(NAME_0 = "Dominican Republic") %>%
  #drop imported & other
  filter(NAME_1 != "EXTRANJERA") %>%
  filter(NAME_1 != "OTHER")

gadm.dom1 <- gadm.data %>%
  filter(GID_0 == "DOM") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1)) 

zika.dom.join1 <- zika.dom1 %>%
  left_join(gadm.dom1, by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1"))

# get unique key and save
key.dom2 <- zika.dom.join %>%
  select(location, NAME_0, NAME_1, NAME_2, GID_2) %>%
  distinct()
key.dom1 <- zika.dom.join1 %>%
  select(location, NAME_0, NAME_1, GID_1)

key.dom.all <- bind_rows(key.dom1, key.dom2) %>%
  #add Santo Domingo and Santiago Rodriguez provinces
  bind_rows(data.frame(location = c("Dominican_Republic-Santo_Domingo", "Dominican_Republic-Santiago-Rodriguez"),
                       NAME_0 = "Dominican Republic",
                       NAME_1 = c("SANTO DOMINGO", "SANTIAGO RODRIGUEZ"),
                       GID_1 = c("DOM.31_1", "DOM.29_1")))

write.csv(key.dom.all, "../../Dominican_Republic/DO_GADM_Key.csv", row.names = F)

#### Ecuador ####

# This data goes down to ADM level 2 (canton)

zika.ecu <- read.csv("../../Ecuador/EC_Places.csv") %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1", "NAME_2"), sep = "-", remove = F) %>%
  filter(location_type == "county") %>%
  # fix province to match GADM data
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_2 = gsub("_", " ", NAME_2)) %>%
  mutate(NAME_1 = toupper(NAME_1)) %>%
  mutate(NAME_2 = toupper(NAME_2)) %>%
  #manual adjustments
  mutate(NAME_2 = case_when(
    NAME_1 == "ORELLANA JOYA DE LOS SACHAS" ~ "LA JOYA DE LOS SACHAS",
    NAME_2 == "RIOVERDE" ~ "RIO VERDE",
    TRUE ~ NAME_2
  )) %>%
  mutate(NAME_1 = case_when(
    NAME_1 == "ORELLANA JOYA DE LOS SACHAS" ~ "ORELLANA",
    NAME_1 == "LOS RIOS" & NAME_2 == "JARAMIJO" ~ "MANABI",
    TRUE ~ NAME_1
  ))

gadm.ecu <- gadm.data %>%
  filter(GID_0 == "ECU") %>%
  select(NAME_0, NAME_1, NAME_2, GID_2) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1)) %>%
  mutate(NAME_2 = toupper(gsub("`|\\'", "", iconv(NAME_2, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_2 = gsub("\\~", "", NAME_2)) 

# join on the finest scale possible for each row

zika.ecu.join <- left_join(zika.ecu, gadm.ecu, 
                           by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1","NAME_2" = "NAME_2"))

#check it worked
sum(is.na(zika.ecu.join$GID_2)) #success == 0

# get unique key and save
key.ecu2 <- zika.ecu.join %>%
  select(location, NAME_0, NAME_1, NAME_2, GID_2) %>%
  distinct()

## Now go through the provinces
zika.ecu1 <- read.csv("../../Ecuador/EC_Places.csv") %>%
  filter(location_type == "province") %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1", "NAME_2"), sep = "-", remove = F) %>%
  # fix province to match GADM data
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_1 = toupper(NAME_1)) %>%

  mutate(NAME_1 = case_when(
    NAME_1 == "ORELLANA JOYA DE LOS SACHAS" ~ "ORELLANA",
    NAME_1 == "LOS RIOS" & NAME_2 == "JARAMIJO" ~ "MANABI",
    TRUE ~ NAME_1
  ))

gadm.ecu1 <- gadm.data %>%
  filter(GID_0 == "ECU") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1)) 

zika.ecu.join1 <- left_join(zika.ecu1, gadm.ecu1, 
                           by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1"))

key.ecu1 <- zika.ecu.join1 %>%
  select(location, NAME_0, NAME_1, GID_1) %>%
  distinct()

#join adm1 and adm2 and save

key.ecu.all <- bind_rows(key.ecu1, key.ecu2)

write.csv(key.ecu.all, "../../Ecuador/EC_GADM_Key.csv", row.names = F)

#### El Salvador ####

# Cases are reported at department level (NAME_1)

zika.slv <- read.csv("../../El_Salvador/SV_Places.csv", stringsAsFactors = F) %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1"), sep = "-", remove = F) %>% 
  dplyr::filter(location_type == "department") %>%
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_0 = gsub("_", " ", NAME_0)) %>%
  #drop imported (I think) from Guatemala and Honduras
  filter(!(NAME_1 %in% c("GUATEMALA", "HONDURAS")))

gadm.slv <- gadm.data %>%
  filter(GID_0 == "SLV") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1)) 

# join
zika.slv.join <- left_join(zika.slv, gadm.slv, 
                           by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1")) 

# check it worked
sum(is.na(zika.slv.join$GID_1)) # success == 0

# save join key
key.slv <- zika.slv.join %>%
  select(location, NAME_0, NAME_1, GID_1) %>%
  distinct()

write.csv(key.slv, "../../El_Salvador/SV_GADM_Key.csv", row.names = F)

#### France ####
#French colonies in the Carribean, only 5 so just set manually
#all five are actually considered seperate ADM level 0 in the GADM data


zika.fra <- read.csv("../../France/FR_Places.csv", stringsAsFactors = F) %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1"), sep = "-", remove = F) %>%
  mutate(GID_0 = case_when(
    NAME_1 == "Guadeloupe" ~ "GLP",
    NAME_1 == "Guyane" ~ "GUF",
    NAME_1 == "Martinique" ~ "MTQ",
    NAME_1 == "St_Barthelemy" ~ "BLM",
    NAME_1 == "St_Martin" ~ "MAF"
  )) %>%
  select(location, NAME_0 = NAME_1, GID_0)

write.csv(zika.fra, "../../France/FR_GADM_Key.csv", row.names = F)

#### Guatemala ####

# Guatemala has some places that do not match a GADM, they are manually corrected
# many of these are the regions of the health system zones, corrected following:
# https://guiadeguatemala.wordpress.com/guia-general-de-guatemala/
# One issue is the region Peten matches the municipality Peten, so these cases could be double counted

zika.gua <- read.csv("../../Guatemala/GT_Places.csv", stringsAsFactors = F) %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1"), sep = "-", remove = F) %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_1 = toupper(NAME_1)) %>%
  filter(location_type == "municipality") %>%
  #drop regions
  filter(!(NAME_1 %in% c("NOR OCCIDENTE", "NOR ORIENTE", "SUR", "CENTRAL"))) %>%
  #aggregate smaller places to the municipality
  mutate(NAME_1 = case_when(
    NAME_1 %in% c("EL QUICHE", "IXCAN", "IXIL") ~ "QUICHE",
    NAME_1 %in% c("PETEN NORTE", "PETEN SUR OCCIDENTAL", "PETEN SUR ORIENTAL") ~ "PETEN",
    #spelling error
    NAME_1 == "QUETZALTENANGO" ~ "QUEZALTENANGO",
    TRUE ~ NAME_1
  ))

gadm.gua <- gadm.data %>%
  filter(GID_0 == "GTM") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1))

zika.gua.join <- left_join(zika.gua, gadm.gua, 
                           by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1"))
#check it worked
sum(is.na(zika.gua.join$GID_1)) #success == 0 

#select appropriate columns and save
key.gua <- select(zika.gua.join, location, NAME_0, NAME_1, GID_1)

write.csv(key.gua, "../../Guatemala/GT_GADM_Key.csv", row.names = F)

#### Haiti ####
#These match up with GADM3 levels

zika.hti <- read.csv("../../Haiti/HA_Places.csv", stringsAsFactors = F) %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1", "NAME_3"), sep = "-", remove = F) %>%
  mutate(NAME_1 = toupper(gsub("_", " ", NAME_1))) %>%
  mutate(NAME_3 = toupper(gsub("_", " ", NAME_3))) %>%
  filter(location_type == "municipality") %>%
  #adjust typos & spellings
  mutate(NAME_1 = case_when(
    NAME_1 == "ARTIBONITE" ~ "LARTIBONITE",
    NAME_1 == "GRANDE ANSE" ~ "GRANDANSE",
    TRUE ~ NAME_1
  )) %>%
  #more typos and spellings
  mutate(NAME_3 = case_when(
    NAME_3 == "DESSSALINES MARCHAND" ~ "DESSALINES",
    NAME_3 == "VERRETTES" ~ "VERETTES",
    #Fond des Negres is in Miragoâne Arrondissement
    NAME_3 == "FOND DES NEGRES" ~ "MIRAGOANE",
    NAME_3 == "ANSE A PITRES" ~ "ANSE A PITRE",
    TRUE ~ NAME_3
  ))

gadm.hti <- gadm.data %>%
  filter(GID_0 == "HTI") %>%
  select(NAME_0, NAME_1, NAME_2, NAME_3, GID_3) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1)) %>%
  mutate(NAME_2 = toupper(gsub("`|\\'", "", iconv(NAME_2, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_2 = gsub("\\~", "", NAME_2)) %>%
  mutate(NAME_3 = toupper(gsub("`|\\'", "", iconv(NAME_3, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_3 = gsub("\\~", "", NAME_3)) %>%
  mutate(NAME_1 = gsub("-", " ", NAME_1)) %>%
  mutate(NAME_3 = gsub("-", " ", NAME_3))

# join on the finest scale possible for each row
zika.hti.join2 <- left_join(zika.hti, gadm.hti, 
                           by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1","NAME_3" = "NAME_3")) %>%
  select(location,  NAME_0, NAME_1, NAME_2, NAME_3, GID_3)

#check it worked
sum(is.na(zika.hti.join2$GID_3)) #success==0

#Haiti also reports data at the level of the province, ADM1
zika.hti1 <- read.csv("../../Haiti/HA_Places.csv", stringsAsFactors = F) %>%
  filter(location_type == "province") %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1"), sep = "-", remove = F) %>%
  mutate(NAME_1 = toupper(gsub("_", " ", NAME_1))) %>%
  #adjust typos & spellings
  mutate(NAME_1 = case_when(
    NAME_1 == "ARTIBONITE" ~ "LARTIBONITE",
    NAME_1 == "GRANDE ANSE" ~ "GRANDANSE",
    TRUE ~ NAME_1
  ))

gadm.hti1 <- gadm.data %>%
  filter(GID_0 == "HTI") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1)) %>%
  mutate(NAME_1 = gsub("-", " ", NAME_1))

zika.hti.join1 <- left_join(zika.hti1, gadm.hti1, 
                           by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1")) %>%
  select(location, NAME_0, NAME_1, GID_1)

hti.key <- bind_rows(zika.hti.join1, zika.hti.join2)

write.csv(hti.key, "../../Haiti/HA_GADM_Key.csv", row.names = F)

#### Mexico ####

zika.mex <- read.csv("../../Mexico/MX_Places.csv", stringsAsFactors = F) %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1"), sep = "-", remove = F) %>%
  mutate(NAME_1 = toupper(gsub("_", " ", NAME_1))) %>%
  #spelling and name errors
  mutate(NAME_1 = case_when(
    NAME_1 == "COAHUILA DE ZARAGOZA" ~ "COAHUILA",
    NAME_1 == "QUERETARO DE ARTEAGA" ~ "QUERETARO",
    NAME_1 == "MICHOACAN DE OCAMPO" ~ "MICHOACAN",
    NAME_1 == "VERACRUZ DE IGNACIO DE LA LLAVE" ~ "VERACRUZ",
    TRUE ~  NAME_1
  ))

gadm.mex <- gadm.data %>%
  filter(GID_0 == "MEX") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("\\~", "", NAME_1)) %>%
  mutate(NAME_1 = gsub("\\^", "", NAME_1))

# join on the finest scale possible for each row
zika.mex.join <- left_join(zika.mex, gadm.mex, 
                           by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1"))

#check it worked
sum(is.na(zika.mex.join$GID_1)) #success==0

#save
key.mex <- select(zika.mex.join,location, NAME_0, NAME_1, GID_1)

write.csv(key.mex, "../../Mexico/MX_GADM_Key.csv", row.names = F)


#### Nicaragua ####

zika.nic2 <- read.csv("../../Nicaragua/NI_Places.csv", stringsAsFactors = F) %>%
  filter(location_type %in% c("municipality", "district")) %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1", "NAME_2"), sep = "-", remove = F) %>%
  mutate(NAME_1 = toupper(gsub("_", " ", NAME_1))) %>%
  mutate(NAME_2 = toupper(gsub("_", " ", NAME_2))) %>%
  #drop the Other category
  filter(NAME_1 != "OTHER") %>%
  #spelling errors
  mutate(NAME_2 = case_when(
    NAME_2 == "QUETZALGUAQUE" ~ "QUEZALGUAQUE",
    NAME_2 == "CIUDAD SANDINO" ~ "MANAGUA", #in managua city
    NAME_2 == "CARDENAS" ~ "TOLA",
    NAME_2 %in% c("DISTRITO I", "DISTRITO II", "DISTRITO III", "DISTRITO IV", "DISTRITO V") ~ "MANAGUA", #in managua city
    TRUE ~ NAME_2
  ))

gadm.nic2 <- gadm.data %>%
  filter(GID_0 == "NIC") %>%
  select(NAME_0, NAME_1, NAME_2,GID_2) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("\\~", "", NAME_1)) %>%
  mutate(NAME_1 = gsub("\\^", "", NAME_1)) %>%
  #drop accents and capitalize
  mutate(NAME_2 = toupper(gsub("`|\\'", "", iconv(NAME_2, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_2 = gsub("\\~", "", NAME_2)) %>%
  mutate(NAME_2 = gsub("\\^", "", NAME_2)) 


# join on the finest scale possible for each row
zika.nic.join2 <- left_join(zika.nic2, gadm.nic2, 
                           by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1", "NAME_2" = "NAME_2")) %>%
  select(location, NAME_0, NAME_1, NAME_2, GID_2)

#check it worked
sum(is.na(zika.nic.join2$GID_2)) #success==0

# Do the same for ADM1 (city and district)
zika.nic1 <- read.csv("../../Nicaragua/NI_Places.csv", stringsAsFactors = F) %>%
  filter(location_type %in% c("city")) %>%
  tidyr::separate(location, into=c("NAME_0", "NAME_1"), sep = "-", remove = F) %>%
  mutate(NAME_1 = toupper(gsub("_", " ", NAME_1))) %>%
  #drop the Other category
  filter(NAME_1 != "OTHER") 

gadm.nic1 <- gadm.data %>%
  filter(GID_0 == "NIC") %>%
  select(NAME_0, NAME_1,GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("\\~", "", NAME_1)) %>%
  mutate(NAME_1 = gsub("\\^", "", NAME_1))

zika.nic.join1 <- left_join(zika.nic1, gadm.nic1, 
                            by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1")) %>%
  select(location, NAME_0, NAME_1,GID_1)

key.nic <- bind_rows(zika.nic.join1, zika.nic.join2)

write.csv(key.nic, "../../Nicaragua/NI_GADM_Key.csv", row.names = F)

#### Panama ####

#' The Panama data has an issue in that some data is reported at the level of GID3 and others at GID2,
#' and others at the level of a small town or neighborhood. For small towns/islands, the places have 
#' been georeferenced. These coordinates are then used to find out what GID3 shapefile they fall within
#' and summed up to get ADM level 3 values.

zika.pan <- read.csv("../../Panama/PA_Places.csv", stringsAsFactors = F) %>%
  dplyr::filter(location_type == "county" ) %>% 
  #the second bit of the name is actually NAME_3 for Panama becuase they are counties
  tidyr::separate(location, into=c("NAME_0", "NAME_1", "NAME_3"), sep = "-", remove = F) %>% 
  # fix accent issue
  mutate(NAME_0 = toupper(NAME_0)) %>%
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  mutate(NAME_3 = toupper(gsub("`|\\'", "", iconv(NAME_3, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_3 = gsub("_", " ", NAME_3))

# get panama data from gadm
gadm.pan <- gadm.data %>%
  filter(GID_0 == "PAN") %>%
  select(NAME_0, NAME_1, NAME_2, NAME_3, GID_3) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_0 = toupper(NAME_0)) %>%
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1)) %>%
  mutate(NAME_3 = toupper(gsub("`|\\'", "", iconv(NAME_3, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_3 = gsub("~", "", NAME_3)) 

#load manual join file (created from zika.pan and then georeferenced) with coordinates
pan.coords <- read.csv("zika-panama-manual.csv", stringsAsFactors = F)
#get those missing and make spatial
pan.missing.gid <- filter(pan.coords, is.na(GID_3) | GID_3=="") %>%
  #drop ones without coordinates (one I couldn't find, another is GID level 2)
  filter(!is.na(lat))

pan.missing.gid.shp <- SpatialPointsDataFrame(coords = pan.missing.gid[,c("long", "lat")], data = pan.missing.gid,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#load panama shapefile
pan.shp <- readOGR("shapefiles/gadm36_PAN_3.shp")

#find GID3 codes for missing coordinates based on spatial location
georef.gid <- over(pan.missing.gid.shp, pan.shp) %>%
  select(NAME_0, NAME_1, NAME_2, NAME_3,GID_3) %>%
  mutate(location = pan.missing.gid.shp$location) %>%
  filter(!(is.na(GID_3))) #drop san blas islands 
  

#' note that this still misses some islands on the san blas archipelago which are not included in gadm
#add georeferenced back to others that already had a gid_3
pan.ref <- pan.coords %>%
  filter(!(is.na(GID_3)|GID_3 == "")) %>%
  select(NAME_0, NAME_1, NAME_2, NAME_3, GID_3, location) %>%
  bind_rows(georef.gid) %>%
  select(location, NAME_0, NAME_1, NAME_2, NAME_3, GID_3) %>%
  mutate(NAME_0 = toupper(NAME_0))

#join with places
zika.pan.join <- left_join(zika.pan, pan.ref, 
                           by = c("location" = "location"))

#check it worked
#there are some NAs due to the not specified places and the San Blas Islands
sum(is.na(zika.pan.join$GID_3)) #success == 0

#select only the wanted columns
key.pan <- select(zika.pan.join, location, NAME_0 = NAME_0.y, NAME_1 = NAME_1.y, NAME_2, NAME_3 = NAME_3.y, GID_3) %>%
  filter(!is.na(GID_3))

#There is also data reported at the province level (ADM1) that needs to be georeferenced
zika.pan1 <-  read.csv("../../Panama/PA_Places.csv", stringsAsFactors = F) %>%
  dplyr::filter(location_type == "province") %>%
  #the second bit of the name is actually NAME_3 for Panama becuase they are counties
  tidyr::separate(location, into=c("NAME_0", "NAME_1"), sep = "-", remove = F) %>% 
  # fix accent issue
  mutate(NAME_0 = toupper(NAME_0)) %>%
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  #drop imported
  filter(NAME_1 != "IMPORTADOS") %>%
  #most correspond to ADM1, but some are within Panama province
  mutate(NAME_1 = case_when(
    NAME_1  %in% c("SAN MIGUELITO", "METRO", "PANAMA NORTE", "PANAMA ESTE") ~ "PANAMA",
    NAME_1 == "P OESTE" ~ "PANAMA OESTE",
    NAME_1 == "NGABE BUGLE" ~ "NGOBE BUGLE",
    TRUE ~ NAME_1
  )) 

gadm.pan1 <- gadm.data %>%
  filter(GID_0 == "PAN") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_0 = toupper(NAME_0)) %>%
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT")))) %>%
  mutate(NAME_1 = gsub("~", "", NAME_1))

zika.pan.join1 <- left_join(zika.pan1, gadm.pan1,  by = c("NAME_0", "NAME_1")) %>%
  select(location, NAME_0, NAME_1, GID_1)

#combine all
key.pan.all <- bind_rows(zika.pan.join1, key.pan)
#manually add La Chorrera because it is a GID_2
la.chorerra <- data.frame(location = "Panama-Panama_Oeste-La_Chorrera", NAME_0 = "PANAMA", NAME_1 = "PANAMA OESTE", NAME_2 = "LA CHORRERA", GID_2 = "PAN.11.4_1")

key.pan.all <- bind_rows(key.pan.all, la.chorerra)

write.csv(key.pan.all, "../../Panama/PA_GADM_Key.csv", row.names = F)

#### Puerto Rico ####

zika.pr <- read.csv("../../Puerto_Rico/PR_Places.csv", stringsAsFactors = F) %>%
  filter(location_type == "municipality") %>%
  tidyr::separate(location, into=c("country", "NAME_0", "NAME_1"), sep = "-", remove = F) %>%
  mutate(NAME_1 = toupper(gsub("`|\\'", "", iconv(NAME_1, to="ASCII//TRANSLIT", sub = NA)))) %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1))  %>%
  mutate(NAME_0 = toupper(gsub("_", " ", NAME_0)))  %>%
  select(-country)
  
#there are weird issues with accents that I can't get to work in R, so I'm fixing them manually (poor form, I know)
zika.pr$NAME_1[6] <- "AÑASCO"
zika.pr$NAME_1[11] <- "BAYAMÓN"
zika.pr$NAME_1[15] <- "CANÓVANAS"
zika.pr$NAME_1[17] <- "CATAÑO"
zika.pr$NAME_1[23] <- "COMERÍO"
zika.pr$NAME_1[29] <- "GUÁNICA"
zika.pr$NAME_1[39] <- "JUANA DÍAZ"
zika.pr$NAME_1[43] <- "LAS MARÍAS"
zika.pr$NAME_1[45] <- "LOÍZA"
zika.pr$NAME_1[47] <- "MANATÍ"
zika.pr$NAME_1[50] <- "MAYAGÜEZ"
zika.pr$NAME_1[57] <- "PEÑUELAS"
zika.pr$NAME_1[60] <- "RINCÓN"
zika.pr$NAME_1[61] <- "RÍO GRANDE"
zika.pr$NAME_1[62] <- "SABANA GRANDE"
zika.pr$NAME_1[64] <- "SAN GERMÁN"
zika.pr$NAME_1[67] <- "SAN SEBASTIÁN"


gadm.pr <- gadm.data %>%
  filter(GID_0 == "PRI") %>%
  select(NAME_0, GID_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_0 = toupper(NAME_0)) %>%
  mutate(NAME_1 = toupper(NAME_1))

#join
zika.pr.join <- left_join(zika.pr, gadm.pr, 
                           by = c( "NAME_0", "NAME_1" = "NAME_1")) 

sum(is.na(zika.pr.join$GID_1)) # success == 0

#select columns and save
pri.key <- select(zika.pr.join, location, NAME_0, NAME_1,  GID_1)

write.csv(pri.key, "../../Puerto_Rico/PR_GADM_Key.csv", row.names = F)

#### US ####

zika.usa <- read.csv("../../United_States/US_Places.csv") %>%
  dplyr::filter(location_type %in% c("county")) %>%
  dplyr::filter(alt_name1 != "Not Reported") %>%
  mutate(NAME_0 = "United States") %>%
  rename(NAME_1 = "state_province") %>%
  mutate(NAME_2 = gsub(" County", "", alt_name1)) %>%
  mutate(NAME_2 = gsub("St", "Saint", NAME_2)) %>%
  mutate(NAME_2 = ifelse(NAME_2 == "DeSoto", "Desoto", NAME_2))


gadm.usa <- gadm.data %>%
  filter(GID_0 == "USA") %>%
  select(NAME_0, NAME_1, NAME_2, GID_2) %>%
  distinct() 

#join
usa.join2 <- left_join(zika.usa, gadm.usa, 
                           by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1", "NAME_2" = "NAME_2")) %>%
  select(location, NAME_0, NAME_1, NAME_2, GID_2) %>%
  distinct()

sum(is.na(usa.join2$GID_2)) # success == 0


#now georeference the states
zika.usa1 <- read.csv("../../United_States/US_Places.csv") %>%
  dplyr::filter(location_type %in% c("state")) %>%
  mutate(NAME_0 = "United States") %>%
  rename(NAME_1 = "state_province") %>%
  mutate(NAME_1 = gsub("_", " ", NAME_1)) %>%
  select(location, NAME_0, NAME_1)

gadm.usa1 <- gadm.data %>%
  filter(GID_0 == "USA") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() 

usa.join1 <- left_join(zika.usa1, gadm.usa1, by = c("NAME_0", "NAME_1"))

#create data for American Somoa (PR and USVI already have their own better data)
asm <- data.frame(location = "United_States-American_Samoa", NAME_0 = "United States", NAME_1 = "AMERICAN SOMOA", GID_0 = "ASM")

#combine with county data
key.usa <- bind_rows(usa.join1, usa.join2, asm) 
  
write.csv(key.usa, "../../United_States/US_GADM_Key.csv", row.names = F)

#### US VI ####

# Unlike PR and American Somoa, which don't go any further than GID_, virgin island data
# goes down to NAME_1 (individual islands)

zika.vir <- read.csv("../../USVI/USVI_Places.csv") %>%
  dplyr::filter(location_type %in% c("county")) %>%
  mutate(NAME_0 = "United States") %>%
  mutate(NAME_0 = toupper("Virgin Islands, U.S.")) %>%
  mutate(NAME_1 = toupper(gsub("_", " ", district_county_municipality)))

gadm.vir <- gadm.data %>%
  filter(GID_0 == "VIR") %>%
  select(NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #drop accents and capitalize
  mutate(NAME_0 = toupper(NAME_0)) %>%
  mutate(NAME_1 = toupper(NAME_1))

#join it all up
#join
zika.vir.join <- left_join(zika.vir, gadm.vir, 
                               by = c("NAME_0" = "NAME_0", "NAME_1" = "NAME_1")) 

sum(is.na(zika.vir.join$GID_1)) 

# get unique key and save
key.vir <- zika.vir.join %>%
  select(location, NAME_0, NAME_1, GID_1) %>%
  distinct() %>%
  #add in row for the whole territory
  bind_rows(data.frame(location = "United_States_Virgin_Islands", NAME_0 = "VIRGIN ISLANDS, U.S.", GID_0 = "VIR"))

write.csv(key.vir, "../../USVI/USVI_GADM_Key.csv", row.names = F)
