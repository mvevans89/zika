# Georeferencing Methods

All of the georeferencing is done automatically through the `create-spatial-keys.R` script. This creates an individual csv of the GADM ID keys for each country in the respective folder (Ex: `/Guatemala/GT_GADM_Key.csv)`. Each country has different spatial resolution, so please note the level of GADM ID (v. 3.6). See `map_data_by_country` folder for instructions on how to plot data in R.

GADM ID levels and any special notes are noted by country below.

## Argentina

Join level: ADM_1, "province"

## Brazil

Join level: ADM_1, "state"

## Colombia 

Join level: ADM_2, "municipality"

Many (171/1152) of these were in units that did not match the GADM administrative boundaries. These locations were manually georeferenced by MV Evans in February 2019. Their coordinates are saved in `zika-colombia-manual.csv`, which is used in the script to locate which GADM polygon those points do fall in. In order to get measures by ADM2, these will need to be aggregated up to that level.

## Dominican Republic

Join level: ADM_2,  "municipality"

## Ecuador

Join level: ADM_2 , "canton/county"

## El Salvador

Join level: ADM_1, "department"

Guatemala and Honduras are reported as departments in this dataset, but are imported cases and so are not included.

## France

This refers to French colonies in the Carribean. There are only five and they are assigned the code manually.

Join level: ADM_0, "country"

## Guatemala

Join level: ADM_1, "municipality"

Some reported locations did not match the ADM boundaries or were reported at a finer scale (ADM_2) than others. These were georeferenced following the deparment maps: https://guiadeguatemala.wordpress.com/guia-general-de-guatemala/. One potential issue is thathe region Peten matches the municipality Peten in name, so these cases could be double counted if they were reported at both levels.

## Haiti

Join level: ADM_3, "municipality"

## Mexico

Join level: ADM_1, "state"

## Nicaragua

Join level: ADM_2, "municipality"

This dataset also had locations listed as cities, which were not georeferenced as they seem to be a larger polygon than the municipalities.

## Panama

Join level: ADM_3, "county"

The Panama data has an issue in that some data is reported at the level of GID3 and others at GID2, and others at the level of a small town or neighborhood. For small towns/islands, the places have  been georeferenced manually by MV Evans in February 2019 and saved in `zika-panama-manual.csv`. These coordinates are then used to find out what GID3 shapefile they fall within and data should be aggregated up to GID_3 values before use.

## Puerto Rico

Join level: ADM_1,  "municipality"

There were a lot of join errors due to missing or additional accents. These are fixed manually. If the order of `PR_Places.csv` is updated, this georeferencing will need to be checked. All the municipalities of PR are included as oif Jan 2020, so this should not be an issue.

## United States of America

Join level: ADM_2, "county"

## United States Minor Islands (American Somoa)

Join level: ADM_0, "country"

Data on American Somoa is included in the US dataset, but American Somoa is a level 0 ADM in the GADM data. The code to add the island's GID_0 (`ASM`) is included, but no output is saved.

## United States Virgin Islands

Join level: ADM_1, "county"

