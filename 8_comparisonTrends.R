
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(sf)
require(stringr)
require(data.table)

# Initial setup
rm(list = ls())
setwd('W:/_chirps')
options(scipen = 999)
cat('\f')

# Function to use
match_ssn <- function(shp_ch, shp_wc){
  
  # Clean dataframes
  shp_ch <- shp_ch %>% dplyr::select(ID_New, trndDyD) %>% rename(ID_OK = ID_New, trend_ch = trndDyD)
  shp_wc <- shp_wc %>% dplyr::select(ID_OK, trnDayDry) %>% rename(trend_wc = trnDayDry)
  
  # Making the match
  diff <- !(shp_ch$trend_ch %in% shp_wc$trend_wc)
  nF <- which(diff == FALSE) %>% length()
  nT <- which(diff == TRUE) %>% length()
  
  return(list(diff, nF, nT))
  
}

# Load data
dry_ssn_ch <- read_sf('_shp/_chirps/dry_3season_chirps.shp')
wet_ssn_ch <- read_sf('_shp/_chirps/wet_3season_chirps.shp')
dry_ssn_wc <- read_sf('_data/_shp/_pnts/occ_trendDryDays.shp')

# Or
load(file = '_rData/dry_3season_chirps.rData')
load(file = '_rData/wet_3season_chirps.rData')
load(file = '_rData/dry_season_wc.rData')

# Apply Function
dry <- match_ssn(shp_ch = dry_ssn_ch, shp_wc = dry_ssn_wc)
wet <- match_ssn(shp_ch = wet_ssn_ch, shp_wc = dry_ssn_wc)

1+1
