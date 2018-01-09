
# Load libraries
library(raster)
library(rgdal)
library(tidyverse)

# Initial setup
rm(list = ls())
setwd('Z:/_chirps')
options(scipen = 999)
cat('\f')

# Load data
chr_wrl <- list.files('_data/_raster/_monthly/_world', full.names = T, pattern = '.bil$') %>%
              lapply(FUN = raster) %>%
              stack()
hnd <- shapefile('_data/_shp/_base/HND_adm0.shp')

# Cut raster (Honduras)
chr_hnd <- raster::crop(chr_wrl, hnd) %>% raster::mask(hnd)
chr_hnd <- unstack(chr_hnd)
dir.create('_data/_raster/_monthly/_hnd')
nms <- list.files('_data/_raster/_monthly/_world', pattern = '.bil$') %>% strsplit(., '.bil') %>% unlist()
Map('writeRaster', x = chr_hnd, filename = paste0('_data/_raster/_monthly/_hnd/', nms, '.asc'), overwrite = T)

