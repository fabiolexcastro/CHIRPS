
# Questions:
# What is the number of days without rainfall at coffee locations in Honduras?
# What is the rainfall during dry/wet season at coffee locations?

# Load libraries
library(raster)
library(rgdal)
library(tidyr)
library(lubridate)
library(magrittr)
library(compiler)
library(caTools)
library(foreach)
library(doSNOW)
library(stringr)

# Initial setup
rm(list = ls())
setwd('Z:/_chirps')
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_chirps')
options(scipen = 999)
cat('\f')

# Function
extVal_yr <- function(fls, yr, occ){
  
  # fls <- fls; # yr <- 1981
  print(yr)
  
  # Select raster files
  fls_sub <- grep(yr, fls, value = T)
  lyrs <- stack(fls_sub)
  
  print(paste0(yr, ' to extract Data'))
  
  # Extraction data
  vls <- raster::extract(x = lyrs, y = occ[,c('Longitude', 'Latitude')]) %>% cbind(occ[,9], .)
  vls_tidy <- tbl_df(vls) %>% 
                gather(Date, Value, -ID) %>% 
                mutate(Date = as.character(Date))
  
  # Order the date of the data
  dates <- unique(extract2(vls_tidy, 2)) 
  dates_sub <- paste0(str_sub(dates, 11, 14)) %>% as.numeric, '-', str_sub(dates, 15, 16) %>% as.numeric)
  dates_sub <- sort(rep(dates_sub, nrow(vls)))
  vls_tidy <- mutate(vls_tidy, Date = dates_sub); rm(dates, dates_sub)
  
  write.csv(vls_tidy, paste0('_data/_tbl/_vlsCHIRPS/_monthly/df_', yr, '_monthly.csv'), row.names = F)
  
  print(paste0(yr, ' Done'))
  
}


# Load data
occ <- read.csv('_data/_csv/occHND_coffee_rmDup.csv') %>% tbl_df()
fls <- list.files('_data/_raster/_monthly/_hnd', full.names = T, pattern = '.asc$')
nms <- list.files('_data/_raster/_monthly/_hnd', full.names = F, pattern = '.asc$')
yrs <- 1981:2016
n.yrs <- length(yrs)

msk <- raster(fls[[1]]) * 0 + 1 
msk <- rasterToPolygons(msk)
pnt <- occ
coordinates(pnt) <- ~ Longitude + Latitude
pnt <- crop(pnt, msk)
plot(pnt, col = 'red', pch = 16, add = T)

occ <- cbind(coordinates(pnt), pnt@data) %>% tbl_df()

# Execute function
cl <- makeCluster(16) # To Windows
registerDoSNOW(cl)

registerDoMC(18) # To Linux

foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach', 'tidyr', 'magrittr', 'stringr'), .verbose = TRUE) %dopar% { 
  
  print(yrs[i])
  
  extVal_yr(fls = fls, yr = yrs[[i]], occ = occ )
  
}
