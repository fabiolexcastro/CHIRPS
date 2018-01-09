
# Load libraries
library(raster)
library(rgdal)
library(tidyverse)
library(lubridate)

# Initial setup
rm(list = ls())
setwd('W:/_chirps')
options(scipen = 999)
cat('/f')

# Functions
rec.list <- function(len){
  if(length(len) == 1){
    vector("list", len)
  } else {
    lapply(1:len[1], function(...) rec.list(len[-1])) #x <- rec.list(c(6, 32))
  }
}

ext.msk <- function(nameCode, inp, msk, out){
  
  sink(paste0('_codes/', nameCode))
  cat('import arcpy', fill = T)
  cat('from arcpy import env', fill = T)
  cat('from arcpy.sa import *', fill = T)
  # cat('os.system("cls")', fill = T)
  cat(paste0('input =', '"', inp, '"'), fill = T)
  cat(paste0('mask =', '"', msk, '"'), fill = T)
  cat(paste0('output =', '"', out, '"'), fill = T)
  cat('arcpy.env.workspace = input', fill = T)
  cat('rasters = arcpy.ListRasters("*", "")', fill = T)
  cat('for raster in rasters:', fill = T)
  cat('\tprint raster', fill = T)
  cat('\tarcpy.env.snapRaster = raster', fill = T)
  cat(paste0('\tcut = output +', "\\", "+ raster"), fill = T) 
  cat('\toutExtractbyMask = ExtractByMask(raster, mask)', fill = T)
  cat('\toutExtractbyMask.save(cut)', fill = T)
  cat('print "Done"')
  sink()
  
  system(paste0('python W:/_chirps/_codes/', nameCode))
  
  print('Done')
  
}

# Load data
occ <- read_csv('_data/_csv/occHND_coffee.csv')
nms <- list.files('_data/_raster/_daily/_32bits', full.names = F, pattern = '.tif$')
nms <- grep(paste0('chirps-v2.0.', 1981:2016, collapse = '|'), nms, value = T)
fls <- list.files('_data/_raster/_daily/_32bits', full.names = T, pattern = '.tif$') 
fls <- fls[-grep('2017', fls, value = F)]
pth.hnd <- 'W:/_chirps/_data/_shp/_base/HND_adm0.shp'
hnd <- shapefile(pth.hnd)

ext.msk(nameCode = 'test.py', inp = 'W:/_chirps/_data/_workspace/_inp', msk = pth.hnd, out = 'W:/_chirps/_data/_workspace/_out')


n.yrs <- length(1981:2016)
n.leap <- sum(sapply(1981:2016, leap_year))

length(fls)/365

# First 12 years
fls.1 <- grep(paste0(1981:(1981+12), collapse = '|'), fls, value = T) %>% stack()
objects()



fls[]



yrs <- lapply(1:length(nms), function(i){
  
  x[[i]] <- strsplit(nms[i], '[^[:digit:]]') %>% 
              unlist %>%
              as.numeric %>%
              .[!is.na(.)]
  
  return(x)
  
})


tst <- strsplit(nms[[1]], "[^[:digit:]]") %>% unlist() %>% as.numeric() %>% .[!is.na(.)]


unique(as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(fls)), ""))))


lyr <- lapply(fls, FUN = stack)



leap_year(2016)


#

stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time)


