
# Load libraries
library(raster)
library(rgdal)
library(tidyverse)
library(lubridate)
library(magrittr)
library(ggplot2)
library(caTools)
library(foreach)
library(doSNOW)
library(stringr)
library(gtools)

# Initial setup
rm(list = ls())
setwd('W:/_chirps')
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_chirps')
options(scipen = 999)
cat('\f')

# Functions
createDate <- function(tbls){
  
  require(stringr)
  require(tidyverse)
  
  tb <- tbls %>%
          mutate(Year = str_sub(Date, 1, 4),
                 Month = str_sub(Date, 6, 7),
                 Day = str_sub(Date, 9, 10))
  
}

# Load data
fls <- list.files('_data/_tbl/_vlsCHIRPS/_daily', full.names = T, pattern = '.csv$')
dfs <- lapply(fls, FUN = read_csv)
yrs <- 1981:2016
dfs <- lapply(1:length(dfs), function(x) dfs[[x]] <- createDate(tbls = dfs[[x]]))

# Leap Years
leap_yrs <- leap_year(yrs) %>% yrs[.]
n_leapYrs <- length(leap_yrs)
yrs_leap <- data.frame(year = 1981:2016, leap = leap_year(yrs))
pos_leap <- grep(TRUE, yrs_leap$leap, value = F)
pos_noLeap <- grep(FALSE, yrs_leap$leap, value = F)

# Dataframes Not Leap Years
dfs_noLeap <- dfs[pos_noLeap]
ID_noLeap <- sort(rep(sort(rep(1:(365/5), 5)), 353))
dfs_noLeap <- lapply(1:length(dfs_noLeap), function(x) dfs_noLeap[[x]] <- mutate(dfs_noLeap[[x]], ID_5days = ID_noLeap))

# Dataframes Leap Years
dfs_leap <- dfs[pos_leap]
ID_Leap <- rep(1:(365/5), 5) %>% sort() %>% rep(., 353) %>% sort()
ID_Leap <- c(ID_Leap, sort(rep(73, 353)))

dfs_leap <- lapply(1:length(dfs_leap), function(x) dfs_leap[[x]] <- mutate(dfs_leap[[x]], ID_5days = ID_Leap))

# Join all the dataframe into only one
dfs_order <- c(dfs_noLeap, dfs_leap)
dfs_order <- bind_rows(dfs_order) %>% arrange(Year, Month, Day)

write.csv(dfs_order, '_data/_tbl/_vlsCHIRPS/_daily/df_all_daily.csv', row.names = F)

rm(list = ls())

# End Code
