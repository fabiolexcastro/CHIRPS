
# Questions:
# What is the number of days without rainfall at coffee locations in Honduras?
# What is the rainfall during dry/wet season at coffee locations?
  
# Load libraries
library(raster)
library(rgdal)
library(tidyverse)
library(lubridate)
library(magrittr)
library(compiler)
library(ggplot2)
library(caTools)
library(foreach)
library(doSNOW)

# Initial setup
rm(list = ls())
setwd('Z:/_chirps')
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_chirps')
options(scipen = 999)
cat('\f')

# Functions
dup_cell <- function(path_mask, path_df){
  
  mask <- raster(path_mask)
  df<- read.csv(path_df)
  
  cellNum <- raster::extract(mask, df[,c('Longitude', 'Latitude')], cellnumbers = T) 
  cells <- xyFromCell(mask, cellNum[,'cells'])
  dupvec <- duplicated(cells[,c('x', 'y')])
  occ_rmDupCell <- tbl_df(df[!dupvec,])
  occ_DupCell <- tbl_df(df[dupvec,])
  
  return(list(occ_rmDupCell, occ_DupCell))
  
}

extVal_yr <- function(fls, yr, occ){
  
  # fls <- fls
  # yr <- yrs[1]
  # occ <- occ
  
  print(yr)
  
  # Select raster files
  fls_sub <- grep(yr, fls, value = T)
  
  print('to Do Stack')
  
  lyrs <- stack(fls_sub)
  
  print(paste0(yr, ' to extract Data'))
  
  # Extraction data
  vls <- raster::extract(x = lyrs, y = occ[,c('Longitude', 'Latitude')]) %>% cbind(occ[,9], .)
  vls_tidy <- tbl_df(vls) %>% 
    gather(Date, Value, -ID) %>% 
    mutate(Date = as.character(Date))
  
  print(paste0(yr, ' to order Data'))
  
  # Order the date of the data
  dte <- extract2(vls_tidy[,2], 1)
  dte <- strsplit(dte, 'chirps.v2.0.') %>% unlist()
  dte <- dte[dte != '']
  vls_tidy <- mutate(vls_tidy, Date = dte)
  vls_tidy <- mutate(vls_tidy, Date = ymd(Date))
  
  # Write final table
  write.csv(vls_tidy, paste0('_data/_tbl/_vlsCHIRPS/_daily/df_', yr, '_daily.csv'), row.names = F)
  
  print(paste0(yr, ' Done'))
  
}

# Load data
occ <- read.csv('_data/_csv/occHND_coffee_rmDup_clean.csv') %>% tbl_df()
fls <- list.files('_data/_raster/_daily/_all/_hnd', full.names = T, pattern = '.tif$')
nms <- list.files('_data/_raster/_daily/_all/_hnd', full.names = F, pattern = '.tif$')
yrs <- 1981:2016
n.yrs <- length(yrs)
leap.yrs <- yrs[sapply(1981:2016, leap_year)]

# Process date
n.leap <- sum(sapply(1981:2016, leap_year))

# Remove duplicate by cell
occ_rmDup <- dup_cell(path_mask = '_data/_raster/_daily/_all/_hnd/chirps-v2.0.1981.01.01.tif', path_df = '_data/_csv/occHND_coffee_rmDup_clean.csv')
occ <- occ_rmDup[[1]]
occ <- mutate(occ, ID = 1:nrow(occ))

write.csv(occ, '_data/_csv/occHND_coffee_rmDup.csv', row.names = F)

# Execute main function
cl <- makeCluster(12)
registerDoSNOW(cl)
registerDoMC(26) # To Linux

foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach', 'tidyr', 'stringr'), .verbose = FALSE) %dopar% { 
  
  print(yrs[i])
  
  extVal_yr(fls = fls, yr = yrs[[i]], occ = occ )
  
}





############ END CODE ##############

##### 5 Days totally dry (prec = 0) - Each one day #####
days_1_dry <- filter(vls_tidy, Value == 0)
days_1_dry_n <- days_1_dry %>%
  group_by(ID) %>% 
  summarize(count = n())

total_days_dry_1 <- data.frame(year = yr, avg_days_dry = round(mean(days_1_dry_n$count), 0))

write.csv(total_days_dry_1, paste0('_data/_tbl/_vlsCHIRPS/tot_dry_1_', yr, '.csv'), row.names = F)

# Graph 
gg_days_dry_1 <- ggplot(data = days_1_dry_n, aes(x = ID, y = count)) + 
  geom_bar(stat = "identity") +
  xlab('') + 
  ylab(paste0('Count Days Dry ', yr)) +
  theme()

ggsave(plot = gg_days_dry_1, filename = paste0(path_grph, '/days_dry1_', yr, '.png'), width = 9, height = 7, units = 'in')

##### 5 Days totally dry (prec = 0) - Each five days #####
print(paste0(yr, ' to  make the summary by 5 days totally dry'))

# Summaryze by each 5 days (function: sum)
ID_days <- sort(rep(sort(rep(1:(365/5), 5)), nrow(vls)))
vls_tidy <- mutate(vls_tidy, ID_days = ID_days)
vls_5 <- vls_tidy %>%
  group_by(ID, ID_days) %>%
  summarize(avg = sum(Value)) %>%
  ungroup()

days_5_dry <- filter(vls_5, avg == 0)
days_5_dry_n <- days_5_dry %>%
  group_by(ID) %>%
  summarize(count = n())

total_days_dry_5 <- data.frame(year = yr, avg_days_dry = round(mean(days_5_dry_n$count), 0))

write.csv(total_days_dry_5, paste('_data/_tbl/_vlsCHIRPS/tot_dry_5_', yr, '.csv'), row.names = F)



# Parallelization


