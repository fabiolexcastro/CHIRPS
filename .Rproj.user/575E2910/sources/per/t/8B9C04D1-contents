
# Load libraries
require(tidyverse)
require(magrittr)
require(ggplot2)
require(trend)
require(sf)
require(stringr)
require(data.table)

# Initial setup
rm(list = ls())
setwd('W:/_chirps')
options(scipen = 999)
cat('\f')

# Function to use
calc_season <- function(df_season, df_dly, id){
  
  # Proof
  # df_season <- dfs_3dry
  # df_dly <- df_dly
  # id <- 1

  # Daily
  dly <- df_dly %>% 
            mutate(Month = as.numeric(Month), 
                   Day = as.numeric(Day))
  
  print('To select the season')
  
  # Select by season
  ssn <- df_season %>%
            filter(ID == id) %>%
            extract2(4)
  ssn <- season_tb[grep(ssn, season_tb$season, value = F),]
  
  print('To tidy the result')
  
  # Filter and tidy by the season 
  result <- dly %>%
              filter(Month %in% c(extract2(ssn, 2), extract2(ssn, 3), extract2(ssn, 4))) %>%
              group_by(Year, ID_5days) %>%
              summarize(sum = sum(Value)) %>%
              ungroup() %>%
              mutate(sum = sum / 10,
                     ID = id) #the raw values are multiplied by 10
  
  print('Done...!')
  
  return(result)          
  
}

calc_sum_season <- function(df, season_name, val_name){
  
  # Proof
  # df <- dfs_all
  # season_name <- 'season_3wet'
  # val_name <- 'val_3wet'
  
  # Apply function
  df_sub <- df %>% select(ID, Lon, Lat, season_name, val_3wet) %>% tbl_df()
  
  print('To apply calc season function')
  
  df_ssn <- lapply(1:353, function(x) calc_season(df_season = df_sub, df_dly = df_dly, id = x))
  df_ssn <- bind_rows(df_ssn)
  
  print('To Make the summary')
  
  # Summarize table by dry days
  summ <- df_ssn %>%
            filter(sum == 0) %>%
            group_by(Year, ID) %>%
            summarize(count = n()) %>%
            ungroup()
  
  # Write final table
  write.csv(summ, paste0('_data/_tbl/_ssnCHIRPS/dry_days_', season_name, '.csv'), row.names = F)
  
  return(summ)
  
  print('Done!')
  
}

# Load data
df_dly <- read_csv('_data/_tbl/_vlsCHIRPS/_daily/df_all_daily.csv')
df_5dly <- read_csv('_data/_tbl/_vlsCHIRPS/_daily/df_all_daily5.csv')
season_tb <- read.csv('_tbl/seasons.csv')
season_tb6 <- read.csv('_tbl/seasons6.csv')

df_3dry <- read_csv('_data/_tbl/_precWC/months3_dry.csv')
df_3wet <- read_csv('_data/_tbl/_precWC/months3_wet.csv')
# df_6dry <- read_csv('_data/_tbl/_precWC/months6_dry.csv')
# df_6wet <- read_csv('_data/_tbl/_precWC/months6_wet.csv')

# dfs <- list(df_3dry, df_3wet, df_6dry, df_6wet); rm(df_3dry, df_3wet, df_6dry, df_6wet)
dfs <- list(df_3dry, df_3wet); rm(df_3dry, df_3wet)

# Rename the column names
nms <- c('3dry', '3wet', '6dry', '6wet')

dfs <- lapply(1:length(dfs), function(x) 
  
  setnames(dfs[[x]], old = c('ID', 'Longitude', 'Latitude', 'season', 'Prec_A'), new = c('ID', 'Longitude', 'Latitude', 'season', nms[[x]]))
  
)

# Join all the tables into only one
dfs_all <- do.call('cbind.data.frame', dfs)
dfs_all <- dfs_all[,c(1:5,9:10,14:15,19:20)]  
colnames(dfs_all) <- c('ID', 'Lon', 'Lat', 'season_3dry', 'val_3dry', 'season_3wet', 'val_3wet', 'season_6dry', 'val_6dry', 'season_6wet', 'val_6wet')

# Selection only for the quarter dry season
rslt_3dry_sum <- calc_sum_season(df = dfs_all, season_name = 'season_3dry', val_name = 'val_3dry')

# Selection only for the quarter wet season
rslt_3wet_sum <- calc_sum_season(df = dfs_all, season_name = 'season_3wet', val_name = 'val_3wet')



