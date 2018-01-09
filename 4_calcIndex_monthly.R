
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
dr_stress <- function(PREC, p_thresh){
  
  runs <- rle(PREC < p_thresh)
  cons_days <- max(runs$lengths[runs$values == 1], na.rm = TRUE)
  
  return(cons_days)
  
}

dry_byOcc <- function(df, yr){
  
  # df <- df; yr <- '1981'
  
  x <- df[grep(yr, df$Date, value = F),]
  z <- vector(mode = 'numeric', length = 0)
  
  for(i in 1:length(unique(df$ID))){
    
    y <- filter(x, ID == i)
    z[i] <- dr_stress(PREC = extract2(y, 3), p_thresh = 100)
    
  }
  
  return(z)
  
}

createGraph <- function(df, ID_n){
  
  gg <- ggplot(data = filter(df, ID == ID_n), aes(x = yr, y = nDry_days)) + 
    geom_line() + 
    xlab('') +
    ylab('Number of consecutive months < 100 mm') +
    scale_y_continuous(limits = c(0, 6)) +
    scale_x_continuous(breaks = seq(1980, 2015, 5))
  
  ggsave(plot = gg, filename = paste0('_png/_graphsMonthly/consecutiveDry_100_months_ID_', ID_n, '.png'), units = 'in', width = 9, height = 7)
  
}

# Load data
fls <- list.files('_data/_tbl/_vlsCHIRPS/_monthly', full.names = T, pattern = '.csv$')
dfs <- lapply(fls, FUN = read_csv)
df  <- do.call('rbind', dfs)
yrs <- 1981:2015; mixedsort(df$Date)
occ <- read.csv('_data/_csv/occHND_coffee_rmDup_clean.csv') %>% tbl_df()

df <- mutate(df, Year = str_sub(df$Date, 1, 4))
months <- strsplit(df$Date, split = "-")
months <- as.numeric(unlist(lapply(months, function(x)x[[2]])))
df <- mutate(df, Month = months); rm(months)
df <- arrange(df, Year, Month)

# Execute functions
dry_mnth_calc <- list()

for(i in 1:length(yrs)){
  
  print(yrs[i])
  
  dry_mnth_calc[[i]] <- dry_byOcc(df = df, yr = yrs[i]) 
  
}

df_dryMnth <- list()

for(i in 1:length(dry_mnth_calc)){
  
  df_dryMnth[[i]] <- data.frame(yr = yrs[i], nDry_days = dry_mnth_calc[[i]]) %>% 
                        mutate(ID = 1:nrow(occ)) %>%
                        tbl_df()
  
} 

df_dryMnth <- bind_rows(df_dryMnth) %>% tbl_df()#;df_dryMnth <- dplyr::filter(df_dryMnth, nDry_days != -Inf)

write.csv(df_dryMnth, '_data/_tbl/_sumCHIRPS/n_dryDays_monthly.csv', row.names = F)

# Graph
lapply(1:nrow(occ), function(x){
  
  createGraph(df = df_dryDays, ID_n = x)

})
    
# End code


#######################

df_1981 <- df[grep('1981', df$Date, value = F),]
tst <- filter(df_1981, ID == 1)
dr_stress(PREC = tst$Value, p_thresh = 100)

runs <- rle(df$Value < 100)
cons_month <- max(runs$lengths[runs$values == 1], na.rm = TRUE)


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
