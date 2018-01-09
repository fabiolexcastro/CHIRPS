
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
library(trend)
library(sf)

# Initial setup
rm(list = ls())
setwd('W:/_chirps')
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_chirps')
options(scipen = 999)
cat('\f')

# Functions
calc_slp <- function(tb, id){
  
  # tb <- df_5days_zero_n; id <- 1
  
  # Calculate slope
  df_sub <- dplyr::filter(tb, ID_New == id)
  ts_slp <- ts(data = df_sub$count, start = 1981)
  slp <- extract2(df_sub, 3) %>% sens.slope(.)
  
  # Extract values from slope
  interval <- slp$conf.int %>% as.numeric()
  slp_val  <- slp$estimates %>% as.numeric()
  p_val <- slp$p.value
  
  df_values <- data.frame(ID_New = id, intervalo_min = interval[1], intervalo_max = interval[2], slope = slp_val, valor_p = p_val)
  
  # Generate graph
  gg <- ggplot(data = df_sub, aes(x = Year, y = count)) +
             geom_line() +
             xlab('') +
             ylab('Number of days without rainfall') +
             scale_y_continuous(limits = c(0, 50)) +
             scale_x_continuous(breaks = seq(1980, 2016, 5)) +
             stat_smooth(method = 'lm', se = TRUE, colour = 'black') +
             annotate('text', x = 2005, y = 10, label = paste0('tendency: ', round(df_values[,4], 2), '/year')) +
             annotate('text', x = 2005, y = 8, label = paste0('p value: ', round(df_values[,5], 2)))
  
  ggsave(plot = gg, filename = paste0('_png/_graphs/_dly/_consecutiveDryMonths/daysWithout_rainfall_', id, '.png'), width = 9, height = 7, units = 'in', dpi = 300)

  return(df_values)
  
}

# Load data
df <- read_csv('_data/_tbl/_vlsCHIRPS/_daily/df_all_daily.csv')
occ <- read_csv('_data/_csv/occHND_coffee_rmDup_clean.csv')
occ <- mutate(occ, ID_OK = 1:nrow(occ))
newID <- rep(1:nrow(occ), nrow(df) / nrow(occ))
df <- mutate(df, ID_New = newID)

df_5days <- df %>%
               group_by(ID_New, Year, ID_5days) %>%
               summarize(sum = sum(Value),
                         sd = sd(Value)) %>%
               ungroup() %>%
               mutate(sum = sum / 10,
                      sd = sd / 10)

write.csv(df_5days, '_data/_tbl/_vlsCHIRPS/_daily/df_all_daily5.csv', row.names = F)

# Days without rainfall
rm(df)
df_5days_zero <- filter(df_5days, sum == 0)
df_5days_zero_n <- df_5days_zero %>%
                      group_by(ID_New, Year) %>%
                      summarize(count = n()) %>%
                      ungroup()
nocc <- length(unique(df_5days_zero_n$ID_New))

lapply(1:nocc, function(x) calc_slp(df = df_5days_zero_n, ID_n = x))

# Graph
df_values_all <- lapply(1:nocc, function(x) calc_slp(tb = df_5days_zero_n, id = x))
df_sum <- bind_rows(df_values_all) %>% 
            tbl_df() %>%
            mutate(intervalo_min = round(intervalo_min, 2),
                   intervalo_max = round(intervalo_max, 2),
                   slope = round(slope, 2),
                   valor_p = round(valor_p, 2))
                   
df_sum_pvalues_noTrend <- filter(df_sum, valor_p >= 0.05) 
df_sum_pvalues_trend <- filter(df_sum, valor_p < 0.05) 

# Add data de si hay o no hay tendencia en el shapefile de puntos
occ_noTrend <- filter(occ, ID_OK %in% df_sum_pvalues_noTrend$ID_New) %>% mutate(trendDaysDry = 'No')
occ_Trend <- filter(occ, ID_OK %in% df_sum_pvalues_trend$ID_New) %>% mutate(trendDaysDry = 'Yes')

occ_allTrend <- rbind(occ_noTrend, occ_Trend) %>% arrange(ID_OK)

coordinates(occ_allTrend) <- ~ Longitude + Latitude
occ_allTrend <- st_as_sf(as(occ_allTrend, 'SpatialPointsDataFrame'))
occ_allTrend <- rename(occ_allTrend, trnDayDry = trendDaysDry)

st_write(occ_allTrend, dsn = '_data/_shp/_pnts', layer = 'occ_trendDryDays4', driver = 'ESRI Shapefile', update = TRUE)

# ############---------############ Test ############---------############ 
df_sub <- filter(df_5days_zero_n, ID == 1)
tsrs <- ts(data = df_sub$count, start = 1981)
slp_1 <- sens.slope(extract2(df_sub, 3)) # Como el intervalo de confianza no incluiye el valor negativo hay suficiente evidencia estadística para decir que hay una tendencia en los datos de 0.18
class(slp_1)



