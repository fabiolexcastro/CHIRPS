
# Load libraries
require(tidyverse)
require(magrittr)
require(ggplot2)
require(trend)
require(sf)
require(stringr)
require(data.table)
require(raster)
require(rgdal)

# Initial setup
rm(list = ls())
setwd('W:/_chirps')
options(scipen = 999)
cat('\f')

# Function to use
calc_slope <- function(df, id, folder, nme){
  
  # df <- df_3dry; id <- 1
  
  # Calculate slope
  df_sub <- dplyr::filter(df, ID == id)
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
          scale_y_continuous(limits = c(0, 20)) +
          scale_x_continuous(breaks = seq(1980, 2016, 5)) +
          stat_smooth(method = 'lm', se = TRUE, colour = 'black') +
          annotate('text', x = 2005, y = 15, label = paste0('tendency: ', round(df_values[,4], 2), '/year')) +
          annotate('text', x = 2005, y = 14, label = paste0('p value: ', round(df_values[,5], 2)))
  
  ggsave(plot = gg, filename = paste0('_png/_graphs/_dly/_drySeason/', folder, '/', nme,  'daysWithout_rainfall_', id, '.png'), width = 9, height = 7, units = 'in', dpi = 300)
  
  return(df_values)
  
}

process_slope <- function(df_ssn, folder, nme){
  
  # df_ssn <- df_3dry
  # nme <- 'dry_3season_chirps'
  
  # Apply function
  df_values_all <- lapply(c(1:353), function(x){
    
    df_sub <- filter(df_ssn, ID == x)
    
    if(nrow(df_sub) == 0){
      
      print(paste0('Not done ', x))
      
    } else{
      
      result <- calc_slope(df = df_sub, id = x, folder = folder, nme = nme)
      
    }
    
  })
  
  # Select only the dataframes
  df <- lapply(df_values_all, function(df) df_values_all[sapply(df_values_all, is.data.frame)])[[1]]
  df <- do.call('rbind', df) %>% tbl_df()
  
  print('To tidy the dataframe')
  
  # Select trend and not trend
  df <- df %>% mutate(intervalo_min = round(intervalo_min, 2),
                      intervalo_max = round(intervalo_max, 2),
                      slope = round(slope, 2),
                      valor_p = round(valor_p, 2))
  
  df_noTrend <- filter(df, valor_p >= 0.05)
  df_Trend <- filter(df, valor_p < 0.05)
  
  df_1 <- filter(df, ID_New %in% df_noTrend$ID_New) %>% mutate(trndDayDry = 'No')
  df_2 <- filter(df, ID_New %in% df_Trend$ID_New)  %>% mutate(trndDayDry = 'Yes')
  
  df <- rbind(df_1, df_2)
  df <- right_join(df, coords, by = c('ID_New' = 'ID'))
  # df$trndDayDry <- ifelse(is.na(df$trndDayDry), 'No', df$trndDayDry)
  
  shp <- df
  
  # Dataframe to shapefile
  coordinates(shp) <- ~ Longitude + Latitude
  
  # Write shapefile
  writeOGR(shp, dsn = '_shp/_chirps', layer = nme, driver = 'ESRI Shapefile')
  
  return(shp)
  
}

# Load data
coords <- read_csv('_data/_csv/occHND_coffee_rmDup_clean.csv') %>% dplyr::select(ID, Longitude, Latitude)
df_3dry <- read_csv('_data/_tbl/_ssnCHIRPS/dry_days_season_3dry.csv')
df_3wet <- read_csv('_data/_tbl/_ssnCHIRPS/dry_days_season_3wet.csv')

# Apply functions
df_calc_3dry <- process_slope(df_ssn = df_3dry, folder = '_season_3dry', nme = 'dry_3season_chirps')
df_calc_3wet <- process_slope(df_ssn = df_3wet, folder = '_season_3wet', nme = 'wet_3season_chirps')


