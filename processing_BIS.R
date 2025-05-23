# Script to get and prepare data from BIS
library(tidyverse)
library(lubridate)

BIS_data <- function(release_dates){
  urls = c("https://stats.bis.org/api/v2/data/dataflow/BIS/WS_DPP/1.0/Q.GB.0.1.2.1.0.0?format=csv", # Real YoY House prices
           "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_DSR/1.0/Q.GB.P?format=csv", # PNFC DSR
           "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_DSR/1.0/Q.GB.H?format=csv") # Household DSR
  
  # From sentiment index
  start_date <- min(release_dates) %m-% months(2)
  end_date <- max(release_dates)
  
  
  data_bis = list()
  
  for (i in 1:length(urls)) {
    data_bis[[i]] <- read.csv(urls[i])
  }
  
  
  process_BIS_data <- function(df, start_dt, end_dt) {
    # Helper function to sort the dates out
    # Check if TIME_PERIOD and OBS_VALUE columns exist
    if (!all(c("TIME_PERIOD", "OBS_VALUE") %in% names(df))) {
      stop("Input data frame must contain 'TIME_PERIOD' and 'OBS_VALUE' columns.")
    }
    
    processed_df <- df %>%
      select(TIME_PERIOD, OBS_VALUE) %>%
      mutate(
        year_quarter = zoo::as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"),
        Date = as.Date(year_quarter, frac = 1) # Last day of the quarter
      ) %>%
      select(Date, OBS_VALUE) %>%
      filter(Date >= start_dt & Date <= end_dt) # Filter by date range
    
    return(processed_df)
  }
  
  house_price_start <- start_date - 365 # As do logged YoY changes need an extra year
  house_prices <- process_BIS_data(data_bis[[1]], house_price_start, end_date)
  pnfc_dsr     <- process_BIS_data(data_bis[[2]], start_date, end_date)
  household_dsr<- process_BIS_data(data_bis[[3]], start_date, end_date)
  
  house_prices <- house_prices %>% 
    arrange(Date) %>% 
    mutate(OBS_VALUE = log(OBS_VALUE) - lag(log(OBS_VALUE), 4)) %>%  # Log difference
    drop_na() # Drop NAs (extra year) so back to same dates as the others
  
  BIS_df <- house_prices %>%
    left_join(pnfc_dsr, by = "Date") %>%
    left_join(household_dsr, by = "Date") %>%
    rename(
      house_price_yoy = OBS_VALUE.x,
      pnfc_dsr = OBS_VALUE.y,
      household_dsr = OBS_VALUE
    ) %>%
    mutate(
      house_price_yoy = house_price_yoy * 100, # Convert to percentage followinh B&P and Correa
    )
  
  return(BIS_df)
}
