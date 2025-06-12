# Load necessary libraries
library(quantmod)
library(dplyr)     
library(lubridate) # For easier date handling and end-of-month calculation

# Define the ticker symbol for FTSE 100
ticker <- "^FTSE" # FTSE 100 Index on Yahoo Finance

# Define the start and end dates
start_date <- "2005-01-01"
end_date <- "2025-01-01" # Data up to end of 2024

# --- Step 1: Get Daily Data ---
tryCatch({
  ftse100_daily_xts <- getSymbols(ticker,
                                  src = "yahoo",
                                  from = start_date,
                                  to = end_date,
                                  auto.assign = FALSE)
  
  if (is.null(ftse100_daily_xts) || nrow(ftse100_daily_xts) == 0) {
    stop(paste("No data retrieved for ticker:", ticker))
  }
  
  # Select the Adjusted Close price
  adj_close <- Ad(ftse100_daily_xts)
  colnames(adj_close) <- "Adjusted_Close"
  
  # --- Step 2: Calculate Daily Fractional Returns ---
  daily_returns_frac <- dailyReturn(adj_close, type = "arithmetic")
  colnames(daily_returns_frac) <- "Daily_Return_Fractional"
  
  daily_returns_frac <- daily_returns_frac[-1, , drop = FALSE] 
  
  if (nrow(daily_returns_frac) == 0) {
    stop("Not enough data to calculate daily returns.")
  }
  
  # --- Step 3: Square Daily Fractional Returns ---
  squared_daily_returns_frac <- daily_returns_frac^2
  colnames(squared_daily_returns_frac) <- "Squared_Daily_Return_Fractional"
  
  # --- Step 4: Calculate Quarterly Realized Variance (Sum of Squared Daily Fractional Returns) ---
  quarterly_realized_variance_frac <- apply.quarterly(squared_daily_returns_frac, FUN = sum, na.rm = TRUE)
  colnames(quarterly_realized_variance_frac) <- "Quarterly_Realized_Variance_Frac"
  
  # --- Step 5: Calculate Quarterly Realized Volatility (Fractional) ---
  quarterly_realized_volatility_frac <- sqrt(quarterly_realized_variance_frac)
  colnames(quarterly_realized_volatility_frac) <- "Quarterly_Realized_Volatility_Frac"
  
  # --- Step 6: Annualize Quarterly Realized Volatility (Fractional) ---
  annualized_realized_volatility_frac <- quarterly_realized_volatility_frac * 2
  colnames(annualized_realized_volatility_frac) <- "Annualized_Realized_Volatility_Frac"
  
  # --- Step 7: Convert to Percentage Terms ---
  annualized_realized_volatility_percent_xts <- annualized_realized_volatility_frac * 100 # Keep as xts for now
  colnames(annualized_realized_volatility_percent_xts) <- "Annualized_Realized_Volatility_Percent"
  
  # --- Step 8: Adjust Dates to Calendar End-of-Month for the Quarter ---
  # Get the original index dates (last trading day of the quarter)
  original_index_dates <- index(annualized_realized_volatility_percent_xts)
  
  # Convert these dates to the calendar end of their respective months
  # The dates from apply.quarterly are already in the correct end-of-quarter month
  # (March, June, September, December). This ensures it's the *calendar* end.
  calendar_eom_dates <- lubridate::ceiling_date(original_index_dates, unit = "month") - lubridate::days(1)
  
  # --- Convert to data frame with adjusted dates ---
  final_volatility_df <- data.frame(
    Date = calendar_eom_dates,
    Volatility_Percent = coredata(annualized_realized_volatility_percent_xts)
  )
  colnames(final_volatility_df) <- c("Date", "Stock.Volatility")
  
  # Print the head and tail to verify dates and values
  print("Head of Standard Annualized Quarterly Realized Volatility (Percentage Terms with Calendar EOM Dates):")
  print(head(final_volatility_df))
  print("Tail of Standard Annualized Quarterly Realized Volatility (Percentage Terms with Calendar EOM Dates):")
  print(tail(final_volatility_df))
  
  # Verify the class of the Date column
  print(paste("Class of Date column:", class(final_volatility_df$Date)))
  
  
}, error = function(e) {
  message("An error occurred:")
  message(e$message)
  if (exists("ticker") && grepl("HTTP error 404", e$message, ignore.case = TRUE)) {
    message(paste("This might be due to an incorrect ticker symbol ('", ticker,
                  "') or the data not being available for the specified period on Yahoo Finance.", sep=""))
  }
})

setwd("C:\\Users\\charl\\OneDrive\\Uni\\Masters\\Dissertation\\NLP_FS\\Data\\")
write.csv(final_volatility_df, 
          file = "FTSE100_Volatility.csv", 
          row.names = FALSE, 
          na = "NA")


