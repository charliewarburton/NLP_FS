library(vars)
library(tidyverse)
library(lubridate)

# standardise data helper function
standardise <- function(x){
  # If not numeric, return as is
  if (!is.numeric(x)) {
    return(x)
  }
  return((x - mean(x))/sd(x))
}

setwd("C:\\Users\\charl\\OneDrive\\Uni\\Masters\\Dissertation\\NLP_FS")

df <- read.csv("full_results.csv")

# Assuming released on 1st month for convinience and data cut off
release_dates <- c(
  "2024-11-01", "2024-06-01",
  "2023-12-01", "2023-07-01",
  "2022-12-01", "2022-07-01",
  "2021-12-01", "2021-07-01",
  "2020-12-01", "2020-08-01",
  "2019-12-01", "2019-07-01",
  "2018-11-01", "2018-06-01",
  "2017-11-01", "2017-06-01",
  "2016-11-01", "2016-07-01",
  "2015-12-01", "2015-07-01",
  "2014-12-01", "2014-06-01",
  "2013-11-01", "2013-06-01",
  "2012-11-01", "2012-06-01",
  "2011-12-01", "2011-06-01",
  "2010-12-01", "2010-06-01",
  "2009-12-01", "2009-06-01",
  "2008-10-01", "2008-05-01",
  "2007-10-01", "2007-04-01",
  "2006-07-01"
)
# order in reverse
release_dates <- as.Date(release_dates, format = "%Y-%m-%d") %>% 
  rev()
  
source("processing_BIS.R")
BIS_df <- BIS_data(release_dates)

# Grpah showing pos/neg over time
df_full <- df %>% 
  group_by(Report) %>%
  summarise(neg = sum(Sentiment.Label == "negative"),
            pos = sum(Sentiment.Label == "positive"),
            total = n())

df_full$Report <- as.Date(release_dates, format = "%Y-%m-%d")


point_graph <- function(df, title = "Sentiment over time"){
  df_graph <- df %>% 
    pivot_longer(cols = c(neg, pos), names_to = "Sentiment", values_to = "Count")
  
  graph <- ggplot(df_graph, aes(x = Report, y = Count, color = Sentiment)) +
    geom_point() +
    labs(title = title,
         x = "Report",
         y = "Count") +
    theme_minimal()
  
  return(graph)
}

standardise <- function(x){
  return((x - mean(x))/sd(x))
}

df_87 <- df %>% 
  filter(Sentiment.Score >= 0.87) %>% 
  group_by(Report) %>%
  summarise(neg = sum(Sentiment.Label == "negative"),
            pos = sum(Sentiment.Label == "positive"))
df_87$Report <- as.Date(release_dates, format = "%Y-%m-%d")
point_graph(df_87, "Sentiment over time (Sentiment Score >= 0.87)")

df_87_60 <- df %>% 
  filter(Sentiment.Label == "negative" & Sentiment.Score >= 0.87 | 
           Sentiment.Label == "positive" & Sentiment.Score >= 0.60) %>% 
  group_by(Report) %>%
  summarise(neg = sum(Sentiment.Label == "negative"),
            pos = sum(Sentiment.Label == "positive"))
df_87_60$Report <- as.Date(release_dates, format = "%Y-%m-%d")
  
point_graph(df_87_60, "Sentiment over time (Sentiment Score >= 0.87 & 0.60)")


# Sentiment index in Correa is (neg-pos)/total

df_full <- df_full %>% 
  mutate(Sentiment.Index = (neg - pos)/total)


df_87$total <- df_full$total
df_87$Sentiment.Index <- (df_87$neg - df_87$pos)/df_87$total

df_87_60$total <- df_full$total
df_87_60$Sentiment.Index <- (df_87_60$neg - df_87_60$pos)/df_87_60$total

sentiment_graph <- function(df, title = "Sentiment Index over time"){
  graph <- ggplot(df, aes(x = Report, y = Sentiment.Index)) +
    geom_point() +
    labs(title = title,
         x = "Report",
         y = "Sentiment Index") +
    theme_minimal()
  
  return(graph)
}

sentiment_graph(df_full, "Sentiment Index over time")
sentiment_graph(df_87, "Sentiment Index over time (Sentiment Score >= 0.87)")
sentiment_graph(df_87_60, "Sentiment Index over time (Sentiment Score >= 0.87 & 0.60)")

# Compare sentiment indexes with the different thresholds
df_sentiment_indexs <- data.frame(Report = df_full$Report,
                                  Full = df_full$Sentiment.Index,
                                  Both_87 = df_87$Sentiment.Index,
                                  Neg_87_Pos_60 = df_87_60$Sentiment.Index)

# Pivoting longer for ggplot
df_sentiment_indexs <- df_sentiment_indexs %>%
  pivot_longer(cols = c(Full, Both_87, Neg_87_Pos_60), 
               names_to = "Threshold", values_to = "Sentiment.Index")

ggplot(df_sentiment_indexs, aes(x = Report, y = Sentiment.Index, color = Threshold, group = Threshold)) +
  geom_line() +
  geom_point() +
  labs(title = "Sentiment Index over time",
       x = "Report",
       y = "Sentiment Index") +
  theme_minimal()


# Compare my sentiment index with the one in Correa
compare_with_correa <- function(df, title = "Comparison of Sentiment Index"){
  setwd("C:\\Users\\charl\\OneDrive\\Uni\\Masters\\Dissertation\\NLP_FS\\Data\\")
  correa_df <- readxl::read_xlsx("correa_data.xlsx", sheet = "Sheet1")
  # For some reason, only had 1 2017 result
  
  comparison_df <- df[1:21, ]
  correa_df <- correa_df[15:35, ]
  comparison_df$correa_index <- correa_df$`fs_score (%)`*-1 # Negative as Correa is opposite
  
  # Standardise
  comparison_df <- comparison_df %>%
    mutate_at(vars(correa_index, Sentiment.Index ), standardise) # standardise
  
  out <- ggplot(comparison_df, aes(x = Report)) +
    geom_line(aes(y = correa_index, color = "Correa")) +
    geom_line(aes(y = Sentiment.Index, color = "Mine")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title,
         x = "Report",
         y = "Sentiment Index") +
    theme_minimal()
  
  return(out)
  
}


plot <- compare_with_correa(df_87_60, "Comparison of Unfiltered Sentiment Index (Sentiment Score >= 0.87 & 0.60)")
plot

# Filter for words in Correa's dictionary in text chunk
correa_dict <- readxl::read_xlsx("correa_dictionary.xlsx", sheet = "1 FS Dictionary")
# Add british spellings
british_spellings = c(
  'destabilising',
  'favourable',
  'favourably',
  'jeopardise',
  'lacklustre',
  'stabilise',
  'stabilised',
  'stabilising',
  'unfavourable'
)
correa_words <- c(correa_dict$Word, british_spellings)

# filter text chunks for words in dictionary
pattern <- paste0("\\b(", paste(correa_words, collapse="|"), ")\\b")

df_filtered <- df %>%
  filter(str_detect(Text.Chunk, regex(pattern, ignore_case = TRUE)))

# Like the correa paper, using total of all chunks
df_totals <- df %>% 
  group_by(Report) %>%
  summarise(total = n())


df_filtered_full <- df_filtered %>% 
  group_by(Report) %>%
  summarise(neg = sum(Sentiment.Label == "negative"),
            pos = sum(Sentiment.Label == "positive"))
            # total = n()) %>%  # Instead of total of all chunks, total of filtered chunks
df_filtered_full$total <- df_totals$total
df_filtered_full <- df_filtered_full %>%
  mutate(Sentiment.Index = (neg - pos)/total)
df_filtered_full$Report <- as.Date(release_dates, format = "%Y-%m-%d")

df_filtered_87_60 <- df_filtered %>% 
  filter(Sentiment.Label == "negative" & Sentiment.Score >= 0.87 | 
           Sentiment.Label == "positive" & Sentiment.Score >= 0.60) %>% 
  group_by(Report) %>%
  summarise(neg = sum(Sentiment.Label == "negative"),
            pos = sum(Sentiment.Label == "positive"))
            #total = n()) %>% 
df_filtered_87_60$total <- df_totals$total
df_filtered_87_60 <- df_filtered_87_60 %>%
  mutate(Sentiment.Index = (neg - pos)/total)
df_filtered_87_60$Report <- as.Date(release_dates, format = "%Y-%m-%d")


# Now compare with Correa's sentiment index
plot_filtered <- compare_with_correa(df_filtered_full, "Comparison of Filtered Sentiment Index")
plot_filtered

plot_filtered_87_60 <- compare_with_correa(df_filtered_87_60, "Sentiment Index over time (Sentiment Score >= 0.87 & 0.60)")
plot_filtered_87_60

df_sentiment_indexs <- data.frame(Report = df_full$Report,
                                  Full = df_full$Sentiment.Index,
                                  Both_87 = df_87$Sentiment.Index,
                                  Neg_87_Pos_60 = df_87_60$Sentiment.Index,
                                  Filtered_full = df_filtered_full$Sentiment.Index,
                                  Filtered_87_60 = df_filtered_87_60$Sentiment.Index)
df_sentiment_indexs <- df_sentiment_indexs %>%
  mutate_at(vars(-Report), standardise)

df_sentiment_indexs <- df_sentiment_indexs %>%
  pivot_longer(cols = -c(Report), 
               names_to = "Threshold", values_to = "Sentiment.Index")


ggplot(df_sentiment_indexs, aes(x = Report, y = Sentiment.Index, color = Threshold, group = Threshold)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Sentiment Index over time",
       x = "Report",
       y = "Sentiment Index") +
  theme_minimal()

# ----------------------------------- #
##### Get Sub Indexes #####
# ----------------------------------- #


create_sub_index <- function(df_filtered, df_unfiltered , agent_label){
  # Function to create sub index for a given agent label
  # Need an unfiltered df to get total counts to follow Correa
  totals_sub <- df_unfiltered %>% 
    filter(Agent.Label == agent_label) %>%
    group_by(Report) %>%
    summarise(total = n())
  
  sub_df <- df_filtered %>% 
    filter(Agent.Label == agent_label) %>%
    filter(Sentiment.Label == "negative" & Sentiment.Score >= 0.87 | 
             Sentiment.Label == "positive" & Sentiment.Score >= 0.60) %>%
    group_by(Report) %>%
    summarise(neg = sum(Sentiment.Label == "negative"),
              pos = sum(Sentiment.Label == "positive"))
  
  sub_df$total <- totals_sub$total
  sub_df <- sub_df %>%
    mutate(Sentiment.Index = (neg - pos)/total)
  sub_df$Report <- as.Date(release_dates, format = "%Y-%m-%d")
  
  return(sub_df)
} 

financial_sub <- create_sub_index(df_filtered, df, "Financial Sector")
household_sub <- create_sub_index(df_filtered, df, "Households")







# ----------------------------------- ##
##### Get financial cycle data #####
# ----------------------------------- #
setwd("C:\\Users\\charl\\OneDrive\\Uni\\Masters\\Dissertation\\NLP_FS\\Data\\")
data <- readxl::read_xlsx("cleaned_data.xlsx", sheet = "Sheet1")

cols <- c("Date", "Total.Credit.To.GDP", "Credit.To.GDP.Gap", "PNF.Credit.Growth", "External.Debt.To.GDP")
colnames(data) <- cols

data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

CDS <- readxl::read_xlsx("cleaned_data.xlsx", sheet = "CDS")
colnames(CDS) <- c("Date", "CDS")

price_book <- readxl::read_xlsx("cleaned_data.xlsx", sheet = "PB")
colnames(price_book) <- c("Date", "Price.Book.Ratio")

calculate_monthly_avg <- function(df, date_col, value_col, date_format = "%Y-%m-%d"){
  df_processed <- df # Work on a copy of the dataframe
  
  # Standardize the date column to 'TempDate' and ensure it's Date type.
  # This involves creating/updating a 'TempDate' column from the specified 'date_col'.
  # If 'date_col' is not already a Date object, it will be converted.
  if (inherits(df_processed[[date_col]], "Date")) {
    # If the source column is already Date type, directly assign it to TempDate
    df_processed$TempDate <- df_processed[[date_col]]
  } else {
    # If the source column is not Date type (e.g., character, factor), attempt conversion
    message(paste0("Column '", date_col, "' is not of Date type. Attempting conversion using format: '", date_format, "'."))
    
    # Explicitly convert to character first, in case it's a factor or other non-character type
    date_values_to_convert <- as.character(df_processed[[date_col]])
    
    df_processed$TempDate <- tryCatch({
      as.Date(date_values_to_convert, format = date_format)
    }, error = function(e) {
      stop(paste0("Date conversion failed for column '", date_col, "'. ",
                  "Please ensure the data matches the format '", date_format, "' or is already a Date object. Original error: ", e$message), call. = FALSE)
    })
  }
  
  # Check if conversion resulted in NAs, which indicates a format mismatch or bad data
  if (any(is.na(df_processed$TempDate))) {
    warning(paste0("Some values in '", date_col, "' could not be converted to dates (resulted in NAs after attempt). ",
                   "These rows will be excluded from the analysis. Please check date format and values."))
    df_processed <- df_processed[!is.na(df_processed$TempDate), ] # Remove rows where TempDate is NA
    if(nrow(df_processed) == 0) {
      stop("All date conversions resulted in NA or all rows with valid dates were filtered out. Please check your date column and format.", call. = FALSE)
    }
  }
  
  
  # --- Calculation ---
  monthly_avg_df <- df_processed %>%
    # Create a year-month column for grouping
    mutate(YearMonth = floor_date(TempDate, "month")) %>%
    # Group by this new YearMonth column
    group_by(YearMonth) %>%
    # Calculate the average of the specified value column and get the last day of the month
    summarise(
      MonthlyAverage = mean(.data[[value_col]], na.rm = TRUE), # Use .data[[value_col]] for dynamic column name
      .groups = 'drop' # Drop grouping structure after summarising
    ) %>%
    # Set the date to the last day of the month
    mutate(MonthEnd = ceiling_date(YearMonth, "month") - days(1)) %>%
    # Select and rename columns for the final output
    select(MonthEnd, MonthlyAverage)
  colnames(monthly_avg_df) <- c("Date", value_col) # Rename the columns to match the original
  return(monthly_avg_df)
}
  
price_book <- calculate_monthly_avg(price_book, "Date", "Price.Book.Ratio")
CDS <- calculate_monthly_avg(CDS, "Date", "CDS")

# Join to other data
data <- data %>%
  left_join(price_book, by = "Date") %>%
  left_join(CDS, by = "Date")

# Corp bond spreads
corp_bond_spreads <- readxl::read_xlsx("cleaned_data.xlsx", sheet = "CorpBonds") %>% 
  na.omit() %>%  # Clears rows with GBHY 
  select(Date, `GBP investment-grade`, `GBP high-yield`)
colnames(corp_bond_spreads) <- c("Date", "GBP.Investment.Grade", "GBP.High.Yield")
# Convert Date to Date type
corp_bond_spreads$Date <- as.Date(corp_bond_spreads$Date, format = "%Y-%m-%d")
corp_bond_spreads_IG <- calculate_monthly_avg(corp_bond_spreads, "Date", "GBP.Investment.Grade")
corp_bond_spreads_HY <- calculate_monthly_avg(corp_bond_spreads, "Date", "GBP.High.Yield")
corp_bond_spreads <- corp_bond_spreads_IG %>%
  left_join(corp_bond_spreads_HY, by = "Date")
# Join corp bond spreads
data <- data %>%
  left_join(corp_bond_spreads, by = "Date")
# Join BIS data
data <- data %>%
  left_join(BIS_df, by = "Date")

# Real GDP from FRED
real_GDP <- read.csv("real_GDP.csv")
colnames(real_GDP) <- c("Date", "Real.GDP")
real_GDP$Date <- as.Date(real_GDP$Date, format = "%Y-%m-%d")
real_GDP$QoQ <- (real_GDP$Real.GDP - lag(real_GDP$Real.GDP, 1))/lag(real_GDP$Real.GDP, 1) # QoQ growth
real_GDP$QoQ <- real_GDP$QoQ * 100
real_GDP <- real_GDP %>%
  select(Date, QoQ) %>% 
  na.omit()
# Move dates back 1 day as FRED data is start of next month whereas others are end of month
real_GDP$Date <- real_GDP$Date - days(1)
colnames(real_GDP) <- c("Date", "Real.GDP.QoQ")
data <- data %>%
  left_join(real_GDP, by = "Date")

# SRISK data from VLAB
usdgbp <- read.csv("EXUSUK.csv")
usdgbp$observation_date <- as.Date(usdgbp$observation_date, format = "%Y-%m-%d")
colnames(usdgbp) <- c("Date", "USD.GBP")
usdgbp$Date <- usdgbp$Date- days(1) # FRED first of month whereas other data end of month

# Folowing Correa, take SRISK and divide by nominal GDP (Data from FRED).
# SRISK is in $ so converting UK GDP to $
SRISK <- read.csv("srisk.csv")
colnames(SRISK) <- c("Date", "SRISK.Raw", "UKNGDP")
SRISK <- SRISK %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  left_join(usdgbp, by = "Date") %>%  
  mutate(USD.GDP = UKNGDP * USD.GBP) %>% # Convert to USD
  mutate(SRISK = SRISK.Raw/USD.GDP) %>% 
  select(Date, SRISK) %>% 
  na.omit()



data <- data %>%
  left_join(SRISK, by = "Date")

# Add VIX (daily series from FRED)
vix <- read.csv("VIXCLS.csv") %>% 
  na.omit()
vix$observation_date <- as.Date(vix$observation_date, format = "%d/%m/%Y")
vix <- calculate_monthly_avg(vix, "observation_date", "VIXCLS")
colnames(vix) <- c("Date", "VIX")
# Join VIX
data <- data %>%
  left_join(vix, by = "Date") 

# Downloaded FTSE data and processed into vol in processing_ftse.R
ftse_vol <- read.csv("FTSE100_Volatility.csv")
ftse_vol$Date <- as.Date(ftse_vol$Date, format = "%Y-%m-%d")
# Join FTSE volatility
data <- data %>% 
  left_join(ftse_vol, by = "Date")

join_data <- function(sentiment_df, data){
  
  # Function to join sentiment data with financial cycle data
  
  joined_df <- sentiment_df %>%
    left_join(data %>% 
                rename(Financial_Date = Date), # Rename to prevent conflicts
              by = join_by(closest(Report >= Financial_Date)) )# Ensuring past data is used
    
  joined_df <- joined_df %>%
    dplyr::select(-c(pos, neg, total, Financial_Date)) # Removing unneeded columns
  
  return(joined_df)
}
df_87_60_joined <- join_data(df_87_60, data)
df_87_joined <- join_data(df_87, data)
df_filtered_full_joined <- join_data(df_filtered_full, data)
df_filtered_87_60_joined <- join_data(df_filtered_87_60, data)

# Convert to Z values as different scales

standardise_timeseries_diff <- function(df){
  # Preprocess data for VAR model
  
  standardise <- function(x){
    return((x - mean(x))/sd(x))
  }
  
  df <- df %>%
    mutate_at(vars(-Report), standardise)
  
  ts_df <- ts(df[,-1], start = 2006.5, end = 2024.5,
              frequency = 2) # The .5s signal second report
  
  ts_diff <- diff(ts_df)
  
  return(ts_diff)
}

ts_87_60_diff <- standardise_timeseries_diff(df_87_60_joined)
ts_87_diff <- standardise_timeseries_diff(df_87_joined)
ts_filtered_full_diff <- standardise_timeseries_diff(df_filtered_full_joined)
ts_filtered_87_60_diff <- standardise_timeseries_diff(df_filtered_87_60_joined)


# Variables to use in VAR
variables_to_use <- c("Sentiment.Index", "Credit.To.GDP.Gap",
                      "pnfc_dsr" ,"Price.Book.Ratio", "VIX", "PNF.Credit.Growth")
# Decent results with
# variables_to_use <- c("Sentiment.Index", "Credit.To.GDP.Gap",
# "PNF.Credit.Growth", "pnfc_dsr" ,"Price.Book.Ratio", "VIX")
# Get locations of these in the data frame
var_indices <- match(variables_to_use, colnames(ts_filtered_full_diff))

# These results are quite promising, VAR(1) wasn't significant but this is
# Drop: Total Credit (VIF)
# External Debt (Irrelevant)
# Household DSR (VIF)
# GDP (Irrelevant)

ts_filtered_87_60_diff <- ts_filtered_87_60_diff[, var_indices] # Drop Total credit and household DSR (Because VIFs)
var_model_filtered_87_60 <- VAR(ts_filtered_87_60_diff, p = 2, type = "const") # VAR(2) with constant term
summary(var_model_filtered_87_60)

ts_filtered_full_diff <- ts_filtered_full_diff[,var_indices] # Drop Total credit a household DSR and GDP (Because VIFs)
var_model_filtered_full <- VAR(ts_filtered_full_diff, p = 2, type = "const") # VAR(2) with constant term
summary(var_model_filtered_full)

# Sig neg coeff for 2nd lag on Total credit to GDP
  # Contraction in lending 2 periods after increase in index (decrease sentiment)
# Same for Credit to GDP gap (deviation of credit to GDP from trend)
  # Low value means deleveraging
# Sig neg coefficient for 2nd lag on External Debt to GDP
  # But this equation low R squared and doesn't pass F test

# TODO: Unsure about whether I need to drop the columns here (need to in linear regression but unsure about VAR)


# How impulse response might be done I'm not sure
# oir <- irf(var_model_filtered_full, impulse = "house_price_yoy", response = "Sentiment.Index",
#            n.ahead = 4, ortho = TRUE, runs = 1000, seed = 12345)
# plot(oir)

# ---------------------------
##### Linear Regression #####
# ---------------------------



df_filtered_87_60_joined <- df_filtered_87_60_joined %>%
  mutate_at(vars(-Report), standardise)

# With total credit to GDP and household_dsr the VIF scores were too high
# Results might be better without PNF credit growth and GDP too
# Stock Volatility basically a worse VIX
lm <- lm(Sentiment.Index ~ Credit.To.GDP.Gap  + SRISK + VIX  +
           pnfc_dsr + Price.Book.Ratio + CDS, data = df_filtered_87_60_joined)
rounded_coefficients <- round(lm$coefficients, digits = 3)
lm$coefficients <- rounded_coefficients
summary(lm)
car::vif(lm)

lm_full <- lm(Sentiment.Index ~ Credit.To.GDP.Gap + PNF.Credit.Growth+ SRISK+ VIX+
                house_price_yoy + pnfc_dsr + Price.Book.Ratio + CDS, data = df_filtered_full_joined) 
summary(lm_full)
car::vif(lm_full)

lm_var <- lm(Sentiment.Index ~ Credit.To.GDP.Gap + PNF.Credit.Growth + CDS+
                  pnfc_dsr + SRISK + VIX, data = df_filtered_87_60_joined)
rounded_coefficients <- round(lm_var$coefficients, digits = 3)
lm_var$coefficients <- rounded_coefficients
summary(lm_var)
car::vif(lm_var)


# Lagged regression - giving same results as contemperous which is good
# Should be restandardised though
df_filtered_87_60_joined_lag <- df_filtered_87_60_joined %>%
  mutate(Sentiment.Index_lag = lag(Sentiment.Index, 2),
         VIX_lag = lag(VIX, 2),
         Credit.To.GDP.Gap_lag = lag(Credit.To.GDP.Gap, 2),
         PNF.Credit.Growth_lag = lag(PNF.Credit.Growth, 2),
         SRISK_lag = lag(SRISK, 2),
         house_price_yoy_lag = lag(house_price_yoy, 2),
         pnfc_dsr_lag = lag(pnfc_dsr, 2),
         Price.Book.Ratio_lag = lag(Price.Book.Ratio, 2),
         CDS_lag = lag(CDS, 2)) %>%
  na.omit() %>% 
  mutate_at(vars(-Report), standardise) # Standardise again

lm_87_lag <- lm(Sentiment.Index ~ Credit.To.GDP.Gap_lag  +SRISK_lag +VIX_lag +
            pnfc_dsr_lag + CDS_lag, data = df_filtered_87_60_joined_lag)
lm_87_lag$coefficients <- round(lm_87_lag$coefficients, digits = 3)
summary(lm_87_lag)

# SRISK makes this worse right now
df_filtered_full_joined_lag <- df_filtered_full_joined %>%
  mutate(Credit.To.GDP.Gap_lag = lag(Credit.To.GDP.Gap, 2),
         PNF.Credit.Growth_lag = lag(PNF.Credit.Growth, 2),
         SRISK_lag = lag(SRISK, 2),
         house_price_yoy_lag = lag(house_price_yoy, 2),
         pnfc_dsr_lag = lag(pnfc_dsr, 2),
         Price.Book.Ratio_lag = lag(Price.Book.Ratio, 2),
         CDS_lag = lag(CDS, 2)) %>%
  na.omit()
lm_full_lag <- lm(Sentiment.Index ~ Credit.To.GDP.Gap + PNF.Credit.Growth +
                     house_price_yoy + pnfc_dsr + Price.Book.Ratio + CDS, data = df_filtered_full_joined_lag)
summary(lm_full_lag)


# -------------------
# Sub index analysis
# -------------------

financials_joined <- join_data(financial_sub, data)
colnames(financials_joined)[2] <- "Financial.Sentiment.Index"
household_joined <- join_data(household_sub, data)
colnames(household_joined)[2] <- "Household.Sentiment.Index"
# Standardise sub indexes
financials_joined <- financials_joined %>%
  mutate_at(vars(-Report), standardise)
household_joined <- household_joined %>%
  mutate_at(vars(-Report), standardise)


lm_financials <- lm(Financial.Sentiment.Index ~ Credit.To.GDP.Gap + SRISK + VIX+
                        pnfc_dsr + CDS, data = financials_joined) #Price book marginal
lm_financials$coefficients <- round(lm_financials$coefficients, digits = 3)
summary(lm_financials)
car::vif(lm_financials)


lm_household <- lm(Household.Sentiment.Index ~ Credit.To.GDP.Gap +household_dsr + house_price_yoy
                   ,data = household_joined)
lm_household$coefficients <- round(lm_household$coefficients, digits = 3)
summary(lm_household)
car::vif(lm_household)


#---------------------------
# Sentiment as an explanatory variable
#---------------------------
# Restandardise the lag dfs
df_filtered_87_60_joined_lag <- df_filtered_87_60_joined_lag %>%
  mutate_at(vars(-Report), standardise) %>% 
  mutate(Sentiment.Index = ifelse(Sentiment.Index > 2, 2, Sentiment.Index))



lm_dsr <- lm(pnfc_dsr ~ Sentiment.Index + Credit.To.GDP.Gap + PNF.Credit.Growth + lag(Sentiment.Index,1) +
           VIX + Price.Book.Ratio + CDS, data = df_filtered_87_60_joined)
lm_dsr$coefficients <- round(lm_dsr$coefficients, digits = 3)
summary(lm_dsr)

lm_dsr_lag <- lm(pnfc_dsr ~ Sentiment.Index_lag + Credit.To.GDP.Gap_lag + PNF.Credit.Growth_lag +
           SRISK_lag + VIX_lag + Price.Book.Ratio_lag + CDS_lag, data = df_filtered_87_60_joined_lag)
lm_dsr_lag$coefficients <- round(lm_dsr_lag$coefficients, digits = 3)
summary(lm_dsr_lag)
car::vif(lm_dsr_lag)


lm_credit_gap <- lm(Credit.To.GDP.Gap ~ Sentiment.Index + PNF.Credit.Growth + lag(Sentiment.Index,1) +
                        SRISK + VIX + Price.Book.Ratio + CDS, data = df_filtered_87_60_joined)
lm_credit_gap$coefficients <- round(lm_credit_gap$coefficients, digits = 3)
summary(lm_credit_gap)

lm_credit_gap_lag <- lm(Credit.To.GDP.Gap ~ Sentiment.Index_lag + PNF.Credit.Growth_lag + lag(Sentiment.Index,1) +
                        SRISK_lag + VIX_lag + Price.Book.Ratio_lag + CDS_lag, data = df_filtered_87_60_joined_lag)
lm_credit_gap_lag$coefficients <- round(lm_credit_gap_lag$coefficients, digits = 3)
summary(lm_credit_gap_lag)

lm_credit_growth <- lm(PNF.Credit.Growth ~ Sentiment.Index + Credit.To.GDP.Gap + lag(Sentiment.Index,1) +
                        SRISK + VIX + Price.Book.Ratio + CDS, data = df_filtered_87_60_joined)
lm_credit_growth$coefficients <- round(lm_credit_growth$coefficients, digits = 3)
summary(lm_credit_growth)

lm_HY <- lm(GBP.Investment.Grade ~ lag(Sentiment.Index,1) +
              lag(Credit.To.GDP.Gap,1)  +
              lag(CDS,1), data = df_filtered_87_60_joined)
lm_HY$coefficients <- round(lm_HY$coefficients, digits = 3)
summary(lm_HY)
car::vif(lm_HY)



# Plot IG spread vs sentiment
ggplot(df_filtered_87_60_joined, aes(x = Report)) +
  geom_line(aes(y = GBP.High.Yield, color = "Investment Grade Spread")) +
  geom_line(aes(y = Sentiment.Index, color = "Sentiment Index")) +
  labs(title = "Investment Grade Spread vs Sentiment Index",
       x = "Report",
       y = "Spread / Sentiment Index") +
  theme_minimal() +
  scale_color_manual(values = c("Investment Grade Spread" = "blue", "Sentiment Index" = "red"))
