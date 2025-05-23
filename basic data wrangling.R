library(vars)
library(tidyverse)
library(lubridate)

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

# filter text chunks for words in dictionary
pattern <- paste0("\\b(", paste(correa_dict$Word, collapse="|"), ")\\b")

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



# ----------------------------------- #
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
# Folowing Correa, take SRISK and divide by nominal GDP (Data from FRED)
SRISK <- read.csv("srisk.csv")
colnames(SRISK) <- c("Date", "SRISK.Raw", "UKNGDP")
SRISK <- SRISK %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  mutate(SRISK = SRISK.Raw/UKNGDP) %>% 
  select(Date, SRISK) %>% 
  na.omit()

data <- data %>%
  left_join(SRISK, by = "Date")

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

# VAR lag selection 
# Not working right now
lag_selection <- VARselect(ts_87_60_diff, lag.max = 6, type = "const") # Test up to 6 lags
print(lag_selection$criteria) # Show AIC, BIC, and HQC criteria



# These results are quite promising, VAR(1) wasn't significant but this is
ts_filtered_full_diff <- ts_filtered_full_diff[, -c(2, 8)] # Drop Total credit and household DSR (Because VIFs)
var_model_filtered_full <- VAR(ts_filtered_full_diff, p = 2, type = "const") # VAR(2) with constant term
summary(var_model_filtered_full)

# Sig neg coeff for 2nd lag on Total credit to GDP
  # Contraction in lending 2 periods after increase in index (decrease sentiment)
# Same for Credit to GDP gap (deviation of credit to GDP from trend)
  # Low value means deleveraging
# Sig neg coefficient for 2nd lag on External Debt to GDP
  # But this equation low R squared and doesn't pass F test

# TODO: Unsure about whether I need to drop the columns here (need to in linear regression but unsure about VAR)
ts_filtered_87_60_diff <- ts_filtered_87_60_diff[, -c(2,8)] # Drop Total credit and household DSR (Because VIFs)
var_model_filtered_87_60 <- VAR(ts_filtered_87_60_diff, p = 2, type = "const") # VAR(2) with constant term
summary(var_model_filtered_87_60)

# How impulse response might be done I'm not sure
# oir <- irf(var_model_filtered_full, impulse = "house_price_yoy", response = "Sentiment.Index",
#            n.ahead = 4, ortho = TRUE, runs = 1000, seed = 12345)
# plot(oir)

# ---------------------------
##### Linear Regression #####
# ---------------------------

# With total credit to GDP and household_dsr the VIF scores were too high
# Results might be better without PNF credit growth and GDP too
lm <- lm(Sentiment.Index ~ Credit.To.GDP.Gap + PNF.Credit.Growth + SRISK +
         house_price_yoy + pnfc_dsr + Price.Book.Ratio + CDS, data = df_filtered_87_60_joined)
summary(lm)
car::vif(lm)


lm_full <- lm(Sentiment.Index ~ Credit.To.GDP.Gap + PNF.Credit.Growth+ SRISK+
                house_price_yoy + pnfc_dsr + Price.Book.Ratio + CDS, data = df_filtered_full_joined) 
summary(lm_full)
car::vif(lm_full)


# Lagged regression - giving same results as contemperous which is good
df_filtered_87_60_joined_lag <- df_filtered_87_60_joined %>%
  mutate(Sentiment.Index = lag(Sentiment.Index, 2),
         Credit.To.GDP.Gap = lag(Credit.To.GDP.Gap, 2),
         PNF.Credit.Growth = lag(PNF.Credit.Growth, 2),
         SRISK = lag(SRISK, 2),
         house_price_yoy = lag(house_price_yoy, 2),
         pnfc_dsr = lag(pnfc_dsr, 2),
         Price.Book.Ratio = lag(Price.Book.Ratio, 2),
         CDS = lag(CDS, 2)) %>%
  na.omit()
lm_87_lag <- lm(Sentiment.Index ~ Credit.To.GDP.Gap + PNF.Credit.Growth +SRISK +
           house_price_yoy + pnfc_dsr + Price.Book.Ratio + CDS, data = df_filtered_87_60_joined_lag)
summary(lm_87_lag)

# SRISK makes this worse right now
df_filtered_full_joined_lag <- df_filtered_full_joined %>%
  mutate(Sentiment.Index = lag(Sentiment.Index, 2),
         Credit.To.GDP.Gap = lag(Credit.To.GDP.Gap, 2),
         PNF.Credit.Growth = lag(PNF.Credit.Growth, 2),
         SRISK = lag(SRISK, 2),
         house_price_yoy = lag(house_price_yoy, 2),
         pnfc_dsr = lag(pnfc_dsr, 2),
         Price.Book.Ratio = lag(Price.Book.Ratio, 2),
         CDS = lag(CDS, 2)) %>%
  na.omit()
lm_full_lag <- lm(Sentiment.Index ~ Credit.To.GDP.Gap + PNF.Credit.Growth +
                     house_price_yoy + pnfc_dsr + Price.Book.Ratio + CDS, data = df_filtered_full_joined_lag)
summary(lm_full_lag)
