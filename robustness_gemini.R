library(tidyverse)
library(caret) # For confusion matrix and related stats
library(ggpubr) # For arranging plots
library(scales) # For formatting percentages

setwd("C:\\Users\\charl\\OneDrive\\Uni\\Masters\\Dissertation\\NLP_FS")

standardise <- function(x){
  return((x - mean(x))/sd(x))
}

# Load the data
df <- read_csv("full_results_with_gemini.csv")

# Ensure labels are factors with consistent levels for comparison
# For CBRoBERTa (assuming 'positive', 'negative')
df <- df %>%
  mutate(
    CBRoBERTa_Label = factor(`Sentiment Label`, levels = c("positive", "negative")),
    Gemini_Label = factor(`Gemini Sentiment`, levels = c("positive", "neutral", "negative")),
    CBRoBERTa_Score = `Sentiment Score` # Assuming this is the numeric score
  )


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

cross_tab <- table(CBRoBERTa = df$CBRoBERTa_Label, Gemini = df$Gemini_Label)
print("Cross-tabulation (CBRoBERTa vs. Gemini 3-class):")
print(cross_tab)

# Visualize as a stacked bar chart or mosaic plot
ggplot(df, aes(x = Gemini_Label, fill = CBRoBERTa_Label)) +
  geom_bar(position = "fill") + # "fill" shows proportions
  labs(
    title = "CBRoBERTa Labels within each Gemini Sentiment Category",
    x = "Gemini Sentiment",
    y = "Proportion",
    fill = "CBRoBERTa Label"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Or absolute counts
ggplot(df, aes(x = Gemini_Label, fill = CBRoBERTa_Label)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of CBRoBERTa Labels by Gemini Sentiment",
    x = "Gemini Sentiment",
    y = "Count",
    fill = "CBRoBERTa Label"
  ) +
  theme_minimal()


ggplot(df, aes(x = Gemini_Label, y = CBRoBERTa_Score, fill = Gemini_Label)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribution of CBRoBERTa Scores by Gemini Sentiment Label",
    x = "Gemini Sentiment Label",
    y = "CBRoBERTa Sentiment Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Density plot of CBRoBERTa scores, faceted by Gemini label
ggplot(df, aes(x = CBRoBERTa_Score, fill = CBRoBERTa_Label)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~Gemini_Label, ncol = 1, scales = "free_y") +
  labs(
    title = "Density of CBRoBERTa Scores, Grouped by Gemini Label",
    subtitle = "Color shows CBRoBERTa's own classification",
    x = "CBRoBERTa Sentiment Score",
    y = "Density",
    fill = "CBRoBERTa Label"
  ) +
  theme_minimal()

df_analysis <- df %>%
  mutate(
    Agreement_Binary = case_when(
      CBRoBERTa_Label == "positive" & Gemini_Label == "positive" ~ "Agree Positive",
      CBRoBERTa_Label == "negative" & Gemini_Label == "negative" ~ "Agree Negative",
      CBRoBERTa_Label == "positive" & Gemini_Label == "negative" ~ "Disagree (CB:Pos, Gem:Neg)",
      CBRoBERTa_Label == "negative" & Gemini_Label == "positive" ~ "Disagree (CB:Neg, Gem:Pos)",
      Gemini_Label == "neutral" & CBRoBERTa_Label == "positive" ~ "CB:Pos, Gem:Neutral",
      Gemini_Label == "neutral" & CBRoBERTa_Label == "negative" ~ "CB:Neg, Gem:Neutral",
      TRUE ~ "Other" # Should not happen with current logic if all covered
    )
  )

# View counts of agreement/disagreement types
print(table(df_analysis$Agreement_Binary))




# ========================================
df_gemini <- df %>% 
  group_by(Report) %>%
  summarise(neg = sum(Gemini_Label == "negative"),
            pos = sum(Gemini_Label == "positive"))
#total = n()) %>% 
total = 6358
df_gemini <- df_gemini %>%
  mutate(Gem.Sentiment.Index = (neg - pos)/total)
df_gemini$Report <- as.Date(release_dates, format = "%Y-%m-%d")

df_gemini <- df_gemini %>%
  arrange(Report) %>%
  mutate(Gem.Sentiment.Index = standardise(Gem.Sentiment.Index))

ggplot(data = df_gemini, mapping = aes(x = Report, y = Gem.Sentiment.Index))+
  geom_line()+
  geom_point() +
  labs(title = "Sentiment Index over time",
       x = "Report",
       y = "Sentiment Index") +
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")


# ===================================================================
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

join_data <- function(sentiment_df, data){
  
  # Function to join sentiment data with financial cycle data
  
  joined_df <- sentiment_df %>%
    left_join(data %>% 
                rename(Financial_Date = Date), # Rename to prevent conflicts
              by = join_by(closest(Report >= Financial_Date)) )# Ensuring past data is used
  
  joined_df <- joined_df %>%
    dplyr::select(-c(pos, neg, Financial_Date)) # Removing unneeded columns
  
  return(joined_df)
}

gemini_joined <- join_data(df_gemini, data)
# Standardise
gemini_joined <- gemini_joined %>%
  mutate_at(vars(-Report), standardise)
gem_lm <- lm(Gem.Sentiment.Index ~ Credit.To.GDP.Gap  + SRISK +
           VIX + pnfc_dsr + Price.Book.Ratio + CDS, data = gemini_joined)
gem_lm$coefficients <- round(gem_lm$coefficients, digits = 3)
summary(gem_lm)
car::vif(gem_lm)

gem_var <- lm(Gem.Sentiment.Index ~ Credit.To.GDP.Gap + PNF.Credit.Growth + CDS+
               pnfc_dsr + SRISK , data = gemini_joined)
rounded_coefficients <- round(gem_var$coefficients, digits = 3)
gem_var$coefficients <- rounded_coefficients
summary(gem_var)
car::vif(gem_var)
