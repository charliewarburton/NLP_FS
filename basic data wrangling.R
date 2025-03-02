library(vars)
library(tidyverse)

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
    mutate_at(vars(correa_index, Sentiment.Index ), standardise)
  
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

var_model <- VAR(ts_87_60_diff, p = 1, type = "const") # VAR(1) with constant term
summary(var_model)

var_model_87 <- VAR(ts_87_diff, p = 1, type = "const") # VAR(1) with constant term
summary(var_model_87)


# These results are quite promising, VAR(1) wasn't significant but this is
var_model_filtered_full <- VAR(ts_filtered_full_diff, p = 2, type = "const") # VAR(2) with constant term
summary(var_model_filtered_full)

# Sig neg coeff for 2nd lag on Total credit to GDP
  # Contraction in lending 2 periods after increase in index (decrease sentiment)
# Same for Credit to GDP gap (deviation of credit to GDP from trend)
  # Low value means deleveraging
# Sig neg coefficient for 2nd lag on External Debt to GDP
  # But this equation low R squared and doesn't pass F test

var_model_filtered_87_60 <- VAR(ts_filtered_87_60_diff, p = 2, type = "const") # VAR(2) with constant term
summary(var_model_filtered_87_60)



