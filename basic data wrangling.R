library(tidyverse)

setwd("C:\\Users\\charl\\OneDrive\\Uni\\Masters\\Dissertation\\NLP_FS")

df <- read.csv("full_results.csv")

# Grpah showing pos/neg over time
df_full <- df %>% 
  group_by(Report) %>%
  summarise(neg = sum(Sentiment.Label == "negative"),
            pos = sum(Sentiment.Label == "positive"),
            total = n())

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
point_graph(df_87, "Sentiment over time (Sentiment Score >= 0.87)")

df_87_60 <- df %>% 
  filter(Sentiment.Label == "negative" & Sentiment.Score >= 0.87 | 
           Sentiment.Label == "positive" & Sentiment.Score >= 0.60) %>%
  group_by(Report) %>%
  summarise(neg = sum(Sentiment.Label == "negative"),
            pos = sum(Sentiment.Label == "positive")) %>% 
  inner
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
