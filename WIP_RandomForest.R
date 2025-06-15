# WIP Random F
library(party)
library(tidyverse)

rf_importance <- function(df, independent, explanatory, lag,
                          repeats=1000, cond=FALSE, alpha_level = 0.05, trees = 500){

  
  min = 0 # initialise min value
  #Df to hold the variable importance values
  temp <- data.frame(values = numeric(),
                     var = character())
  
  # Lag data
  df <- df %>%
    mutate(Sentiment.Index_lag = lag(Sentiment.Index, lag),
           VIX_lag = lag(VIX, lag),
           Credit.To.GDP.Gap_lag = lag(Credit.To.GDP.Gap, lag),
           PNF.Credit.Growth_lag = lag(PNF.Credit.Growth, lag),
           SRISK_lag = lag(SRISK, lag),
           house_price_yoy_lag = lag(house_price_yoy, lag),
           pnfc_dsr_lag = lag(pnfc_dsr, lag),
           Price.Book.Ratio_lag = lag(Price.Book.Ratio, lag),
           CDS_lag = lag(CDS, lag)) %>%
    na.omit() %>% 
    mutate_at(vars(-Report), standardise)
  
  
  x <- df %>% select(all_of(explanatory))
  y <- df[[independent]]

  # Repeating the random forest
  for(j in 1:repeats){
    set.seed(j)
    temp_data <- data.frame(y=y, x) #combines x and y in a df so the next line can run
    rf <- party::cforest(y ~. , data=temp_data, control = party::cforest_control(ntree = trees,
                                                                                 mtry = round(sqrt(length(x))),
                                                                                 mincriterion = 1-alpha_level,
                                                                                 testtype = "Teststatistic"))
    
    temp1 <- stack(party::varimp(rf, conditional = cond)) %>% #stack turns the named list into a df
      mutate(var = ind) %>%  #varimp(rf) doesn't give asset so temp1 adds it and renames ind to var to be compatible with other functions
      select(!ind)#drop ind
    temp <- bind_rows(temp, temp1) 
    
    if(j %% 100 == 0) {
      print(paste("Completed", j, "iterations"))
    }
  }
  
  if(min > min(temp$values)){
    min <- min(temp$values)
    var_min <- temp %>% 
      filter(values == min) %>% 
      select(var)
  }
  
  
  
  results <- temp %>% 
    group_by(var) %>% 
    summarise(avg = mean(values)) %>% 
    select(avg, var)
  
  print(slice_tail(temp %>% arrange(desc(values)), n =5))
  
  return(results)
  
} 

find_turning_points <- function(df, col_name) {
  # Check if the specified column exists in the dataframe
  if(!col_name %in% names(df)){
    stop(paste("Column '", col_name, "' not found in the dataframe.", sep = ""))
  }
  
  # Create the name for the new turning point column
  tp_col_name <- paste0(col_name, ".TP")
  
  df <- df %>%
    mutate(
      # A peak occurs if the value is greater than its neighbors, and the
      # subsequent downward trend persists for at least one more step.
      # Condition: x[i-1] < x[i] > x[i+1] AND x[i+1] > x[i+2]
      is_peak = lag(.data[[col_name]]) < .data[[col_name]] &
        lead(.data[[col_name]]) < .data[[col_name]] &
        lead(.data[[col_name]], 1) > lead(.data[[col_name]], 2),
      
      # A trough occurs if the value is less than its neighbors, and the
      # subsequent upward trend persists for at least one more step.
      # Condition: x[i-1] > x[i] < x[i+1] AND x[i+1] < x[i+2]
      is_trough = lag(.data[[col_name]]) > .data[[col_name]] &
        lead(.data[[col_name]]) > .data[[col_name]] &
        lead(.data[[col_name]], 1) < lead(.data[[col_name]], 2),
      
      # Combine conditions. Use !! and := for dynamic column naming.
      # Convert the resulting boolean (TRUE/FALSE) to an integer (1/0).
      # Replace NAs (at the start/end of the series) with 0.
      !!tp_col_name := as.integer(is_peak | is_trough),
      !!tp_col_name := tidyr::replace_na(!!sym(tp_col_name), 0)
    ) %>%
    # Remove the intermediate helper columns
    select(-is_peak, -is_trough)
  
  # Make new col type factor
  df[[tp_col_name]] <- as.factor(df[[tp_col_name]])
  
  return(df)
}

test <- rf_importance(df = df.TP,
              independent = "GBP.Investment.Grade.TP",
              explanatory = c("Sentiment.Index_lag", "CDS_lag", "VIX_lag", 
                              "Credit.To.GDP.Gap", "SRISK_lag", "Price.Book.Ratio_lag"
                              ),
              lag = 1,
              repeats = 1000, cond = F, alpha_level = 0.05, trees = 500)

df.TP <- df_filtered_87_60_joined
df.TP <- find_turning_points(df.TP, col_name = "GBP.Investment.Grade")

df_filtered_87_60_joined$Gem.Sentiment.Index <- df_gemini$Gem.Sentiment.Index
df_filtered_87_60_joined$Gem.Sentiment.Index_lag <- lag(df_filtered_87_60_joined$Gem.Sentiment.Index, 1)


# Set upper limit on sentiment index to 2
df.TP <- df.TP %>%
  mutate(Sentiment.Index = ifelse(Sentiment.Index > 2, 2, Sentiment.Index))



#--------------------------
# RF for turning points
#---------------------------
# Ensure required packages are loaded
library(dplyr)
library(party)
library(tidyr)

#' Calculates variable importance for a binary turning point (TP) variable.
#'
#' This function is adapted for a binary classification task with severe class imbalance.
#' It uses case weighting to train the random forest and evaluates performance using
#' OOB Recall, Precision, and F1-score.
#'
#' @param df A dataframe containing the data.
#' @param independent The name of the binary (0/1) response variable (e.g., "Credit.To.GDP.Gap.TP").
#' @param explanatory A character vector of explanatory variable names.
#' @param lag The number of periods to lag the explanatory variables.
#' @param repeats The number of times to repeat the random forest model for stable importance scores.
#' @param cond Logical. Whether to use conditional permutation importance.
#' @param trees The number of trees in each forest.
#'
#' @return A list containing two dataframes:
#'         1. `importance`: Average variable importance scores across all repeats.
#'         2. `performance`: Average OOB performance metrics (Recall, Precision, F1-Score).

rf_importance_TP <- function(df, independent, explanatory, lag,
                             repeats = 100, cond = FALSE, trees = 500) {
  
  # --- Data Preparation ---
  min = 0 # Initialize min value for variable importance
  
  # Lag explanatory variables. Note: This assumes a fixed set of variables.
  # A more flexible approach would be to pass the variable names to be lagged.
  df_lagged <- df %>%
    mutate(
      across(all_of(explanatory), ~lag(.x, n = lag), .names = "{.col}_lag")
    ) %>%
    na.omit()
  
  # Standardizing is less critical for RF but doesn't harm.
  # We standardize only the predictors, not the response.
  explanatory_lagged <- paste0(explanatory, "_lag")
  df_final <- df_lagged %>%
    mutate(across(all_of(explanatory_lagged), ~ as.vector(scale(.x))))
  
  x <- df_final %>% select(all_of(explanatory_lagged))
  
  # --- Key Modification 1: Ensure response is a factor for classification ---
  y <- as.factor(df_final[[independent]])
  
  # --- Key Modification 2: Calculate case weights to handle class imbalance ---
  n0 <- sum(y == "0")
  n1 <- sum(y == "1")
  
  if (n1 == 0) stop("The independent variable contains no positive cases (1s).")
  
  # Assign higher weight to the rare class (1s)
  minority_weight <- n0 / n1
  
  # Assign weight of 1 to the majority class and the calculated weight to the minority
  case_weights <- if_else(y == "1", minority_weight, 1)
  
  # Dataframes to store results from each iteration
  importance_results <- data.frame()
  performance_results <- data.frame()
  
  # --- Repeating the random forest ---
  for (j in 1:repeats) {
    set.seed(j)
    temp_data <- data.frame(y = y, x)
    
    # Train the conditional random forest with case weights
    rf_control <- party::cforest_control(ntree = trees,
                                         mtry = round(sqrt(ncol(x))),
                                         replace = T,      # Sample without replacement
                                         fraction = 0.8)      # Fraction of data to sample for each tree
    
    rf <- party::cforest(y ~ ., data = temp_data,
                         control = rf_control,
                         weights = case_weights)
    
    # Store variable importance for this run
    imp <- party::varimp(rf, conditional = cond)
    temp_imp <- data.frame(var = names(imp), value = imp, row.names = NULL)
    importance_results <- bind_rows(importance_results, temp_imp)
    
    if(min > min(temp_imp$value)) {
      min <- min(temp_imp$value)
      var_min <- temp_imp %>% filter(value == min) %>% select(var)
    }
    
    # --- Key Modification 3: OOB Performance Evaluation ---
    oob_preds <- predict(rf, OOB = TRUE)
    conf_matrix <- table(Predicted = oob_preds, Actual = y)
    
    # Safely calculate metrics to avoid division-by-zero errors
    TP <- if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) conf_matrix["1", "1"] else 0
    FP <- if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) conf_matrix["1", "0"] else 0
    FN <- if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) conf_matrix["0", "1"] else 0
    
    Recall <- if ((TP + FN) > 0) TP / (TP + FN) else 0
    Precision <- if ((TP + FP) > 0) TP / (TP + FP) else 0
    F1_Score <- if ((Precision + Recall) > 0) 2 * (Precision * Recall) / (Precision + Recall) else 0
    
    performance_results <- bind_rows(performance_results, 
                                     data.frame(Recall, Precision, F1_Score))
    
    if (j %% 20 == 0) {
      print(paste("Completed", j, "of", repeats, "iterations."))
    }
  }
  
  # --- Aggregate and Finalise Results ---
  # Summarise importance scores
  final_importance <- importance_results %>%
    group_by(var) %>%
    summarise(
      avg_importance = mean(value),
      sd_importance = sd(value)
    ) %>%
    arrange(desc(avg_importance))
  
  # Summarize performance metrics
  avg_performance <- performance_results %>%
    summarise(
      Avg_Recall = mean(Recall),
      Avg_Precision = mean(Precision),
      Avg_F1_Score = mean(F1_Score)
    )
  
  print("--- Average OOB Performance ---")
  print(avg_performance)
  cat("\n") # Add a newline for cleaner output
  
  print("--- Top 5 Most Important Variables ---")
  print(head(final_importance, 5))
  
  print(paste("Minimum importance value:", min, " for variable:", var_min$var))
  
  # --- Key Modification 4: Return a structured list ---
  return(list(importance = final_importance, performance = avg_performance, min_val = min, var_min = var_min$var))
}

explanatory_vars <- c("Sentiment.Index", "VIX", "Credit.To.GDP.Gap", 
                      "SRISK", "house_price_yoy", 
                      "pnfc_dsr", "Price.Book.Ratio", "CDS")

# Much better performance with this smaller subset of variables
# (Others just adding noise aka importance = 0)
explanatory_vars <- c("Sentiment.Index", "house_price_yoy", "SRISK", "CDS")


# Todo: Smoothing param on sentiment or take rollig avg

# I thought 1 lag used to work but now doesn't?
results_lag2 <-  rf_importance_TP(df = df.TP, 
                                 independent = "GBP.Investment.Grade.TP", 
                                 explanatory = explanatory_vars, 
                                 lag = 2,
                                 repeats = 800, 
                                 trees = 350,
                                 cond = F)

# Robust to using Gemini too
df.TP$Gem.Sentiment.Index <- df_gemini$Gem.Sentiment.Index



#----------------------------
# Plot IG spread vs sentiment
#----------------------------
ggplot(df_filtered_87_60_joined, aes(x = Report)) +
  geom_line(aes(y = GBP.High.Yield, color = "Investment Grade Spread")) +
  geom_line(aes(y = Sentiment.Index, color = "Sentiment Index")) +
  labs(title = "Investment Grade Spread vs Sentiment Index",
       x = "Report",
       y = "Spread / Sentiment Index") +
  theme_minimal() +
  scale_color_manual(values = c("Investment Grade Spread" = "blue", "Sentiment Index" = "red"))






