# Preidctive Random Forest Function

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
  min = 0 # Initialise min value for variable importance
  
  # Lag explanatory variables. Note: This assumes a fixed set of variables.
  # A more flexible approach would be to pass the variable names to be lagged.
  df_lagged <- df %>%
    mutate(
      across(all_of(explanatory), ~lag(.x, n = lag), .names = "{.col}_lag")
    ) %>%
    na.omit()
  
  # Standardizing is less critical for RF but doesn't harm.
  # We standardise only the predictors, not the response.
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
                                         replace = T,      
                                         fraction = 0.8)      # Fraction of data to sample for each tree
    
    rf <- party::cforest(y ~ ., data = temp_data,
                         control = rf_control,
                         weights = case_weights)
    
    # Store variable importance for this run
    imp <- suppressWarnings(party::varimp(rf, conditional = cond)) # Suppress warnings for custom weights
    # Ok because the case weights are relative to how they would be if 
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
  
  minimum_importance <- importance_results %>% 
    group_by(var) %>%
    summarise(min_importance = min(value)) %>%
    arrange(min_importance)
  
  # Summarise performance metrics
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
  
  # ---  Return a structured list ---
  return(list(importance = final_importance, performance = avg_performance, 
              min_performance = minimum_importance, min_val = min, var_min = var_min$var))
}
