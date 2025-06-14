library(glmnet)
library(ggplot2)
library(tidyr)
library(dplyr)

#' @title Run Bootstrapped Ridge Regression
#' @description This function performs ridge regression using bootstrapping to estimate
#'              the distribution and stability of coefficients. It standardises the data,
#'              runs the bootstrap loop, and returns a violin plot of the coefficient
#'              distributions and a summary table.
#'
#' @param data A data.frame containing the response and predictor variables.
#' @param formula A model formula object (e.g., y ~ x1 + x2).
#' @param n_bootstraps An integer specifying the number of bootstrap samples to run.
#'                     Default is 1000.
#' @param sig_level A numeric value for the significance level (e.g., 5 for 95% CI).
#'                  This determines the quantiles in the summary table. Default is 5.
#'
#' @return A list containing two elements:
#'         1. `violin_plot`: A ggplot object showing the distribution of coefficients.
#'         2. `summary_table`: A data.frame summarizing the bootstrapped coefficients
#'                            (mean, median, SD, and quantiles).

run_bootstrap_ridge <- function(data, formula, n_bootstraps = 1000, sig_level = 5, shaded = TRUE, 
                                title = "Distribution of Ridge Coefficients") {
  
  # --- 1. Input Validation and Setup ---
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (!inherits(formula, "formula")) stop("'formula' must be a formula object.")
  if (!is.numeric(n_bootstraps) || n_bootstraps <= 0) stop("'n_bootstraps' must be a positive integer.")
  if (!is.numeric(sig_level) || sig_level <= 0 || sig_level >= 100) stop("'sig_level' must be between 0 and 100.")
  
  # Calculate quantile probabilities from the significance level
  lower_quantile <- (sig_level / 2) / 100
  upper_quantile <- 1 - lower_quantile
  
  # Extract response variable name from the formula
  response_var <- all.vars(formula)[1]
  
  # --- 2. standardise Data ---
  df_standardised <- data
  
  cols_to_scale <- setdiff(names(df_standardised), "Report")
  numeric_cols_to_scale <- cols_to_scale[sapply(df_standardised[cols_to_scale], is.numeric)]
  
  df_standardised[numeric_cols_to_scale] <- lapply(df_standardised[numeric_cols_to_scale], function(x) as.numeric(scale(x)))
  
  
  # --- 3. Bootstrap Loop for Ridge Regression ---
  set.seed(456) 
  
  temp_x_for_names <- model.matrix(formula, data = df_standardised)[, -1, drop = FALSE]
  temp_y_for_names <- df_standardised[[response_var]]
  temp_glmnet_fit <- glmnet(temp_x_for_names, temp_y_for_names, alpha = 0, lambda = 0.01)
  coeff_names <- rownames(coef(temp_glmnet_fit))
  
  bootstrap_coeffs_matrix <- matrix(NA, nrow = n_bootstraps, ncol = length(coeff_names),
                                    dimnames = list(NULL, coeff_names))
  
  for (i in 1:n_bootstraps) {
    if (i %% 100 == 0) cat("Bootstrap sample:", i, "/", n_bootstraps, "\n")
    
    sample_indices <- sample(1:nrow(df_standardised), size = nrow(df_standardised), replace = TRUE)
    bootstrap_sample_df <- df_standardised[sample_indices, ]
    
    x_matrix <- model.matrix(formula, data = bootstrap_sample_df)[, -1, drop = FALSE]
    y_vector <- bootstrap_sample_df[[response_var]]
    
    cv_ridge_model <- NULL
    tryCatch({
      cv_ridge_model <- cv.glmnet(x_matrix, y_vector, alpha = 0, nfolds = min(10, nrow(x_matrix) - 1))
    }, error = function(e) {
      cat("Error in cv.glmnet for bootstrap sample", i, ":", e$message, "\n")
    })
    
    if (!is.null(cv_ridge_model)) {
      coeffs <- coef(cv_ridge_model, s = "lambda.min")
      bootstrap_coeffs_matrix[i, ] <- as.vector(coeffs)
    } else {
      bootstrap_coeffs_matrix[i, ] <- rep(NA, length(coeff_names))
    }
  }
  cat("Bootstrap complete.\n\n")
  
  bootstrap_coeffs_df <- as.data.frame(bootstrap_coeffs_matrix)
  bootstrap_coeffs_df <- bootstrap_coeffs_df[rowSums(is.na(bootstrap_coeffs_df)) < ncol(bootstrap_coeffs_df), ]
  
  # --- 4. Summarize Coefficients ---
  lower_quantile_name <- paste0(lower_quantile * 100, "%")
  upper_quantile_name <- paste0(upper_quantile * 100, "%")
  
  summary_coeffs <- apply(bootstrap_coeffs_df, 2, function(x) {
    stats <- c(
      Mean = round(mean(x, na.rm = TRUE),3),
      SD = round(sd(x, na.rm = TRUE),3),
      Median = round(median(x, na.rm = TRUE),3)
    )
    stats[lower_quantile_name] <- round(quantile(x, lower_quantile, na.rm = TRUE),3)
    stats[upper_quantile_name] <- round(quantile(x, upper_quantile, na.rm = TRUE),3)
    return(stats)
  })
  
  summary_table <- as.data.frame(t(summary_coeffs))
  
  coeffs_long <- bootstrap_coeffs_df %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Coefficient", values_to = "Value")
  
  if (shaded) {
    # --- Shaded Violin Plot with Horizontal Cut-off ---
    
    # Calculate quantiles for shading regions
    coeffs_quantiles <- coeffs_long %>%
      group_by(Coefficient) %>%
      summarise(
        q025 = quantile(Value, 0.025, na.rm = TRUE),
        q975 = quantile(Value, 0.975, na.rm = TRUE),
        q05 = quantile(Value, 0.05, na.rm = TRUE),
        q95 = quantile(Value, 0.95, na.rm = TRUE),
        .groups = 'drop'
      )
    
    coeffs_shaded_df <- left_join(coeffs_long, coeffs_quantiles, by = "Coefficient")
    
    # Build the layered plot. The base layer uses the full data.
    # Subsequent layers use pre-filtered data to create the horizontal cut-off effect.
    violin_plot <- ggplot(data = coeffs_shaded_df, aes(x = reorder(Coefficient, Value, FUN=median), y = Value)) +
      # Layer 1: Partial shade for 95% interval. Shape is based on pre-filtered data.
      geom_violin(data = . %>% filter(Value >= q025 & Value <= q975), aes(fill = Coefficient), color = "transparent", alpha = 0.4, trim = TRUE) +
      # Layer 2: Opaque shade for 90% interval. Shape is based on pre-filtered data.
      geom_violin(data = . %>% filter(Value >= q05 & Value <= q95), aes(fill = Coefficient), color = "transparent", alpha = 0.7, trim = TRUE) +
      # Layer 3: Outline for the entire distribution (no fill). Inherits full data from ggplot().
      geom_violin(color = "black", fill = "transparent", trim = TRUE) +
      # Add boxplot and median point for clarity. These use the full data.
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.7, outlier.shape = NA) +
      stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      labs(
        title = paste(title, " (", n_bootstraps, " Bootstraps)", sep=""),
        x = "Variable",
        y = "Coefficient Value (Standardised Scale)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none"
      )
  } else {
    # --- Original Violin Plot Logic ---
    violin_plot <- ggplot(coeffs_long, aes(x = reorder(Coefficient, Value, FUN=median), y = Value, fill = Coefficient)) +
      geom_violin(trim = TRUE, alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5, outlier.shape = NA) +
      labs(
        title = paste(title, " (", n_bootstraps, " Bootstraps)", sep=""),
        x = "Coefficient",
        y = "Coefficient Value (standardised Scale)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none"
      ) +
      stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1)
  }
  
  # --- 6. Return Results ---
  return(list(
    violin_plot = violin_plot,
    summary_table = summary_table
  ))
}

# Although it looks like the shaded gets cut off around 0 it is actually working
# It looks like this and not much of a distribution beyond 0, so bottom 2.5 and 5% v similar



# test <- run_bootstrap_ridge(
#   data = df_filtered_87_60_joined,
#   formula = model_formula,
#   n_bootstraps = 1000,
#   sig_level = 5
# )
# 
# test_lag <- run_bootstrap_ridge(
#   data = df_filtered_87_60_joined_lag,
#   formula = model_formula,
#   n_bootstraps = 1000,
#   sig_level = 5
# )

