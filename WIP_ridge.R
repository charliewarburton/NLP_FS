library(glmnet)
df_standardized <- df_filtered_87_60_joined
# df_standardized <- gemini_joined
colnames(df_standardized)[2] <- "Sentiment.Index"


# Identify numeric columns to scale (all except 'Report')
cols_to_scale <- setdiff(names(df_standardized), "Report")
cols_to_scale <- cols_to_scale[sapply(df_standardized[cols_to_scale], is.numeric)] # Ensure they are numeric

# Apply scaling
# Using as.numeric(scale(x)) to ensure the output is a simple vector, not a matrix with attributes
df_standardized[cols_to_scale] <- lapply(df_standardized[cols_to_scale], function(x) as.numeric(scale(x)))

# Check standardized data (means should be ~0, sd ~1 for scaled columns)
#
#print(head(df_standardized))
print(sapply(df_standardized[cols_to_scale], mean))
# print(sapply(df_standardized[cols_to_scale], sd))

# 3. Define the model formula
# This is the formula from your original lm() call
model_formula <- Sentiment.Index ~ Credit.To.GDP.Gap + household_dsr + Total.Credit.To.GDP+ 
  house_price_yoy + pnfc_dsr + Price.Book.Ratio + CDS + SRISK+ VIX+ PNF.Credit.Growth

# 4. Bootstrap loop for Ridge Regression
num_bootstraps <- 1000
all_coeffs_list <- list() # To store coefficients from each bootstrap

# Get predictor names from the formula (excluding the response)
predictor_vars <- all.vars(model_formula)[-1]

# Get coefficient names once (including intercept) for consistent storage
# Create a temporary model matrix and y to get coefficient names
temp_x_for_names <- model.matrix(model_formula, data = df_standardized)[, -1, drop = FALSE] # Predictors, remove intercept
temp_y_for_names <- df_standardized$Sentiment.Index
# Fit a quick glmnet to get coefficient names (any small lambda will do)
# glmnet adds its own intercept, so the names will include "(Intercept)"
temp_glmnet_fit <- glmnet(temp_x_for_names, temp_y_for_names, alpha = 0, lambda = 0.01)
coeff_names <- rownames(coef(temp_glmnet_fit))

# Pre-allocate a matrix for efficiency (optional, but good practice for many iterations)
bootstrap_coeffs_matrix <- matrix(NA, nrow = num_bootstraps, ncol = length(coeff_names),
                                  dimnames = list(NULL, coeff_names))

set.seed(456) # for reproducibility of bootstrap
for (i in 1:num_bootstraps) {
  if (i %% 100 == 0) cat("Bootstrap sample:", i, "/", num_bootstraps, "\n")
  
  # Create bootstrap sample (sampling rows with replacement)
  sample_indices <- sample(1:nrow(df_standardized), size = nrow(df_standardized), replace = TRUE)
  bootstrap_sample_df <- df_standardized[sample_indices, ]
  
  # Prepare X (matrix of predictors) and y (response vector)
  # model.matrix automatically handles formula and creates dummy variables if needed
  # We remove the intercept column [,-1] because cv.glmnet fits an intercept by default
  x_matrix <- model.matrix(model_formula, data = bootstrap_sample_df)[, -1, drop = FALSE]
  y_vector <- bootstrap_sample_df$Sentiment.Index
  
  # Perform Ridge Regression (alpha = 0)
  # Use cross-validation to find the optimal lambda
  # Handle potential errors during cv.glmnet, e.g., if a resampled dataset is problematic
  # (though less likely with numeric data and sufficient samples)
  cv_ridge_model <- NULL
  tryCatch({
    # Note: for very small N or N < P, glmnet might issue warnings or errors.
    # Ensure nfolds is less than the number of observations in the bootstrap sample.
    # Since we sample with replacement, nrow(bootstrap_sample_df) is same as original.
    # If original N is very small, this could be an issue.
    # For ridge, it's generally robust.
    cv_ridge_model <- cv.glmnet(x_matrix, y_vector, alpha = 0, nfolds = min(10, nrow(x_matrix)-1))
  }, error = function(e) {
    cat("Error in cv.glmnet for bootstrap sample", i, ":", e$message, "\n")
  })
  
  if (!is.null(cv_ridge_model)) {
    # Extract coefficients at lambda.min (lambda that gives minimum CV error)
    coeffs <- coef(cv_ridge_model, s = "lambda.min")
    bootstrap_coeffs_matrix[i, ] <- as.vector(coeffs) # Store as a row in the matrix
  } else {
    # If cv.glmnet failed, fill with NAs (or handle as appropriate)
    bootstrap_coeffs_matrix[i, ] <- rep(NA, length(coeff_names))
  }
}
cat("Bootstrap complete.\n")

# Convert matrix to a data frame for easier plotting with ggplot2
bootstrap_coeffs_df <- as.data.frame(bootstrap_coeffs_matrix)

# Remove rows with all NAs if any cv.glmnet failed (unlikely for this setup but good check)
bootstrap_coeffs_df <- bootstrap_coeffs_df[rowSums(is.na(bootstrap_coeffs_df)) < ncol(bootstrap_coeffs_df), ]

coeffs_long <- bootstrap_coeffs_df %>%
  pivot_longer(cols = everything(), names_to = "Coefficient", values_to = "Value")

# Create the violin plot
violin_plot <- ggplot(coeffs_long, aes(x = Coefficient, y = Value, fill = Coefficient)) +
  geom_violin(trim = T, alpha = 0.7) + # trim=FALSE shows full distribution tails
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5, outlier.shape = NA) + # Add boxplot inside
  labs(title = "Distribution of Ridge Regression Coefficients (Bootstrap)",
       x = "Coefficient",
       y = "Coefficient Value (Standardized Scale)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none") + # Hide legend if fills are just for aesthetics
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") # Add horizontal line at 0
violin_plot


# May be worth using 95% and 5%
summary_coeffs <- apply(bootstrap_coeffs_df, 2, function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    `2.5%` = quantile(x, 0.025, na.rm = TRUE),
    `97.5%` = quantile(x, 0.975, na.rm = TRUE))
})
print("Summary of Bootstrapped Coefficients:")
print(t(summary_coeffs)) # Transpose for better readability
