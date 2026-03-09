## ====================================================================
#### ===== Objective 2.: FISCAL IMPACT OF INTEREST RATE ======= ####
rm(list = ls())
# Set working directory
setwd("C:/Users/Gerald/OneDrive/Desktop/Fridah/Dataset/Fridah")

# Load libraries
library(dplyr)      # Data manipulation
library(lmtest)     # For coeftest() and diagnostic tests
library(sandwich)   # For Newey-West HAC standard errors
library(car)        # For VIF (multicollinearity test)

library(ggplot2)

#### Load Dataset####

fiscal_data <- read.csv("Fiscal_channel.csv")
cat("Data loaded. Number of years (observations):", nrow(fiscal_data), "\n\n")
View(fiscal_data)

# Check missing values
is.na(fiscal_data)

# Fix na problem 
data <- fiscal_data[, -(7:12)]
View(data)

sum(is.na(data))
head(data)
str(data)

# Fix the structure of variables as numeric
data <- data %>% mutate(across( 
  c(Tax_rev, Debt_stock, Debt_serv, Exch_rate),
  ~ as.numeric(gsub("[^0-9.-]", "", .x)) 
))

str(data)

# Lets conducts some calculation to determine the Interest burden and Debt leverage
data <- data %>%
  mutate(
    Int_Burden = Debt_serv / Tax_rev,
    Debt_Leverage = Debt_stock / Tax_rev
  ) %>%
  arrange(Year)
head(data)

###==Descriptive statistics==####
cat("=== DESCRIPTIVE STATISTICS ===\n")
library(psych)

describe(data[, c("Debt_serv", "Tbill_rate", "Debt_stock", "Tax_rev", "Exch_rate")])

# Time series plot 
par(mfrow = c(2, 2))
plot(data$Year, data$Debt_serv, type = "l", col = "blue", 
     main = "Domestic Debt Service costs", xlab = "Year", ylab = "KSh Million")
plot(data$Year, data$Tbill_rate, type = "l", col = "red", 
     main = "T-bill Rate (Weighted Avg.)", xlab = "Year", ylab = "%")
plot(data$Year, data$Debt_stock, type = "l", col = "darkgreen", 
     main = "Domestic Debt Stock", xlab = "Year", ylab = "KSh Million")
plot(data$Year, data$Tax_rev, type = "l", col = "purple", 
     main = "Tax Revenue", xlab = "Year", ylab = "KSh Million")
plot(data$Year, data$Exch_rate, type = "l", col = "orange", 
     main = "Exchange Rate", xlab = "Year", ylab = "%")
par(mfrow = c(1, 1))

set.seed(123)

data <- data %>%
  mutate(
    Int_Burden = Debt_serv / Tax_rev,
    Debt_Leverage = Debt_stock / Tax_rev
  ) %>%
  arrange(Year)

str(data)
#### ===STATIONARITY TEST ####
# Let's perform stationarity or Unit root test
library(tseries)
key_vars<- c('Int_Burden', 'Tbill_rate', 'Debt_Leverage', 'Tax_rev', 'Exch_rate')

for (var in key_vars) {
  
  # Perform the Augmented Dickey-Fuller Test
  # Note: Use 'k=1' for annual data, and include a constant (drift)
  adf_result <- adf.test(data[[var]], k = 1)
  
  # Print the test result
  cat("\n--- ADF Test for:", var, "---\n")
  cat("Dickey-Fuller =", round(adf_result$statistic, 3), "\n")
  cat("p-value =", round(adf_result$p.value, 4), "\n")
  
  # Simple interpretation
  if (adf_result$p.value < 0.05) {
    cat("Conclusion: Stationary (Reject H0 of unit root)\n")
  } else {
    cat("Conclusion: NON-Stationary (Fail to reject H0). Differencing required.\n")
  }
}

#Use natural Log transformation
data <- data %>%
  mutate(
    ln_Int_Burden = log(Int_Burden),
    ln_Debt_Leverage = log(Debt_Leverage),
    ln_Tbill_rate = log(Tbill_rate),
    ln_Tax_rev = log(Tax_rev),
    ln_Exch_rate = log(Exch_rate)
  )
transformed_var<- c("ln_Int_Burden", "ln_Debt_Leverage", "ln_Tbill_rate", "ln_Tax_rev", "ln_Exch_rate")

# CREATE DIFFERENCED VARIABLES (IF NEEDED) & RE-TEST
# This creates new columns with a '_diff' suffix
for (var in transformed_var) {
  new_var_name <- paste0(var, "_diff")
  data[[new_var_name]] <- c(NA, diff(data[[var]])) # First value will be NA
  
  # Test the differenced series (skip the first NA)
  if(sum(!is.na(data[[new_var_name]])) > 5) { # Ensure enough data points
    adf_result_diff <- adf.test(na.omit(data[[new_var_name]]), k = 1)
    cat("\n--- ADF Test for Differenced:", new_var_name, "---\n")
    cat("p-value =", round(adf_result_diff$p.value, 4), "\n")
    cat("Dickey-Fuller =", round(adf_result$statistic, 3), "\n")
  }
}

head(data)

# Only two variables are non stationary
# ln_Debt_serv_diff, & ln_Tax_rev_diff

# Second Differencing
data$dd_ln_Int_Burden<- c(NA,NA, diff(diff(data$ln_Int_Burden)))
data$dd_ln_Debt_Leverage<- c(NA, NA, diff(diff(data$ln_Debt_Leverage)))
data$dd_ln_Tbill_rate<- c(NA, NA, diff(diff(data$ln_Tbill_rate)))
data$dd_ln_Tax_rev<- c(NA, NA, diff(diff(data$ln_Tax_rev)))
data$dd_ln_Exch_rate<- c(NA, NA, diff(diff(data$ln_Exch_rate)))

head(data)

# Confirm the stationarity after second difference on Debt stock
transformed_vars <- c("dd_ln_Int_Burden", "dd_ln_Tax_rev", "dd_ln_Tbill_rate", "dd_ln_Debt_Leverage", "dd_ln_Exch_rate")

cat("=== STATIONARITY TEST AFTER SECOND DIFFERENCED SERIES ===\n")
cat("H0: Series has a unit root (Non-stationary)\n")
cat("Reject H0 if p-value < 0.05 (Series is Stationary)\n\n")

# Create a results dataframe for a clean output
results_df <- data.frame(
  Variable = character(),
  ADF_Statistic = numeric(),
  P_Value = numeric(),
  Lag_Order = numeric(),
  Stationary = character(),
  stringsAsFactors = FALSE
)

for (var in transformed_vars) {
  # Extract the series, remove NA values created by differencing
  test_series <- na.omit(data[[var]])
  
  # Perform ADF Test. 
  # k = 0 or 1 is usually sufficient for annual differenced data.
  # Use 'k=0' to start (no lagged differences), but test can select 'k' automatically.
  adf_result <- adf.test(test_series, k = 0)
  
  # Determine stationarity
  is_stationary <- ifelse(adf_result$p.value < 0.05, "YES", "NO")
  
  # Store results
  results_df <- rbind(results_df, data.frame(
    Variable = var,
    ADF_Statistic = round(adf_result$statistic, 4),
    P_Value = round(adf_result$p.value, 4),
    Lag_Order = 0, # The lag order we specified
    Stationary = is_stationary
  ))
}
print(results_df)

head(data)


#### ===MODEL ESTIMATION===####
cat("\n=== MULTIPLE REGRESSION MODEL ===\n")
model <- lm(dd_ln_Int_Burden ~lag(dd_ln_Tbill_rate, 1) + dd_ln_Exch_rate  + lag(dd_ln_Debt_Leverage,1),
            data = data)

# Results with conventional standard errors (for reference)
cat("\n--- Results with Conventional Standard Errors ---\n")
summary(model)

# Show results with Newey-West HAC standard errors (CORRECT FOR TIME SERIES)
cat("\n--- Results with Newey-West Robust Standard Errors (Lag = 1) ---\n")
model_nw <- coeftest(model, vcov = NeweyWest(model, lag = 1, prewhite = FALSE))
print(model_nw)


#### ===4. DIAGNOSTIC TESTING ------------------------------
cat("\n=== MODEL DIAGNOSTICS ===\n")

# 4a. Test for Serial Correlation (Breusch-Godfrey, order 1)
cat("\n1. Serial Correlation Test (Breusch-Godfrey, order=1):\n")
bg_test <- bgtest(model, order = 1)
print(bg_test)
if(bg_test$p.value < 0.05) cat("   WARNING: Potential serial correlation.\n")

# 4b. Test for Heteroskedasticity (Breusch-Pagan)
cat("\n2. Heteroskedasticity Test (Breusch-Pagan):\n")
bp_test <- bptest(model)
print(bp_test)
if(bp_test$p.value < 0.05) cat("   WARNING: Potential heteroskedasticity.\n")

# 4c. Check for Multicollinearity (Variance Inflation Factor)
cat("\n3. Multicollinearity Check (VIF):\n")
vif_values <- vif(model)
print(vif_values)
if(any(vif_values > 5)) cat("   NOTE: VIF >5 suggests moderate multicollinearity.\n")
if(any(vif_values > 10)) cat("   WARNING: VIF >10 indicates serious multicollinearity.\n")

# 4d. Check for Influential Outliers (Cook's Distance)
cooks_d <- cooks.distance(model)
influential <- which(cooks_d > (4 / nrow(fiscal_data))) # Standard threshold
cat("\n4. Influential Observations (Cook's Distance > 4/n):\n")

if(length(influential) > 0) {
  cat("   Year(s):", fiscal_data$year[influential], "\n")
  cat("   Consider verifying data for these years.\n")
} else {
  cat("   No highly influential observations detected.\n")
}
print(cooks_d)


library(tseries)

# Perform the Jarque-Bera test on the model residuals
jb_test <- jarque.bera.test(residuals(model))
print(jb_test)
