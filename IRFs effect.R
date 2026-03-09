#=====================================================
# OBJECTIVE 3: NET EFFECT ON MACROECONOMIC PERFORMANCE
# VAR model with impulse Response Function(IRF)
#=====================================================
setwd("C:/Users/Gerald/OneDrive/Desktop/Fridah/Dataset/Fridah")

# Load required library
library(vars)
library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)

#====================================================
# 1. Data preparation
#_____________________________________________________
net_data<- read.csv("Private_credit.csv", stringsAsFactors = FALSE)
head(net_data)

# Remove unnecessary X columns
clean_data <- net_data[, c("Year", "Quarter", "RGDP", "RIR", "INF", "PSC", "Exch_rate")]
head(clean_data)

# creating proper time index (quarterly)
clean_data$FiscalYear <- clean_data$Year

# Shift Q3 and Q4 to the next calendar year
clean_data$FiscalYear[clean_data$Quarter %in% c("Q3", "Q4")] <- 
  clean_data$FiscalYear[clean_data$Quarter %in% c("Q3", "Q4")] + 1

# Quarter end dates for Kenya fiscal year
quarter_end_day <- c(
  "Q1" = "09-30", 
  "Q2" = "12-31", 
  "Q3" = "03-31",
  "Q4" = "06-30" 
)

# Create proper fiscal-year quarterly dates
clean_data$Time <- as.Date(
  paste0(clean_data$FiscalYear, "-", quarter_end_day[clean_data$Quarter], "-01")
)



str(clean_data)
head(clean_data)
View(clean_data)

# 3. STATIONARITY TESTS (CRITICAL FOR VAR)
cat("\n=== STATIONARITY TESTS (ADF) ===\n")
cat("Null Hypothesis: Series has a unit root (non-stationary)\n")
cat("Reject H0 if p-value < 0.05 (series is stationary)\n\n")

var_list <-c("PSC", "RIR", "INF", "RGDP", "Exch_rate")
stationarity_results <- data.frame()

for (var in var_list) {
  adf_result<- adf.test(na.omit(clean_data[[var]]), k= 1)
  
  #store results
  stationarity_results <- rbind(stationarity_results,
                                data.frame(
                                  Variable = var,
                                  ADF_Statistics = round(adf_result$statistic, 3),
                                  P_Value = round(adf_result$p.value, 4),
                                  Stationary= ifelse(adf_result$p.value < 0.05, "Yes", "No")
                                ))
}

print(stationarity_results)

# create stationarity series for VAR
cat("\n=== CREATE STATIONARY SERIES ===\n")

clean_data$d_RGDP <- c(NA, diff(clean_data$RGDP))
clean_data$d_RIR <- c(NA, diff(clean_data$RIR))
clean_data$d_INF <- c(NA, diff(clean_data$INF))
clean_data$d_PSC <- c(NA, diff(clean_data$PSC))
clean_data$d_Exch <- c(NA, diff(clean_data$Exch_rate))

head(clean_data)
cat("\n=== STATIONARITY TESTS (ADF)  AFTER FIRST DIFF===\n")
cat("Null Hypothesis: Series has a unit root (non-stationary)\n")
cat("Reject H0 if p-value < 0.05 (series is stationary)\n\n")

var_list <-c("d_PSC", "d_RIR", "d_INF", "d_RGDP", "d_Exch")
stationarity_results <- data.frame()

for (var in var_list) {
  adf_result<- adf.test(na.omit(clean_data[[var]]), k= 1)
  
  #store results
  stationarity_results <- rbind(stationarity_results,
                                data.frame(
                                  Variable = var,
                                  ADF_Statistics = round(adf_result$statistic, 3),
                                  P_Value = round(adf_result$p.value, 4),
                                  Stationary= ifelse(adf_result$p.value < 0.05, "Yes", "No")
                                ))
}

print(stationarity_results)

var_data <- na.omit(clean_data[, c("d_RGDP", "d_RIR", "d_INF", "d_PSC", "d_Exch")])
colnames(var_data) <- c("RGDP", "RIR", "INF", "PSC", "EXCh_rate")
head(var_data, 4)

# Confirm no. obs & Time period
cat("\nNumber of observations for VAR:", nrow(var_data), "\n")
cat("Time period:", min(clean_data$Year[!is.na(var_data$RGDP)]), "to", 
    max(clean_data$Year[!is.na(var_data$RGDP)]), "\n")

# Convert to time series
var_ts <- ts(var_data, start = c(2004, 1), frequency = 4)
View(var_ts)

# 5. OPTIMAL LAG SELECTION
cat("\n=== OPTIMAL LAG SELECTION ===\n")
lag_selection <- VARselect(var_ts, lag.max = 8, type = "const")  # 8 lags max for quarterly
print(lag_selection$selection)

# Create  table
lag_criteria <- data.frame(
  Lag = 1:8,
  AIC = round(lag_selection$criteria[1,], 3),
  HQ = round(lag_selection$criteria[2,], 3),
  SC = round(lag_selection$criteria[3,], 3),
  FPE = round(lag_selection$criteria[4,], 3)
)
print(lag_criteria)
set.seed(123)

# Choose lag (SC/BIC is more conservative, AIC might overfit)
optimal_lag <- as.numeric(lag_selection$selection[3])  # SC/BIC
cat("\nOptimal lag selected (SC/BIC):", optimal_lag)

# 6. ESTIMATE VAR MODEL
cat("\n\n=== VAR MODEL ESTIMATION ===\n")
var_model <- VAR(var_ts, p = optimal_lag, type = "const")
summary(var_model)

# 7. VAR DIAGNOSTICS
cat("\n\n=== VAR DIAGNOSTICS ===\n")

# 7.1 Stability check
roots <- roots(var_model)
cat("\nRoots of companion matrix:\n")
print(roots)
cat("All roots < 1? ", all(abs(roots) < 1), " - Model is stable\n")

# Plot stability
plot(stability(var_model), nc = 2)

# 7.2 Serial correlation
cat("\n--- Serial Correlation Test ---\n")
serial_test <- serial.test(var_model, lags.pt = 10, type = "PT.asymptotic")
print(serial_test)

# 7.3 Heteroskedasticity
cat("\n--- ARCH Test ---\n")
arch_test <- arch.test(var_model, lags.multi = 5)
print(arch_test)

# 7.4 Normality
cat("\n--- Normality Test ---\n")
norm_test <- normality.test(var_model, multivariate.only = TRUE)
print(norm_test)

# 8. IMPULSE RESPONSE FUNCTIONS (KEY FOR OBJECTIVE 3)
cat("\n\n=== IMPULSE RESPONSE FUNCTIONS ===\n")

# IRF: Response of all variables to interest rate (RIR) shock
irf_int <- irf(var_model, impulse = "RIR", response = c("RGDP", "INF", "EXCh_rate", "PSC"), 
               n.ahead = 12,  # 12 quarters = 3 years
               boot = TRUE, runs = 500,
               ci = 0.95)

# plot all IRFs 
plot(irf_int)

# 9. FORECAST ERROR VARIANCE DECOMPOSITION
cat("\n\n=== FORECAST ERROR VARIANCE DECOMPOSITION ===\n")
fevd_result <- fevd(var_model, n.ahead = 12)
print(fevd_result)


# Plot FEVD
plot(fevd_result)

# testing for different colors 
par(mfrow = c(2, 3))
invisible(lapply(names(fevd_result), function(v) { 
  matplot(fevd_result[[v]], type="l", lwd=2,
          col=c("red","blue","darkgreen","purple","orange"), lty=1,
          main=paste("FEVD for", v), xlab="Horizon", ylab="Contribution (%)")
  legend("topright", legend=colnames(fevd_result[[v]]),
         col=c("red","blue","darkgreen","purple","orange"), lty=1, lwd=2)
}))
par(mfrow = c(1,1))

# Bar charts
par(mfrow= c(3, 2))
invisible(lapply(names(fevd_result), function(v) {
  barplot(
    t(fevd_result[[v]]),
    beside = TRUE,
    col = c("red","blue","darkgreen","purple","orange"),
    main = paste("FEVD for", v),
    xlab = "Horizon",
    ylab = "Contribution (%)"
  )
  legend(
    "topright",
    legend = colnames(fevd_result[[v]]),
    fill = c("red","blue","darkgreen","purple","orange")
  )
}))
par(mfrow= c(1,1 ))

# 10. GRANGER CAUSALITY TESTS
cat("\n\n=== GRANGER CAUSALITY TESTS ===\n")
# Does interest rate cause other variables?
gc_int <- causality(var_model, cause = "RIR")
print(gc_int)

# Individual IRFs plot
# GDP Response
irf_gdp <- data.frame(
  Period = 0:(length(resp)-1),
  Response = irf_int$irf$RGDP,
  Lower = irf_int$Lower$RGDP,
  Upper = irf_int$Upper$RGDP
)

p1 <- ggplot(irf_gdp, aes(x = Period, y = Response)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "GDP Response to Interest Rate Shock",
       x = "Quarters after shock", y = "Response") +
  theme_minimal()
