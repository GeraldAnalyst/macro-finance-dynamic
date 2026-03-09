# ============================================================
  rm(list = ls())
# Set working Directory
setwd("C:/Users/Gerald/OneDrive/Desktop/Fridah/Dataset/Fridah")

#Load relevant library
library(dplyr)
library(tseries)
library(ARDL)
library(lmtest)
library(sandwich) 

# read data
data <- read.csv('Private_credit.csv')
head(data)

str(data)
dim(data)
psc_data <- data[, -(8:10)]
head(psc_data)
sum(is.na(psc_data))
#Ensure chronological order
psc_data<- psc_data%>%arrange(Year)

head(psc_data)
View(psc_data)
str(psc_data)
set.seed(123)
#### Generating Descriptive Statistics ####

# calculate descriptive statistics
desc_stats <- psc_data %>%
  dplyr:: select(PSC, RIR, INF, RGDP, Exch_rate) %>%
  dplyr:: summarise(across(everything(),
                   list(
                     Mean = ~mean(., na.rm = TRUE),
                     SD = ~sd(., na.rm = TRUE),
                     Min = ~min(., na.rm = TRUE),
                     Max = ~max(., na.rm = TRUE),
                     obs = ~sum(!is.na(.))
                   ),
                   .names = "{.col}_{.fn}"))
print(t(desc_stats))

# Descriptive statistics table
descr_table<- data.frame(
  variable = c("Private Sector Credit growth (%)", "Real Interest Rate (%)",
               "Inflation Rate (%)", "Real GDP Growth (%)", "Exchange Rate (KES/USD)"
               ),
  Mean = c(mean(psc_data$PSC, na.rm = TRUE),
           mean(psc_data$RIR, na.rm = TRUE),
           mean(psc_data$INF, na.rm = TRUE),
           mean(psc_data$RGDP, na.rm = TRUE),
           mean(psc_data$Exch_rate, na.rm = TRUE)),
  StdDev = c(sd(psc_data$PSC, na.rm = TRUE),
           sd(psc_data$RIR, na.rm = TRUE),
           sd(psc_data$INF, na.rm = TRUE),
           sd(psc_data$RGDP, na.rm = TRUE),
           sd(psc_data$Exch_rate, na.rm = TRUE)),
  Min = c(min(psc_data$PSC, na.rm = TRUE),
           min(psc_data$RIR, na.rm = TRUE),
           min(psc_data$INF, na.rm = TRUE),
           min(psc_data$RGDP, na.rm = TRUE),
           min(psc_data$Exch_rate, na.rm = TRUE)),
  Max = c(max(psc_data$PSC, na.rm = TRUE),
           max(psc_data$RIR, na.rm = TRUE),
           max(psc_data$INF, na.rm = TRUE),
           max(psc_data$RGDP, na.rm = TRUE),
           max(psc_data$Exch_rate, na.rm = TRUE)),
  observations = c(sum(!is.na(psc_data$PSC)),
                   sum(!is.na(psc_data$RIR)),
                   sum(!is.na(psc_data$INF)),
                   sum(!is.na(psc_data$RGDP)),
                   sum(!is.na(psc_data$Exch_rate)))
  
)

print(descr_table)

#### Create timeseries trend plots ####
# ----------------------------------------------------------------------
# FOUR-PANEL DUAL-AXIS VISUALIZATION
# ----------------------------------------------------------------------

par(mfrow = c(2, 2), mar = c(4, 4, 3, 4) + 0.5)

# Panel 1: GDP Growth vs Inflation
plot(psc_data$Year, psc_data$RGDP, type = "b", pch = 16, lwd = 2, col = "#2E86AB",
     main = "Growth vs Inflation", xlab = "Year", ylab = "GDP Growth (%)", 
     xaxt = "n")
axis(1, at = seq(2005, 2020, 5))
grid(col = "gray90")
par(new = TRUE)
plot(psc_data$Year, psc_data$INF, type = "b", pch = 15, lwd = 2, col = "#F18F01",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 35))
axis(4, col = "#F18F01", col.axis = "#F18F01")
mtext("Inflation (%)", side = 4, line = 2, col = "#F18F01", cex = 0.8)
legend("topleft", legend = c("RGDP", "INF"), col = c("#2E86AB", "#F18F01"), 
       pch = c(16, 15), lty = 1, bty = "n", cex = 0.7)

# Panel 2: Interest Rate vs Inflation
plot(psc_data$Year, psc_data$RIR, type = "b", pch = 17, lwd = 2, col = "#A23B72",
     main = "Interest Rate vs Inflation", xlab = "Year", ylab = "Real Interest Rate (%)",
     xaxt = "n")
axis(1, at = seq(2005, 2020, 5))
grid(col = "gray90")
par(new = TRUE)
plot(psc_data$Year, psc_data$INF, type = "b", pch = 15, lwd = 2, col = "#F18F01",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 35))
axis(4, col = "#F18F01", col.axis = "#F18F01")
mtext("Inflation (%)", side = 4, line = 2, col = "#F18F01", cex = 0.8)
legend("topleft", legend = c("RIR", "INF"), col = c("#A23B72", "#F18F01"), 
       pch = c(17, 15), lty = c(2, 1), bty = "n", cex = 0.7)

# Panel 3: Credit Growth vs Exchange Rate
plot(psc_data$Year, psc_data$PSC, type = "b", pch = 16, lwd = 2, col = "#2A9D8F",
     main = "Credit vs Exchange Rate", xlab = "Year", ylab = "Credit Growth (%)",
     xaxt = "n")
axis(1, at = seq(2005, 2020, 5))
grid(col = "gray90")
par(new = TRUE)
plot(psc_data$Year, psc_data$Exch_rate, type = "b", pch = 17, lwd = 2, col = "#E76F51",
     axes = FALSE, xlab = "", ylab = "", ylim = c(50, 160))
axis(4, col = "#E76F51", col.axis = "#E76F51")
mtext("Exchange Rate", side = 4, line = 2, col = "#E76F51", cex = 0.8)
legend("topleft", legend = c("PSC", "Exch Rate"), col = c("#2A9D8F", "#E76F51"), 
       pch = c(16, 17), lty = 1, bty = "n", cex = 0.7)

# Panel 4: All variables summary (simplified)
plot(psc_data$Year, scale(psc_data$RGDP), type = "l", lwd = 2, col = "#2E86AB",
     main = "Standardized Trends (Z-scores)", xlab = "Year", ylab = "Standardized Values",
     ylim = c(-2.5, 2.5), xaxt = "n")
axis(1, at = seq(2005, 2020, 5))
grid(col = "gray90")
lines(psc_data$Year, scale(psc_data$PSC), lwd = 2, col = "#2A9D8F")
lines(psc_data$Year, scale(psc_data$RIR), lwd = 2, lty = 2, col = "#A23B72")
lines(psc_data$Year, scale(psc_data$INF), lwd = 2, col = "#F18F01")
lines(psc_data$Year, scale(psc_data$Exch_rate), lwd = 2, col = "#E76F51")
abline(h = 0, lty = 3, col = "gray50")
legend("topright", legend = c("RGDP", "PSC", "RIR", "INF", "Exch"), 
       col = c("#2E86AB", "#2A9D8F", "#A23B72", "#F18F01", "#E76F51"),
       lty = c(1, 1, 2, 1, 1), lwd = 2, bty = "n", cex = 0.6, ncol = 2)

par(mfrow = c(1,1))


#### Stationarity Testing ####
# Performance ADF tests on all variables
cat("=== AUGMENTED DICKEY - FULLER TESTS FOR OBJECTIVE 1 ===\n")
cat("Null Hypothesis: Series has a unit root (non-stationary)\n")
cat("Reject H0 if p-value < 0.05 (series is stationary)\n\n")

variables <-c("PSC", "RIR", "INF", "RGDP", "Exch_rate")
stationarity_results <- data.frame()

for (var in variables) {
  adf_result<- adf.test(na.omit(psc_data[[var]]), k= 1)
  
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

## Conducting the first differencing 
psc_data$PSC_diff <- c(NA, diff(psc_data$PSC))
#psc_data$RIR_diff <- c(NA, diff(psc_data$RIR))

head(psc_data)

# check for stationarity 
# Performance ADF tests on all variables
cat("=== AUGMENTED DICKEY - FULLER TESTS FOR OBJECTIVE 1 ===\n")
cat("Null Hypothesis: Series has a unit root (non-stationary)\n")
cat("Reject H0 if p-value < 0.05 (series is stationary)\n\n")

variables <-c("PSC_diff", "RIR", "INF", "RGDP", "Exch_rate")
stationarity_results <- data.frame()

for (var in variables) {
  adf_result<- adf.test(na.omit(psc_data[[var]]), k= 1)
  
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

# The variable private sector growth and real interest rates are stationary at first difference

#install.packages("dLagM")
library(dLagM)
library(ARDL)
# Lets run the ARDL model
#### MODEL ESITIMATION ####
# For ARDL model we use the original data, or at level
ardl_model <- auto_ardl(PSC ~ RIR + INF + RGDP + Exch_rate, 
                   data = psc_data,
                   max_order = c(4,4,4,4,4), 
                   selection = "AIC",
                   grid = TRUE)

ardl_model$top_orders
cat ("\n ARDL  order (p, q1, q2, q3, q4): \n")
print(ardl_model$best_order)

ardl_10241<- ardl_model$best_model
ardl_10241$order

summary(ardl_10241)

# Estimate Unrestricted Error Correction Model (UECM)
uecm_10241<- uecm(ardl_10241)
summary(uecm_10241)

# Estimating Restricted Error Correction Model (RECM)
recm_10241<- recm(uecm_10241, case = 2)
summary(recm_10241)

# Test if there is a long-run levels relationship (Cointegration) using bounds test
bounds_f_test(ardl_10241, case = 2)

# Bounds t test (under case 3) assuming alpha = 0.05
tbounds <- bounds_t_test(uecm_10241, case = 3, alpha = 0.01)
tbounds
tbounds$tab

# Let's get the multipliers for short run 
multipliers(ardl_10241, type = "sr")
multipliers(ardl_10241, type = "lr")

# Lets visualize and delay multipliers 
mult15 <- multipliers(ardl_10241, type = 15, se = TRUE)
plot_delay(mult15, interval = 0.95)

# lets check the estimated cointegration
ce <- coint_eq(ardl_10241, case = 2)
plot_lr(ardl_10241, coint_eq = ce, show.legend = TRUE)

set.seed(125)
#### Diagnostic test ####
# 1. Serial correlation (Breusch-Godfrey LM test)
cat("\n No Serial correlation if p-value > 0.05\n")
lmtest::bgtest(ardl_10241)

# 2. Heteroskedasticity (Breusch-Pagan test)
cat("\n Check whether the variance of residuals is constant \n")
cat("\n If p-value > 0.05 homoskedastic residual (constant variance)\n")
lmtest::bptest(ardl_10241)

# 3. Normality
cat("\n Confirm if the residuals are normally distributed \n")
cat("\n Normal residual if p-value is > 0.05 \n")
tseries::jarque.bera.test(residuals(ardl_10241))

# 4. Functional Form (RESET test)
cat("\n No functional form misspecification if p-value > 0.05")
lmtest::resettest(ardl_10241)

# 5. Stability tests (CUSUM & CUSUMSQ)
library(strucchange)

# Extract residuals and regressors
res <- residuals(ardl_10241)
X   <- model.matrix(ardl_10241)

# Run the CUMSUM test 
cusum_test <- efp(res ~ X - 1, type = "Rec-CUSUM")
plot(cusum_test)

# Run the CUMSUMSQ
cusumsq_test <- efp(res ~ X - 1, type = "OLS-CUSUM")
plot(cusumsq_test)


