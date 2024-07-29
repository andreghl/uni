# Code

load("BEL_data.Rdata")
attach(BEL_data)

# View variables

names(BEL_data)

# Load libraries

library(tseries)
library(car)
library(carData)
library(bootUR)
library(vars)
library(sandwich)


# Importing Variables

YO_ts <- ts(BEL_data$YO, start = 1950, frequency = 1) # GDP (or GNP) at base year (2017), cOnstant national prices
KO_ts <- ts(BEL_data$KO, start = 1950, frequency = 1) # Physical capital stock valued at base year, cOnstant prices
L_ts <- ts(BEL_data$EMPL, start = 1950, frequency = 1) # Number of persons engaged/ EMPLoyed (in millions) 

# Creating Variables

YOA <- YO_ts / L_ts 
KOA <- KO_ts / L_ts
YKO <- YO_ts / KO_ts

# Defining Variables

lnYOA <- log(YOA) # Labor Productivity
lnKOA <- log(KOA) # Capital to Labor ratio
lnYKO <- lnYOA - lnKOA # Output per capital unit

ts.plot(lnKOA, xlab = "Time", ylab = "KOA")
title(main = "Time plot of Capital to Labor ratio", sub = "In the logarithmic form")

ts.plot(lnYOA, xlab = "Time", ylab = "YOA")
title(main = "Time plot of Labor Productivity", sub = "In the logarithmic form")

ts.plot(lnYKO, xlab = "Time", ylab = "YKO")
title(main = "Time plot of Output per Capital unit", sub = "In the logarithmic form")


dlnKOA <- diff(lnKOA)
dlnYOA <- diff(lnYOA)
dlnYKO <- diff(lnYKO)


ts.plot(dlnKOA, xlab = "Time", ylab = "KOA")
title(main = "Time plot of Capital to Labor ratio", sub = "In the logarithmic & difference form")

ts.plot(dlnYOA, xlab = "Time", ylab = "YOA")
title(main = "Time plot of Labor Productivity", sub = "In the logarithmic & difference form")

ts.plot(dlnYKO, xlab = "Time", ylab = "YKO")
title(main = "Time plot of Output per Capital unit", sub = "In the logarithmic & difference form")


union_dlnKOA = boot_union(dlnKOA)
union_dlnYOA = boot_union(dlnYOA)
union_dlnYKO = boot_union(dlnYKO)

union_dlnKOA
union_dlnYOA
union_dlnYKO


adf(dlnYOA, deterministics = "none", max_lag = 0)

HCAP_ts <- ts(BEL_data$HCAP, start = 1950, frequency = 1)

lnHCAP <- log(HCAP_ts)
dHCAP <- diff(HCAP_ts)
dlnHCAP <- diff(lnHCAP)

ts.plot(HCAP_ts, xlab = "Time", ylab = "HCAP")
title(main = "Time plot of Human Capital Index")

POP_ts <- ts(BEL_data$POP, start = 1950, frequency = 1)

lnPOP <- log(POP_ts)
dlnPOP <- diff(lnPOP)

ts.plot(POP_ts, xlab = "Time", ylab = "POP")
title(main = "Time plot of Population")


# Labor force

LF <- L_ts / POP_ts

plot(POP_ts, main = "Population")

plot(L_ts, main ="Number of Employed")

plot(LF, main = "Employed over Population")

plot(dHCAP, main = "Human Cap Index")


fit_lnYOA_HCAP <- lm(lnYOA~HCAP_ts)
summary(fit_lnYOA_HCAP)

resid_lnYOA_HCAP <- fit_lnYOA_HCAP$residuals

plot(x = resid_lnYOA_HCAP, type = "l")

GO_ts <- ts(BEL_data$GO, start = 1950, frequency = 1)

plot(GO_ts, main = "Government Speanding")

lnGO <- log(GO_ts)

plot(lnGO, main = "Gov Spending")

dlnGO <- diff(lnGO)

plot(dlnGO, main = "Gov Spend")

t_64 <- 1:64

fit_GO_t <- lm(dlnGO~t_64)
summary(fit_GO_t)
plot(fit_GO_t$residuals, type ="l")

resid_GO_t <- fit_GO_t$residuals

adf(resid_GO_t, deterministics = "trend", max_lag = 0)

union_residGO = boot_union(resid_GO_t)
union_residGO

Y0_ts <- ts(BEL_data$YO, start = 1950, frequency = 1)

lnYO <- log(YO)

fit_lnYO_lnYOA <- lm(lnYO~lnYOA)
summary(fit_lnYO_lnYOA)

ts.plot(lnYO, lnYOA, col = c("black", "blue"))

dlnYO <- diff(lnYO)

ts.plot(dlnYO, dlnGO, col = c("black", "blue"))
ts.plot(lnYO, lnGO, col = c("black", "blue"))

fit_dlnYO_dlnGO <- lm(dlnYO~dlnGO)
summary(fit_dlnYO_dlnGO)

plot(dlnGO, main = "Gov Spend", type = "l")

plot(dlnYO, main = "Output", type = "l")

adf(dlnGO, deterministics = "trend", max_lag = 0)

XO_ts <- ts(BEL_data$XO, start = 1950, frequency = 1)

lnXO <- log(XO_ts)

plot(lnXO, type = "l")

adf(lnXO, deterministics = "trend")

dlnXO <- diff(lnXO)

adf(dlnXO, deterministics = "intercept")

t_65 <- 1:65

fit_lnXO_t <- lm(lnXO~t_65)
summary(fit_lnXO_t)

resid_lnXO <- fit_lnXO_t$residuals

plot(resid_lnXO, type = "l")

adf(resid_lnXO, deterministics = "intercept")

resid_dlnXO <- diff(resid_lnXO)

plot(resid_dlnXO, type = "l")

adf(resid_dlnXO, deterministics = "none")

boot_dlnXO <- boot_union(dlnXO)
boot_dlnXO

boot_dlnYO <- boot_union(dlnYO)
boot_dlnYO

d2lnYO <- diff(dlnYO)

boot_d2lnYO <- boot_union(d2lnYO)
boot_d2lnYO

fit_lnYO_t <- lm(lnYO~t_65)
summary(fit_lnYO_t)

residlnYO <- fit_lnYO_t$residuals

plot(residlnYO, type = "l")


boot_residlnYO <- boot_union(residlnYO)
boot_residlnYO
  
dresidlnYO <- diff(residlnYO) 

plot(dresidlnYO, type = "l")
plot(dlnYO, type = "l")

boot_dresidlnYO <- boot_union(dresidlnYO)
boot_dresidlnYO
  
  

adf(dlnYO, deterministics = "none")

# Output (Constant)

YO_ts <- ts(BEL_data$YO, start = 1950, frequency = 1)

lnYO <- log(YO_ts)

dlnYO <- diff(lnYO)

# Output (Current)

YU_ts <- ts(BEL_data$YU, start = 1950, frequency = 1)

lnYU <- log(YU_ts)

dlnYU <- diff(lnYU)

# Exports (Constant)

XO_ts <- ts(BEL_data$XO, start = 1950, frequency = 1)

lnXO <- log(XO_ts)

dlnXO <- diff(lnXO)

# Exports (Current)

XU_ts <- ts(BEL_data$XU, start = 1950, frequency = 1)

lnXU <- log(XU_ts)

dlnXU <- diff(lnXU)

# Imports (Constant)

MO_ts <- ts(BEL_data$MO, start = 1950, frequency = 1)

lnMO <- log(MO_ts)

dlnMO <- diff(lnMO)

# Imports (Current)

MU_ts <- ts(BEL_data$MU, start = 1950, frequency = 1)

lnMU <- log(MU_ts)

dlnMU <- diff(lnMU)

# Price deflator | Exports

PX_ts <- XU_ts / XO_ts

lnPX <- log(PX_ts)

dlnPX <- diff(lnPX)

# Price deflator | Imports

PM_ts <- MU_ts / MO_ts

lnPM <- log(PM_ts)

dlnPM <- diff(lnPM)

# Price deflator | GDP

PY_ts <- YU_ts / YO_ts

lnPY <- log(PY_ts)

dlnPY <- diff(lnYO)

library(readr)

wages <- read_csv("Datasets/AverageWages(1991-2022).csv")
attach(wages)
names(wages)

# Import Average Wages (OECD)

W_ts <- ts(wages$Value, start = 1991, frequency = 1)

lnW <- log(W_ts)

lnW <- lnW[1:23]

dlnW <- diff(lnW)

# Test

lnPY[43]

b <- lnPY[43:65]

a <- b - dlnW

plot(a, type ="l")

fit_lnPY_lnW <- lm(b~lnW)
summary(fit_lnPY_lnW)

ts.plot(YO_ts, xlab = "Time", ylab = "YO")
title(main = "Output")

plot(dlnYO, type = "l", xlab = "Time", ylab = "YO")
title(main = "Output", sub = "In the difference & logarithmic form")

plot(dresidlnYO, type = "l")

acf(dlnYO)

ts.plot(LF, xlab = "Time", ylab = "LF")
title(main = "Labor Force")


lnLF <- log(LF)

dlnLF <- diff(lnLF)

ts.plot(lnLF, xlab = "Time", ylab = "Labor Force")
title(main = "Labor Force")

boot_lnLF <- boot_union(lnLF)
boot_lnLF

ts.plot(dlnLF, xlab = "Time", ylab = "LF")
title(main = "Labor Force", sub = "In the difference & logarthmic form")

boot_dlnLF <- boot_union(dlnLF)
boot_dlnLF

inflation <- read_csv("Datasets/Inflation.csv") # Level 2015 = 100 -> normalized
attach(inflation)
names(inflation)



CPI <- ts(inflation$Value, start = 1955, frequency = 1)

ts.plot(CPI, xlab = "Time", ylab = "CPI")
title(main = "Inflation")

lnCPI <- log(CPI)

ts.plot(lnCPI, xlab = "Time", ylab = "lnCPI")
title(main = "Inflation")

dlnCPI <- diff(lnCPI)

ts.plot(dlnCPI, xlab = "Time", ylab = "dlnCPI")
title(main = "Inflation")

boot_dlnCPI <- boot_union(dlnCPI)
boot_dlnCPI

ts.plot(XO_ts, xlab = "Time", ylab = "XO")
title(main = "Exports")

ts.plot(dlnXO, xlab = "Time", ylab = "XO")
title(main = "Exports", sub = "In the difference & logarithmic form")

boot_dlnXO

adf(dlnXO, deterministics = "trend")

adf(resid_dlnXO, deterministics = "intercept")

acf(dlnXO, main = "Correlogram of Exports", sub = "Variable: dlnXO")

acf(dlnYO, main = "Correlogram of Output", sub = "Variable: dlnYO")

# Create Lagged Variables

lags_dlnYO <- embed(dlnYO, dimension = 2)

dlnYO_0 <- lags_dlnYO[, 1]
dlnYO_1 <- lags_dlnYO[, 2]

lags_dlnXO <- embed(dlnXO, dimension = 2)

dlnXO_0 <- lags_dlnXO[, 1]
dlnXO_1 <- lags_dlnXO[, 2]

fit_ARDL <- lm(dlnYO_0~dlnXO_0 + dlnYO_1)
summary(fit_ARDL)

ts.plot(dlnPY, dlnCPI, col = c("black", "blue"))

boot_lnPY <- boot_union(lnPY)
boot_lnPY

boot_dlnPY <- boot_union(dlnPY)
boot_dlnPY

2.773e-02

ts.plot(dlnYO, dlnXO, col = c("black", "blue"), lty = c(1,1), xlab = "Time", ylab = "YO & XO")
title(main = "Output & Export", sub = "In the difference and logarithmic form")
legend("topleft", legend = c("YO", "XO"), col = c("black", "blue"), lty = c(1,1))


# Log Output

lags_lnYO <- embed(lnYO, dimension = 2)

lnYO_0 <- lags_lnYO[, 1]
lnYO_1 <- lags_lnYO[, 2]

# Log Exports

lags_lnXO <- embed(lnXO, dimension = 2)

lnXO_0 <- lags_lnXO[, 1]
lnXO_1 <- lags_lnXO[, 2]

# Diff-Log Output

lags_dlnYO <- embed(dlnYO, dimension = 2)

dlnYO_0 <- lags_dlnYO[, 1]
dlnYO_1 <- lags_dlnYO[, 2]

# Diff-Log Exports

lags_dlnXO <- embed(dlnXO, dimension = 2)

dlnXO_0 <- lags_dlnXO[, 1]
dlnXO_1 <- lags_dlnXO[, 2]

# Estimate Model (3)

model3 <- lm(dlnYO_0 ~ dlnXO_0)
summary(model3)


# Save Residuals Model (3)

resid3 <- model3$residuals

# Plot

plot(resid3, type = "l", ylab = "Resid", xlab = "Time")
title(main = "Residuals of Model (3)")

# Run Test

bptest(model3, varformula = ~ dlnXO_0 + I(dlnXO_0^2))

# Create Lagged Variables

# Output

lags_dlnYO2 <- embed(dlnYO, dimension = 3)

dlnYO2_0 <- lags_dlnYO2[, 1]
dlnYO2_1 <- lags_dlnYO2[, 2]
dlnYO2_2 <- lags_dlnYO2[, 3]

# Exports

lags_dlnXO2 <- embed(dlnXO, dimension = 3)

dlnXO2_0 <- lags_dlnXO2[, 1]
dlnXO2_1 <- lags_dlnXO2[, 2]
dlnXO2_2 <- lags_dlnXO2[, 3]

# Estimate model (5)

model5 <- lm(dlnYO2_0 ~ dlnXO2_0 + dlnYO2_1 + dlnYO2_2)
summary(model5)

# Save Residuals Model (5)

resid5 <- model5$residuals

# Plot 

plot(resid5, type = "l", xlab = "Time", ylab = "Resid")
title(main = "Residuals of Model (5)")

# Run Test

bptest(model5, varformula = ~ dlnXO2_0 + dlnYO2_1 + dlnYO2_2 + I(dlnXO2_0^2) + I(dlnYO2_1^2) + I(dlnYO2_2^2) + I(dlnXO2_0*dlnYO2_1) + I(dlnXO2_0*dlnYO2_2) + I(dlnYO2_1*dlnYO2_2))

# Lagged Variables

ut5 <- embed(resid5, dimension = 2)

ut5_0 <- ut5[, 1]
ut5_1 <- ut5[, 2]

# Plot (Time)

ts.plot(ut5_0, xlab = "Time", ylab = "Resid")
title(main = "Residuals of Model (5)")

# Plot (Scatter)

plot(x = ut5_0, y = ut5_1, xlab = "Residuals", ylab = "Lagged Residuals")
title(main = "Scatter of the Residuals of Model (5)", sub = "Residuals against their first lags")

n5 <- 62

k5 <- round(sqrt(n5))

q5 <- length(model5$coefficients)

df5 <- k5 - q5

# Run Test

Box.test(model5$residuals, type = "Ljung-Box", lag = k5, fitdf = df5)

# Plot

acf(ut5_0, main = "Correlogram of the Residuals")

# Run Test

bgtest(model5, order = 3)

# Run Test

resettest(model5)

# Run Test Output

adf(dlnYO, deterministics = "intercept", max_lag = 0)

# Run Test Export

adf(dlnXO, deterministics = "intercept", max_lag = 0)

# Dataset

VAR6_data <- data.frame(dlnYO, dlnXO)

# Select VAR order

VARselect(VAR6_data)

# Estimate Model (6)

model6 <- VAR(VAR6_data, p = 2)
summary(model6)

plot(x = lnYO, y = lnXO, xlab = "Output", ylab = "Export", type = "p")
title(main = "Export v. Output")


data1 <- lnYO

data2 <- lnXO

plot.default(x = dlnYO, y = dlnXO, xlab = "Output", ylab = "Export", type = "p")
title(main = "Export v. Output")

XOratio <- XO_ts / YO_ts
lnXOratio <- lnXO / lnYO

plot(XOratio)
plot(lnXOratio)

CO_ts <- ts(BEL_data$CO, start = 1950, frequency = 1)

IO_ts <- ts(BEL_data$IO, start = 1950, frequency = 1)

COratio <- CO_ts / YO_ts

ts.plot(CO_ts,YO_ts, XO_ts, IO_ts, col = c("blue", "black", "red", "green"), main = "Consumption", xlab = "Time")

EXR_ts <- ts(BEL_data$EXR, start = 1950, frequency = 1)

ts.plot(EXR_ts, col = c("darkgreen"), xlab = "Time", ylab = "EXR")
title(main = "Exchange rates of Belgium")

lnEXR <- log(EXR_ts)

ts.plot(lnEXR, col = c("black"), xlab = "Time", ylab = "EXR")
title(main = "Exchange rates of Belgium")

dlnEXR <- diff(lnEXR)



adf(dlnEXR, deterministics = "intercept", max_lag = 0)

boot_dlnEXR <- boot_union(dlnEXR)
boot_dlnEXR

acf(dlnEXR)

variables <- cbind(dlnYO, dlnXO, dlnEXR)

par(mfrow = c(2,2))

ts.plot(lnYO, lnXO, col = c("black", "blue"), xlab = "Time", ylab = "YO, and XO")
title(main = "Output & Export", sub = "In the logarithmic form")
legend("bottomright", legend = c("lnYO", "lnXO"), col = c("black", "blue"), lty = c(1,1))

ts.plot(lnEXR, col = c("darkgreen"), xlab = "Time", ylab = "EXR")
title(main = "Exchange rate of Belgium", sub = "In the logarithmic form")

par(mfrow = c(2,2))

a <- acf(dlnXO, main = "Correlogram of Export")
 
b <- acf(dlnYO, main = "Correlogram of Output")

c <- acf(dlnEXR, main = "Correlogram of Exchange rate")

par(mfrow = c(2,2))

ts.plot(dlnXO, col = c('blue'), xlab = "Time", ylab = "XO")
title(main = "Exports", sub = "In the difference and log form")

ts.plot(dlnYO, col = c('black'), xlab = "Time", ylab = "YO")
title(main = "Output", sub = "In the difference and log form")

ts.plot(dlnEXR, col = c('darkgreen'), xlab = "Time", ylab = "EXR")
title(main = "Exchange rate", sub = "In the difference and log form")

x <- 1:10

x1 <- embed(x, 1)
x2 <- embed(x, 2)
x3 <- embed(x, 3)

library(vars)

print(x)
print(x1)
print(x2)
print(x3)

# Dataset

VAR6_data <- data.frame(dlnYO, dlnXO)

# Select VAR order

VARselect(VAR6_data)

# Estimate Model (6)

model6 <- VAR(VAR6_data, p = 2)
summary(model6)

irf6 <- irf(model6, ortho = "F")

plot(irf6)


plot(EXR_ts)






















