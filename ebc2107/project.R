
setwd("~/Maastricht University/2 - Mathematical Statistics")
rm(list = ls())

# Load data

data <- read.csv("AnnualTEmp.csv",
                 header = TRUE,
                 sep = ";",
                 dec = ",")

attach(data)

# Import libraries

library("tseries")


Time <- ts(data[,1], start = 1907, frequency = 1)
DeBilt <- ts(data[,2], start = 1907, frequency = 1)
Eelde <- ts(data[,3], start = 1907, frequency = 1)
Maastricht <- ts(data[,4], start = 1907, frequency = 1)

# Average Temp of 3 cities

Netherlands <- rep(0, length(Time))

for (i in 1:117){
  
  Average <- sum(DeBilt[i] + Eelde[i] + Maastricht[i]) / 3
  
  Netherlands[i] <- Average
  
}

Netherlands <- ts(Netherlands, start = 1907, frequency = 1)


# Plot

ts.plot(DeBilt, Eelde, Maastricht, col = c("blue", "red", "grey"))
title(main = "Annual Dutch Temperatures")
legend("topleft", legend = c("DeBilt", "Eelde", "Maastricht"), col = c("blue", "red", "grey"), lty = c(1,1,1))

# Plot with Netherlands

ts.plot(DeBilt, Eelde, Maastricht, Netherlands, col = c("blue", "red", "grey", "black"), lty = c(3,3,3,1))
title(main = "Annual Dutch Temperatures")
legend("topleft", legend = c("DeBilt", "Eelde", "Maastricht", "Netherlands"), col = c("blue", "red", "grey", "black"), lty = c(3,3,3,1))


# Calculate the ten year averages 

tenyear <- function(data) {
  
  # Define Time variable
  
  n <- length(data) - 9
  
  averages <- rep(0, n)
  
  for(i in 1:n) {
    
    # Defining sub-sample size
    
    subset <- data[i:(i+9)]
    
    averages[i] <- mean(subset)
  }
  
  return(averages)
  
}


# Ten Year Average for DeBilt

DeBilt10 <- tenyear(DeBilt)
DeBilt10 <- ts(DeBilt10, start = 1907, frequency = 1)


# Ten Year Average for Eelde

Eelde10 <- tenyear(Eelde)
Eelde10 <- ts(Eelde10, start = 1907, frequency = 1)

# Ten Year Average for Maastricht

Maastricht10 <- tenyear(Maastricht)
Maastricht10 <- ts(Maastricht10, start = 1907, frequency = 1)


# Ten Year Average for Netherlands

Netherlands10 <- tenyear(Netherlands)
Netherlands10 <- ts(Netherlands10, start = 1907, frequency = 1)

# Plot

ts.plot(DeBilt10, Eelde10, Maastricht10, Netherlands10, col = c("blue", "red", "grey", "black"), lty = c(3,3,3,1))
title(main = "Ten year averages")
legend("topleft", legend = c("DeBilt", "Eelde", "Maastricht", "Netherlands"), col = c("blue", "red", "grey", "black"), lty = c(3,3,3,1))


# Mean

Netherlands.mean <- mean(Netherlands)
Netherlands.mean

# Frequency Distributions

# Netherlands

Netherlands.Hist <- hist(Netherlands)


# Confidence Interval for a mean

CI <- function(data, n = length(data), alpha = 0.05){
  
  mean <- mean(data)
  p <- 1 - alpha / 2
  level <- 1 - alpha
  
  # Bounds
  
  Lower <- mean - qt(p, n - 1, lower.tail = TRUE) * (sd(data) / sqrt(n)) # qt() outputs 1.982383 
  Upper <- mean + qt(p, n - 1, lower.tail = TRUE) * (sd(data) / sqrt(n)) # qt() outputs 1.982383 
  
  cat("The", level, "% confidence interval is: [",Lower, ";", Upper,"] \n")
  
}

lowerCI <- function(data, n = length(data), alpha = 0.05){
  
  mean <- mean(data)
  p <- 1 - alpha
  
  # Bounds
  Lower <- mean - qt(p, n - 1, lower.tail = TRUE) * (sd(data) / sqrt(n))
  cat("The", level, "% one-sided confidence interval is: [",Lower, "; +++ ] \n")
}


# Calculate Confidence Interval

n <- 108

for (i in 1:108){
  
  Lower <<- Netherlands10 - qt(0.975, n - 1, lower.tail = TRUE) * (sd(Netherlands10[(1:i)]) / sqrt(n)) # qt() outputs 1.982383 
  
  Upper <<- Netherlands10 + qt(0.975, n - 1, lower.tail = TRUE) * (sd(Netherlands10[(1:i)]) / sqrt(n)) # qt() outputs 1.982383 
  
}

ts.plot(Lower, Upper, Netherlands10, col = c("black", "black", "red"))
title(main = "Confidence Intervals")


# Simple Regression 

regress <- function(y, x, truebeta = 0, alpha = 0.05) {
  
  dataset <- data.frame(y, x)
  
  # Sample Size
  n <- nrow(dataset)
  
  # Find mean of data
  
  meanX <- mean(dataset[,2])
  meanY <- mean(dataset[,1])
  
  # Calculating Beta 
  
  Covariance <- sum((x - meanX) * (y - meanY))
  Variance <- sum((x - meanX) ^ 2) 
  
  # Calculating Estimators 
  
  slope <- Covariance / Variance
  intercept <- meanY - slope * meanX 
  
  # Calculate error term
  
  error <- rep(0, n)
  
  for (i in 1:n){
    
    error[i] <- dataset[i, 1] - intercept -  slope * dataset[i, 2]
  }
  
  # SE of Slope
  
  var.slope <- sum(error^2) / ((n - 2) * Variance)
  
  se.slope <- sqrt(var.slope)
  
  # T(X) of Slope
  
  T.slope <- (slope - truebeta) / se.slope
  
  # P-value of T(X)
  
  p.slope <- pt(T.slope, df = n - 1, lower.tail = FALSE)
  
  # Confidence Interval for slope
  
  p <- alpha / 2
  level <- 1 - alpha
  
  # Bounds
  
  Lower <- slope - qt(p, n - 1, lower.tail = FALSE) * se.slope  
  Upper <- slope + qt(p, n - 1, lower.tail = FALSE) * se.slope 
  
  LowerCI <- slope - qt(alpha, n - 1, lower.tail = FALSE) * se.slope
  
  # Print Output to console
  
  cat("Regression: \n")
  cat("Method: Ordinary Least Squares \n")
  cat("___________________________________ \n")
  cat("          ", "Estimate   ", "Std Error", "\n")
  cat("Intercept:", intercept, "\n")
  cat("    Slope:", slope,      se.slope, "\n")
  cat("\n")
  cat("Hypothesis Testing: \n")
  cat("----------------------------------- \n")
  cat("Null: the slope is less or equal to", truebeta, "\n")
  cat("Alt: the slope is greater than", truebeta, "\n")
  cat("\n")
  cat("          ", "T value",    "P-value", "\n")
  cat("    Slope:",  T.slope,      p.slope,  "\n")
  cat("\n")
  cat("Confidence Interval (One-Sided): \n")
  cat("----------------------------------- \n")
  cat("The", level, "% one-sided CI for the slope is:", "\n",  "[",LowerCI, "; +++ ) \n")
  cat("Confidence Interval (Two-Sided): \n")
  cat("----------------------------------- \n")
  cat("The", level, "% two-sided CI for the slope is:", "\n",  "[",Lower, ";", Upper,"] \n")
  
}


n <- length(Netherlands)
t <- 1:n

# Estimate linear trend in the Netherlands

regress(Netherlands, t)
a <- lm(Netherlands ~ t)
summary(a)
confint(a)


plot(Netherlands[1:117])
abline(a = 8.318279, b = 0.01874516, col = "red")


# T test

T.test <- function(data, mu = 0, n = length(data), S = sd(data)){
  
  X <- mean(data) # Sample mean
  df <- n - 1
  
  T <- sqrt(n) * (X - mu) / S
  
  p <- pt(T, n - 1, lower.tail = FALSE)
  
  cat("t = ", T, "df = ", df, "\n")
  cat("One-Sided Hypothesis", "\n")
  cat("alternative hypothesis: true mean is greater than", mu,"\n")
  cat("The test statistic has a p-value of:", p, "\n")
  cat("Two-Sided Hypothesis", "\n")
  cat("alternative hypothesis: true mean is not equal to", mu,"\n")
  cat("The test statistic has a p-value of:", 2 * p, "\n")
  
}

T2.test <- function(data.1, data.2){
  
  X1 <- mean(data.1)
  X2 <- mean(data.2)
  
  n.1 = length(data.1) 
  n.2 = length(data.2) 
  
  S2.1 = var(data.1) 
  S2.2 = var(data.2)
  
  S2.p <- (((n.1 - 1) * S2.1) + ((n.2 - 1) * S2.2)) / (n.1 + n.2 - 2)
  S.p <- sqrt(S2.p)
  
  a <- (1 / n.1) + (1 / n.2)
  
  df <- n.1 + n.2 - 2
  
  T <- (X1 - X2) / (S.p * sqrt(a))
  p <- pt(T, df, lower.tail = FALSE)
  
  cat("t = ", T, "df = ", df, "\n")
  cat("The test statistic has a p-value of: \n")
  return(p)
  
}


# Time[68] # This should be the year 1974

# Create variables

T1974 <- Time[68:117]
Netherlands1974 <- Netherlands[68:117]

T1974 <- ts(T1974, start = 1974, frequency = 1)
Netherlands1974 <- ts(Netherlands1974, start = 1974, frequency = 1)

# Plot

ts.plot(Netherlands1974, type = "l")
title(main = "Annual Temperatures from 1974 onwards")
legend("topleft", legend = c("Netherlands"), lty = c(1))


n <- length(Netherlands1974)
t <- 1:n

# Estimate linear trend in the Netherlands

regress(Netherlands1974, t)

plot(Netherlands[68:117])
abline(a = 9.00651, b = 0.04191862, col = "red")


# Plot

plot(Netherlands[1:117], ylab = "Temperatures")
abline(a = 6.156, b = 0.04191862, col = "red")
abline(a = 8.318279, b = 0.01874516, col = "orange", lty = 2)
legend("topleft", legend = c("Overall Trend", "Trend after 1974"), col = c("orange", "red"), lty = c(2, 1))


# Create the first sub-sample

Netherlands1907 <- Netherlands[1:67]

# Two Sample t-test with equal variance assumption

T2.test(Netherlands1974, Netherlands1907)

# without equal variance assumption

t.test(Netherlands1974, Netherlands1907, var.equal = FALSE)

# Test whether mean in NL1974 equal to sample mean

T.test(Netherlands1974, mu = 9.424243)

# Function to calculate the T statistic T(X)

T.stat <- function(data, mu = 0, n = length(data)){
  
  X <- mean(data) # Sample mean
  S <- sd(data) # Sample Std Dev
  
  T <- sqrt(n) * (X - mu) / S
  
  cat("The test statistic has a value of:", T, "\n")
  return(T)
}

# Function for the Bootstrap

Bootstrap <- function(data, B = NULL, T.X = NA){ # Used the code from "BootstrapLecture.R"
  
  Q <<- rep(NA, B) # Empty vector containing Bootstrap T-Stats
  n <- length(data)
  mu <- mean(data)
  
  for (b in 1:B) {										
    Sys.sleep(.01)
    J <- sample.int(n, size = n, replace = TRUE)			
    X.star <- data[J]											
    X <- mean(X.star) 							
    S <- sd(X.star)								
    Q[b] <<- sqrt(n) * (X - mu) / S	
  }
  
  if (!is.na(T.X)){
    hist(Q)
    abline(v = T.X, col = "red", lwd = 3)
    mtext("T(X)", side = 3, line = 0, at = T.X, col = "red")
  } else {
    hist(Q)
  }
}

set.seed(2024)

# Test statistic

T <- T.stat(Netherlands, mu = 9.424243) # This mu is the mean of our sample

# Bootstrap

Bootstrap(Netherlands, B = 1000)


# Critical Value

BootC <- function(data, alpha = 0.05){
  
  c <- quantile(data, probs = 1 - alpha)	
  print(paste("The bootstrap critical value is", round(c, 3)))
  
}

C <- BootC(Q, alpha = 0.05)

# P-value

BootP <- function(BootQ, SampleT){
  p.value <- mean(BootQ > SampleT)						
  print(paste("The bootstrap p-value is", round(p.value, 3)))
}

Bootstrap(Netherlands, B = 1000, T.X = T)
BootP(Q, C)

# Means

mean(Netherlands1907) # = 8.938279
mean(Netherlands1974) # = 10.07544


monthly <- read.csv("SmoothedMonthlyTemp.csv",
                    header = TRUE,
                    sep = ";",
                    dec = ",")

attach(monthly)

# Define Variables

mTime <- ts(monthly[,1], start = 190701, frequency = 1)
mDeBilt <- ts(monthly[,2], start = 190701, frequency = 1)
mEelde <- ts(monthly[,3], start = 190701, frequency = 1)
mMaastricht <- ts(monthly[,4], start = 190701, frequency = 1)

# Plot

ts.plot(mDeBilt, mEelde, mMaastricht, col = c("blue", "red", "grey"))
title(main = "Monthly Dutch Temperatures")
legend("topleft", legend = c("DeBilt", "Eelde", "Maastricht"), col = c("blue", "red", "grey"), lty = c(1,1,1))


# Average Temp of 3 cities

mNetherlands <- rep(0, length(mTime))

for (i in 1:1392){
  
  mAverage <- (mDeBilt[i] + mEelde[i] + mMaastricht[i]) / 3
  
  mNetherlands[i] <- mAverage
  
}

mNetherlands <- ts(mNetherlands, start = 190701, frequency = 1)

# Plot

ts.plot(mNetherlands, type = "l")
title(main = "Average Monthly Temperatures in the Netherlands")
legend("topleft", legend = c("Netherlands"), lty = 1)

# Regression

n <- length(mNetherlands)

t <- 1:n

regress(mNetherlands, t)

# Plot

plot(mNetherlands[1:1392], ylab = "Temperatures")
abline(a = 8.31391, b = 0.00153694, col = "red", lwd = 2)

# Histogram

hist(mNetherlands)
title(sub = "The red line is the mean of the Annual data")
abline(v = mean(Netherlands), col = "red", lwd = 3)
mtext("Mean", side = 3, line = 0, at = mean(Netherlands), col = "red")

12 * 0.00153694 # = 0.01844328

n <- length(Netherlands)

t <- 1:n

regress(Netherlands, t, truebeta = 0.01844328)
