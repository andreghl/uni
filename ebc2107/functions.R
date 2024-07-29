setwd("~/Maastricht University/2 - Mathematical Statistics")
rm(list = ls())

# Functions for the Project

#### Simple Regression ----

regress <- function(y, x) {
  
  dataset <- data.frame(y, x)
  
  # Sample Size
  n <- nrow(dataset)
  
  # Find mean of data
  
  meanX <- mean(dataset[,2])
  meanY <- mean(dataset[,1])
  
  # Calculating Beta 
  
  numerator <- sum((x - meanX) * (y - meanY))
  denominator <- sum((x - meanX) ^ 2) 
  
  # Calculating Estimators 
  
  slope <- numerator / denominator
  intercept <- meanY - slope * meanX 
  
  return(list("intercept" = intercept, "slope" = slope))
  
}

#### Calculating the ten year averages ----

tenyear <- function(data) {
  
  # Define Time variable
  
  n <- length(data)
  
  averages <- rep(0, n)
  
  for(i in 1:n) {
    
    # Defining sub-sample size
    
    subset <- data[i:(i+9)]
    
    averages[i] <- mean(subset)
  }
  
  return(averages)
  
}

#### Confidence Interval for a mean ----

CI <- function(data, alpha = 0.05){
  
  mean <- mean(data)
  p <- 1 - alpha / 2
  n <- length(data) 
  level <- 1 - alpha
  
  # Bounds
  
  Lower <- mean - qt(p, n - 1, lower.tail = FALSE) * sd(data) / sqrt(n)
  Upper <- mean + qt(p, n - 1, lower.tail = FALSE) * sd(data) / sqrt(n)
  
  cat("The", level, "% confidence interval is: [",Lower, Upper,"] \n")
  
}

#### T Statistic "T(X)" of a variable ----

T.stat <- function(data, mu = 0, n = length(data)){
  
  X <- mean(data) # Sample mean
  S <- sd(data) # Sample Std Dev
  
  T <- sqrt(n) * (X - mu) / S
  
  cat("The test statistic has a value of: \n")
  return(T)
}

#### Bootstrap Plot ----

          # Simply copy-paste most of the code already written by Smeekes and put it in a function

Bootstrap <- function(data, B = NULL, T = NULL){ # Used the code from "BootstrapLecture.R"
  
  Q <- rep(NA, B) # Empty vector containing Bootstrap T-Stats
  n <- length(data)
  mu <- mean(data)
  var <- deparse(substitute(data))
  title <- paste("Bootstrap Distribution of", var, "\n")
  
  for (b in 1:B) {										
    Sys.sleep(.01)
    J <- sample.int(n, size = n, replace = TRUE)			
    X.star <- data[J]											
    X <- mean(X.star) 							
    S <- sd(X.star)								
    Q[b] <- sqrt(n) * (X - mu) / S	
    
    left.int <- seq(-4, 3.5, 0.5)
    right.int <- seq(-3.5, 4, 0.5)
    mid.int <- (left.int + right.int) / 2
    freq <- colSums(outer(Q[1:b], right.int, "<=") - outer(Q[1:b], left.int, "<="))/ B
    
    if (b %% 10 == 0){
      plot(mid.int, freq, type = "h", col = "blue", lend = "butt", lwd = 34, xlab = "", ylab = "Frequency", ylim = c(0, 0.25))
      title(title)
      lines(T * c(1, 1), c(0, 0.25), lwd = 3, col = "red")
      mtext("T(X)", side = 3, line = 0, at = T, col = "red")
      points(Q[(b - 9):b], rep(0.24, 10), col = "blue", pch = 20)
      Sys.sleep(0)
    }
  }
}
