# Import the data
load("BEL_data.RData") # Working Directory -> "Empirical Econometrics"\Tutorials

# View the data
View(BEL_data)

# Return the names of each variable
names(BEL_data)

# Address the variables with their label
attach(BEL_data)

# Transforming into time series

IO_ts <- ts(BEL_data$IO, start = 1950, frequency = 1)
YO_ts <- ts(BEL_data$YO, start = 1950, frequency = 1)

# Transforming into log(var)

lnIO <- log(IO_ts)
lnYO <- log(YO_ts)

# Transforming log(var) int diff(lnVar)

dlnIO <- diff(log(IO))
dlnYO <- diff(log(YO))

# Create lagged matrices

lags_lnIO <- embed(lnIO, dimension = 2)
lags_lnYO <- embed(lnYO, dimension = 2)

# Create lagged variables for 'IO' and 'YO'

lnIO_0 <- lags_lnIO[, 1]
lnIO_1 <- lags_lnIO[, 2]

lnYO_0 <- lags_lnYO[, 1]
lnYO_1 <- lags_lnYO[, 2]


# Exercise 7: Recap Multiple Hypothesis Testing

# Defining new variables

PY <- (YU/YO)
PI <- (IU/IO)

# Log of new variables

lnPY <- log(PY)
lnPI <- log(PI)

# Lag of new variables

lags_lnPI <- embed(lnPI, dimension = 2)
lnPI_0 <- lags_lnPI[, 1]
lnPI_1 <- lags_lnPI[, 2]

lags_lnPY <- embed(lnPY, dimension = 2)
lnPY_0 <- lags_lnPY[, 1]
lnPY_1 <- lags_lnPY[, 2]

# Ex7a:

fit_model8 <- lm(lnIO_0~lnYO_0 + lnPI_0 + lnPY_0 + lnPI_1 + lnYO_1)
summary(fit_model8)

# Ex7c:

fit_model7 <- lm(lnIO_0~lnYO_0 + lnYO_1)
summary(fit_model7)

SSR_ur <- sum(fit_model8$residuals^2)
SSR_r <- sum(fit_model7$residuals^2)

q <- 3
n <- 64
k <- 5

F_stat <- ((SSR_r - SSR_ur)/q)/(SSR_ur/(n-k-1))
print(F_stat)



