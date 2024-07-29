# This is the R script for "Tutorial 1 - EBC2090.pdf"
# The dataset used in this exercise should be cited as "Feenstra, Robert C., Robert Inklaar and
# Marcel P. Timmer (2015), “The Next Generation of the Penn World Table” American Economic Review,
# 105(10), 3150-3182, available for download at www.ggdc.net/pwt."

# Ex1:

# Import the data
load("BEL_data.RData") # Working Directory -> "Empirical Econometrics"\Tutorials

# View the data
View(BEL_data)

# Return the names of each variable
names(BEL_data)

# Address the variables with their label
attach(BEL_data)

# Ex2a:

IO_ts <- ts(BEL_data$IO, start = 1950, frequency = 1)
YO_ts <- ts(BEL_data$YO, start = 1950, frequency = 1)

pdf(file = "TimePlot-log(IO)vslog(Yo).pdf", width = 6, height = 6)
ts.plot(log(IO_ts), log(YO_ts), ylab = "IO and YO", col = c("black","blue"), lty = c(1,1))
legend("topleft", legend = c("log(IO)", "log(YO)"), col = c("black", "blue"), lty = c(1,1))
dev.off()

# Ex2b: 

lnIO <- log(IO_ts)
lnYO <- log(YO_ts)

# Correlograms


pdf(file = "Correlogram$log(IO_ts).pdf", width = 6, height = 6)
acf(lnIO)
dev.off()

pdf(file = "Correlogram$log(YO_ts).pdf", width = 6, height = 6)
acf(lnYO)
dev.off()

# Ex2c:

dlnIO <- diff(log(IO_ts))
dlnYO <- diff(log(YO_ts))

pdf(file = "TimePlot-dlnIO_dlnYO.pdf", width = 6, height = 6)
ts.plot(dlnIO, dlnYO, ylab = "diff(log(IO)) vs diff(log(YO))",  col = c("black","blue"), lty = c(1,1))
legend("topleft", legend = c("diff(log(IO))", "diff(log(YO))"), col = c("black", "blue"), lty = c(1,1))
dev.off()

pdf(file = "Correlogram$diff(log(IO_ts)).pdf", width = 6, height = 6)
acf(dlnIO)
dev.off()

pdf(file = "Correlogram$diff(log(YO_ts)).pdf", width = 6, height = 6)
acf(dlnYO)
dev.off()

# Ex3a:

lags_lnIO <- embed(lnIO, dimension = 2)

#Ex3b:

View(lags_lnIO)

# The matrix has 64 (n - 1) rows.

# Ex3c:
# _n denotes the "n"th lag of the variable

lnIO_0 <- lags_lnIO[, 1]
lnIO_1 <- lags_lnIO[, 2]

# Ex3d: 

fit_lnIO0_lnIO1 <- lm(lnIO_0~lnIO_1)
summary(fit_lnIO0_lnIO1)

# Ex4a: 
  
lags_dlnIO <- embed(dlnIO, dimension = 2)
dlnIO_0 <- lags_dlnIO[, 1]
dlnIO_1 <- lags_dlnIO[, 2]

# Ex4b:

fit_dlnIO0_dlnIO1 <- lm(dlnIO_0~dlnIO_1)
summary(fit_dlnIO0_dlnIO1)

# Ex5b: Regression

fit_lnIO_lnYO <- lm(lnIO~lnYO)
summary(fit_lnIO_lnYO)

# Ex5b: Residuals

pdf(file = "LinePlot-fit_lnIO_lnYO$Residuals.pdf", width = 6, height = 6)
plot(fit_lnIO_lnYO$residuals, type = "l")
dev.off()

pdf(file = "Correlogram$residuals-fit_lnIO_lnYO.pdf", width = 6, height = 6)
acf(fit_lnIO_lnYO$residuals)
dev.off()

# Ex6b:

lags_lnYO <- embed(lnYO, dimension = 2)
lnYO_0 <- lags_lnYO[, 1]
lnYO_1 <- lags_lnYO[, 2]

# Ex6c:

# fit_FDL <- lm(lnIO~lnYO_0 + lnYO_1)
# summary(fit_FDL)

# EX6d:

lnIO_0 <- lnIO[-1]

fit_FDL <- lm(lnIO_0~lnYO_0 + lnYO_1)
summary(fit_FDL)

# Ex7a 

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

# Regression

fit_lnIO_ur <- lm(lnIO_0~lnYO_0 + lnPI_0 + lnPY_0 + lnPI_1 + lnYO_1)
summary(fit_lnIO_ur)

# Ex7c:

SSR_ur <- sum(fit_lnIO_ur$residuals^2)
SSR_r <- sum(fit_FDL$residuals^2)
q <- 5 - 2

F_test <- ((SSR_r - SSR_ur)/q)/(SSR_ur/(58))

qf(0.05, 3, 58, lower.tail=TRUE)

# Ex7d:

library(car)

linearHypothesis(fit_lnIO_ur, c("lnPI_0=0", "lnPY_0=0", "lnPI_1=0"), test="F")

# Ex7e:

PIR <- (PI/PY)
lnPIR <- log(PIR)

lags_lnPIR <- embed(lnPIR, dimension = 2)
lnPIR_0 <- lags_lnPIR[, 1]
lnPIR_1 <- lags_lnPIR[, 2]

# Regression

fit_lnIO_on_PIR <- lm(lnIO_0~lnYO_0 + lnPIR_0 + lnYO_1)
summary(fit_lnIO_on_PIR)

# EX7f:

linearHypothesis(fit_lnIO_on_PIR, c("lnYO_0=0", "lnPIR_0=0", "lnYO_1=0"), test="F")

# Ex7g:

dlnPI <- diff(lnPI)

# Regression

fit_lnIO_on_dlnPI <- lm(lnIO_0~lnYO_0 + lnPIR_0 + dlnPI + lnYO_1)
summary(fit_lnIO_on_PIR)

# Ex7h:

linearHypothesis(fit_lnIO_on_dlnPI, c("lnPIR_0=0", "dlnPI=0"), test="F")

# Ex7i:

lnIU <- log(IU)
lnYU <- log(YU)

# Regression

fit_lnIU_on_lnYU <- lm(lnIU~lnYU)
summary(fit_lnIU_on_lnYU)

# Ex7j:

linearHypothesis(fit_lnIO_ur, c("lnYO_0=0", "lnPY_0=0", "lnPI_1=0","lnYO_1=0" ), test="F")

# Ex8c:

fit_ARDL <- lm(lnIO_0~lnYO_0 + lnYO_1 + lnIO_1)
summary(fit_ARDL)

