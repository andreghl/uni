# This is the R script for "Tutorial 1 - EBC2090.pdf"
# The dataset used in this exercise should be cited as "Feenstra, Robert C., Robert Inklaar and
# Marcel P. Timmer (2015), “The Next Generation of the Penn World Table” American Economic Review,
# 105(10), 3150-3182, available for download at www.ggdc.net/pwt."

# Import the data
load("BEL_data.RData")

# View the data
View(BEL_data)

# Return the names of each variable
names(BEL_data)

# Address the variables with their label
attach(BEL_data)

# Exercise 3:

# Transform 'YU' into TS data
YU_ts <- ts(BEL_data$YU, start = 1950, frequency = 1)

ts.plot(YU_ts)

# Ex3b: There is an obvious upward trend.

# Import plot to pdf
pdf(file = "time-plot-YU.pdf", width = 6, height = 6)
ts.plot(YU_ts)
dev.off()


# Transform 'YO' var into TS data
YO_ts <- ts(BEL_data$YO, start = 1950, frequency = 1)

# Ex3d: Plot 2 variables in 1 graph and pdf

pdf(file = "time-plot-YU_versus_YO.pdf", width = 6, height = 6)
ts.plot(YU_ts, YO_ts, ylab = "YU versus YO", col = c("blue", "black"))
legend("topleft", legend = c("YU", "YO"), col = c("blue", "black"), lty = c(1,1))
dev.off()

# Ex3d: The TS crosses around the 2008 financial crisis
# The series have an upward (exponential) trend and they do not fluctuate around a constant mean

# Transform 'IU' and 'IO' into TS data

IU_ts <- ts(BEL_data$IU, start = 1950, frequency = 1)

IO_ts <- ts(BEL_data$IO, start = 1950, frequency = 1)

pdf(file = "time-plot-YU,IU_vs_YO,IO.pdf", width = 6, height =6)
ts.plot(YU_ts,YO_ts,IU_ts,IO_ts, ylab = "YU & IU versus YO & IO", col = c("blue", "red", "blue", "red"), lty = c(1,1,2,2))
legend("topleft", legend = c("YU", "YO", "IU", "IO"), col = c("blue", "red", "blue", "red"), lty = c(1,1,2,2))
dev.off()

# They also seem to cross around the financial crisis of 2008. For the Investments, there is a weaker upward trend

# Ex4: Scatter plots
pdf(file = "scatterplot-YO_versus_IO.pdf", width = 6, height = 6)
plot(x = YO, y = IO)
dev.off()

# Ex4a: A dot represents a year and shows its given values for x and y.
# Ex4b: There could be a positive linear relationship between the two variables.

# Ex5: Simple Regression

fit_IO_on_YO <- lm(IO~YO)
summary(fit_IO_on_YO)

# Ex5a: The estimate for the intercept is 3.861 * 10^(-3) and the estimate for the 'YO' is 0.2333
# Ex5b: This tells us that a 1 unit (1 euro) increase in (constant) output results in a 0.2333 unit (0.2333 euros) increase in (constant) investments.
# Ex5c: The variable 'YO' is significant at the 5% level. The t-stat is high (41.872) and the p-value is low (2 * 10^(-16)).
# The confidence interval would be [0.2223, 0.2442] -> [0.2333-1.96*(0.005571), 0.2333+1.96*(0.005571)].

# Ex5d: Compute 95% CI

confint(fit_IO_on_YO, parm = "YO", level = 0.95)

# The R output gives an interval of [0.2221323, 0.2443974]

# Ex5e: The R-squared is 0.9653 which is unusually high. 
# This tells us that 96% of the variation in IO can be explained by the variation in YO.

# Ex6: Residual Inspection

# See what info was saved from regression
names(fit_IO_on_YO)

# Plot residuals of regression
pdf(file = "fit_IO_on_YO$residuals.pdf", width = 6, height = 6)
plot(fit_IO_on_YO$residuals, type = "l")
dev.off()

plot(fit_IO_on_YO$residuals, type = )

# Ex6a: The residuals are not randomly scattered in the plot.

# Ex6b: plot residual against 'YO'
pdf(file = "YO_versus_fit_IO_on_YO$residuals.pdf", width = 6, height = 6)
plot(x = YO, y = fit_IO_on_YO$residuals)
dev.off()

# The homoskedasticity assumption is not satisfied

# Ex6c: plot histogram of residuals
pdf(file = "hist-fit_IO_on_YO$residuals.pdf")
hist(fit_IO_on_YO$residuals)
dev.off()

# The homoskedasticity assumption does not seem plausible

# Ex6d: Jarque-Bera test

library(tseries)
jarque.bera.test(fit_IO_on_YO$residuals)

# Output: Jarque Bera Test
# data:  fit_IO_on_YO$residuals
# X-squared = 17.047, df = 2, p-value = 0.0001987

# The p-value is low thus the null can be rejected. Maybe not normally distributed

# If the homoskedasticity assumption is not met than the t-statistic cannot be trusted.

# Ex7: Log-Log Specification

# Ex7a

lnIO_ts <- log(IO_ts)
lnYO_ts <- log(YO_ts)

# Ex7b

pdf(file = "time-plot-lnIO_ts vs lnYO_ts.pdf", width = 6, height = 6)
ts.plot(lnIO_ts, lnYO_ts, ylab = "lnIO_ts vs lnYO_ts", col = c("blue", "red"))
legend("topleft", legend = c("lnIO_ts", "lnYO_ts"), col = c("blue", "red"), lty = c(1,1))
dev.off()

# The growth is more linear (instead of exponential). 
# This shows the percentage change an reduce mistakes of misinterpreting a constant growth rate.


# Ex7c
pdf(file = "scatter-lnYO_ts_versus_lnIO_ts.pdf", width = 6, height = 6)
plot(x = log(YO), y = log(IO))
dev.off()


# Yes there seems to be a positive linear relation between the two variables.
# It can thus be estimated by a linear function

# Ex7d
# fit_IO_on_YO <- lm(IO~YO)
fit_lnIO_on_lnYO <- lm(lnIO_ts~lnYO_ts)
summary(fit_lnIO_on_lnYO)

# Ex7e
# The estimates are -2.30465 for the intercept and 1.06149 for 'lnYO_ts'

# Ex7f: 
# A 1% increase in Output results in a 1.06% increase in Investments

# Ex7g
# The t-stat of 'lnYO_ts' is very large thus significant.
# The p-value is extremely low thus significant.

confint(fit_lnIO_on_lnYO, parm = "lnYO_ts", level = 0.95)
# The 95% CI is [1.013782, 1.109202] 

# Ex7h
# The R-squared is 0.9691 which is again unusually large. 
# This tells us that 97% of the variation in lnIO_ts can be explained by the variation in lnYO_ts.

# Ex7i

pdf(file = "scatter-fit_lnIO_on_lnYO$residuals.pdf",width = 6, height = 6)
plot(fit_lnIO_on_lnYO$residuals)
dev.off()

# In the same way as before the homoskedasticity assumption is highly unlikely to hold.

pdf(file = "hist-fit_lnIO_on_lnYO$residuals.pdf", width = 6, height =6)
hist(fit_lnIO_on_lnYO$residuals)
dev.off()

# This can be confirmed by the uneven frequency of the residual histogram

# Ex8: Log-Difference

# Ex8a
dlnIO_ts <- diff(log(IO_ts))
dlnYO_ts <- diff(log(YO_ts))

# Ex8b
# There are 64 obs instead of 65 bc we cannot have the diff for the first obs [t=1] bc we do not have any info for [t=0].

# Ex8c
pdf(file = "time-plot-dlnIO_ts_versus_dlnYO_ts.pdf", width = 6, height = 6)
ts.plot(dlnIO_ts, dlnYO_ts, ylab = "dlnIO_ts_versus_dlnYO_ts", col = c("blue", "red"))
dev.off()
# The lines seems to fluctuate more (at least for 'dlnIO_ts')


# Ex8d
pdf(file = "scatter-dlnIO_ts_versus_dlnYO_ts.pdf", width = 6, height = 6)
plot(x = diff(log(YO)), y = diff(log(IO)))
dev.off()


# The scatter plot looks quite messy thus I don't know if it can be estimated by a linear function.

# Ex8e
fit_dlnIO_on_dlnYO <- lm(dlnIO_ts~dlnYO_ts)
summary(fit_dlnIO_on_dlnYO)

# Ex8f
# The estimate for the intercept is -0.04765  and the estimate for 'dlnYO_ts' is 2.86299.

# Ex8g
# The coefficient of dlnYO_ts tells us that a 1% growth in Output results in a 2.86% growth in Investments.

# Ex8h
# The t-stat is high -> significant
# The p-value is low -> significant

confint(fit_dlnIO_on_dlnYO, parm = "dlnYO_ts", level = 0.95)
# The 95% CI is [2.169167,3.556819]

# Ex8i
# The R-squared is 0.5232 which is much lower than the previous regression
# -> But cannot be used to distinguish between different models -> See Woolridge p198

# Ex8j
pdf(file = "scatter-fit_dlnIO_on_dlnYO$residuals.pdf", width = 6, height = 6)
plot(fit_dlnIO_on_dlnYO$residuals)
dev.off()

# The homoskedasticity assumption seems more likely to hold here than in the previous cases.

pdf(file = "hist-fit_dlnIO_on_dlnYO$residuals.pdf", width = 6, height = 6)
hist(fit_dlnIO_on_dlnYO$residuals)
dev.off()

# The frequency is still uneven but less than in previous cases.