using MarketData, Statistics, Plots, DataFrames, LinearAlgebra, TimeSeries, HypothesisTests


start = DateTime(2014, 5, 15)  #DateTime(YYYY, M, DD)

# Getting the data for each stock

# Microsoft
MSFT = yahoo(:MSFT, YahooOpt(period1=start))
PtMSFT = MSFT["Close"]
plot(PtMSFT, xlabel="Time", ylabel="Price",
    label="Closing price")
title!("Price of Microsoft")

# log returns
RtMSFT = diff(log.(PtMSFT))
plot(RtMSFT, label="Log returns",
    xlabel="Time", ylabel="Log returns")
title!("Log returns of Microsoft")

histogram(RtMSFT, xlabel="Returns", ylabel="Frequency",
    title="Log returns of Microsoft")

# Google
GOOG = yahoo(:GOOG, YahooOpt(period1=start))
PtGOOG = GOOG["Close"]
plot(PtGOOG, xlabel="Time", ylabel="Price",
    label="Closing price")
title!("Price of Google")

# log returns
RtGOOG = diff(log.(PtGOOG))
plot(RtGOOG, label="Log returns",
    xlabel="Time", ylabel="Log returns")
title!("Log returns of Google")

histogram(RtGOOG, xlabel="Returns", ylabel="Frequency",
    title="Log returns of Google")

# Apple
AAPL = yahoo(:AAPL, YahooOpt(period1=start))
PtAAPL = AAPL["Close"]
plot(PtAAPL, xlabel="Time", ylabel="Price",
    label="Closing price")
title!("Price of Apple")

# log returns
RtAAPL = diff(log.(PtAAPL))
plot(RtAAPL, label="Log returns",
    xlabel="Time", ylabel="Log returns")
title!("Log returns of Apple")

histogram(RtAAPL, xlabel="Returns", ylabel="Frequency",
    title="Log returns of AAPL")

# IBM
IBM = yahoo(:IBM, YahooOpt(period1=start))
PtIBM = IBM["Close"]
plot(PtIBM, xlabel="Time", ylabel="Price",
    label="Closing price")
title!("Price of IBM")

# log returns
RtIBM = diff(log.(PtIBM))
plot(RtIBM, label="Log returns",
    xlabel="Time", ylabel="Log returns")
title!("Log returns of IBM")

histogram(RtIBM, xlabel="Returns", ylabel="Frequency",
    title="Log returns of IBM")

# Oracle
ORCL = yahoo(:ORCL, YahooOpt(period1=start))
PtORCL = ORCL["Close"]
plot(PtORCL, xlabel="Time", ylabel="Price",
    label="Closing price")
title!("Price of Oracle")

# log returns
RtORCL = diff(log.(PtORCL))
plot(RtORCL, label="Log returns",
    xlabel="Time", ylabel="Log returns")
title!("Log returns of Oracle")

histogram(RtORCL, xlabel="Returns", ylabel="Frequency",
    title="Log returns of Oracle")

# Market Index: S&P 500
SPX = yahoo("^GSPC", YahooOpt(period1=start))
PtSPX = SPX["Close"]
plot(PtSPX, xlabel="Time", ylabel="Price",
    label="Closing price")
title!("Price of S&P 500")

# log returns
RtSPX = diff(log.(PtSPX))
plot(RtSPX, label="Log returns",
    xlabel="Time", ylabel="Log returns")
title!("Log returns of S&P 500")


# Routine that reports the mean
function Routine(data)

    # Compute the mean
    v = values(data)
    n = length(v)
    i = ones(n)
    mean = inv(i'i)i'v

    # Compute the median
    sorted = sort(v)

    if mod(n, 2) != 0
        median = sorted[Int((n + 1) / 2)]
    else
        median = (sorted[Int(n / 2)] + sorted[Int((n / 2) + 1)]) / 2
    end

    # Compute the variance
    variance = (1 / n) * sum((x - mean)^2 for x in v)

    # Compute the skewness
    sd = sqrt(variance)
    skewness = ((1 / n) * sum((x - mean)^3 for x in v)) / sd^3

    # Compute kurtosis
    kurtosis = ((1 / n) * sum((x - mean)^4 for x in v)) / sd^4
    EK = kurtosis - 3
    JB = (n / 6) * (skewness^2 + 0.25 * (EK^2))

    variables = ["Mean"; "Median"; "Variance";
        "Skewness"; "Kurtosis"; "Jarque-Bera"]
    computed = [mean; median; variance;
        skewness; kurtosis; JB]

    output = DataFrame(Variables=variables, Output=computed)

    println(output)
    return (mean=mean, median=median, variance=variance,
        skewness=skewness, kurtosis=kurtosis, JarqueBera=JB)
end


# Data routines for each stock
Routine(RtMSFT)
Routine(RtGOOG)
Routine(RtAAPL)
Routine(RtIBM)
Routine(RtORCL)
Routine(RtSPX)

wPtAAPL = 0.2 * values(PtAAPL)
wPtGOOG = 0.2 * values(PtGOOG)
wPtIBM = 0.2 * values(PtIBM)
wPtMSFT = 0.2 * values(PtMSFT)
wPtORCL = 0.2 * values(PtORCL)

# Portfolio building
Index = wPtAAPL .+ wPtGOOG .+ wPtIBM .+ wPtMSFT .+ wPtORCL
RtIndex = diff(log.(Index))

# compare Index to S&P returns
plot([RtIndex, values(RtSPX)], xlabel="Time", ylabel="returns",
    title="Portfolio against S&P 500", labels=["Index" "S&P 500"])

Routine(RtIndex)

# Create a DataFrame of info
assets = ["Microsoft", "Google", "Apple", "IBM", "Oracle", "SP500", "Index"];
Stocks = [Routine(RtMSFT), Routine(RtGOOG), Routine(RtAAPL), Routine(RtIBM), Routine(RtORCL), Routine(RtSPX), Routine(RtIndex)]

Analysis = DataFrame(
    Asset=assets,
    Mean=[x.mean for x in Stocks],
    Median=[x.median for x in Stocks],
    Variance=[x.variance for x in Stocks],
    Skewness=[x.skewness for x in Stocks],
    Kurtosis=[x.kurtosis for x in Stocks],
    JarqueBera=[x.JarqueBera for x in Stocks]
)

print(Analysis)

# Autocorrelation
function regress(y, X)

    y = values(y)
    X = values(X)

    n = size(X, 1) # Sample size
    K = size(X, 2) # Number of regressors

    beta = inv(X'X)X'y # Estimated coefficients
    yHat = X * beta # Estimates for dependent
    e = y - (X * beta) # Residuals

    SSR = e'e
    s2 = SSR / (n - K)
    s = sqrt(s2)

    i = ones(n)
    yMean = inv(i'i)i'y
    T = (y .- yMean)
    TSS = T'T

    RSq = 1 - (SSR / TSS)
    Adj = 1 - (SSR / (n - K)) / (TSS / (n - 1))

    betaSE = sqrt.(diag(inv(X'X) * s2))

    return (beta=beta, n, K, yHat, e, SSR, s2, s, yMean, TSS, RSq, Adj, betaSE)
end

alpha = ones(length(RtIBM))

function OLS(y, X)

    y = values(y)
    X = values(X)

    n = size(X, 1) # Sample size
    K = size(X, 2) # Number of regressors

    beta = inv(X'X)X'y # Estimated coefficients
    yHat = X * beta # Estimates for dependent
    e = y - (X * beta) # Residuals

    SSR = e'e
    s2 = SSR / (n - K)
    s = sqrt(s2)

    i = ones(n)
    yMean = inv(i'i)i'y
    T = (y .- yMean)
    TSS = T'T

    RSq = 1 - (SSR / TSS)
    Adj = 1 - (SSR / (n - K)) / (TSS / (n - 1))

    AIC = log(s2) + (2K) / n
    SC = log(s2) + (K * log(n)) / n

    return (beta=beta, n, K, yHat, e, SSR, s2, s, yMean, TSS, RSq, Adj, AIC, SC)
end

function AutoCorrelation(y)

    v = values(y)
    n = length(v)
    X = ones(n)
    alpha = OLS(v, X).beta

    resid = OLS(v, X).e[1:n-1, :]
    nResid = length(resid)
    e = OLS(v, X).e[2:n]
    XResid = [ones(n - 1) resid]

    rho = regress(e, XResid).beta
    rhoSE = regress(e, XResid).betaSE

    test = rho[2] / rhoSE[2]

    return (alpha=alpha, rho=rho, test=test)
end

AutoCorrelation(RtAAPL).test
AutoCorrelation(RtGOOG).test
AutoCorrelation(RtIBM).test
AutoCorrelation(RtMSFT).test
AutoCorrelation(RtORCL).test
AutoCorrelation(RtSPX).test
AutoCorrelation(RtIndex).test

# The absolute value of the tests is greater than 1.96 for all Stock
# Thus we can reject the null hypothesis of no Autocorrelation

# Apple
OLS(RtAAPL, ones(2518)).AIC
OLS(RtAAPL[2:end], ones(2517)).AIC
OLS(RtAAPL, ones(2518)).SC
OLS(RtAAPL[2:end], ones(2517)).SC

# Google
OLS(RtGOOG, ones(2518)).AIC
OLS(RtGOOG[2:end], ones(2517)).AIC
OLS(RtGOOG, ones(2518)).SC
OLS(RtGOOG[2:end], ones(2517)).SC

# IBM
OLS(RtIBM, ones(2518)).AIC
OLS(RtIBM[2:end], ones(2517)).AIC
OLS(RtIBM, ones(2518)).SC
OLS(RtIBM[2:end], ones(2517)).SC

# Microsoft
OLS(RtMSFT, ones(2518)).AIC
OLS(RtMSFT[2:end], ones(2517)).AIC
OLS(RtMSFT, ones(2518)).SC
OLS(RtMSFT[2:end], ones(2517)).SC

# Oracle
OLS(RtORCL, ones(2518)).AIC
OLS(RtORCL[2:end], ones(2517)).AIC
OLS(RtORCL, ones(2518)).SC
OLS(RtORCL[2:end], ones(2517)).SC

# S&P 500
OLS(RtSPX, ones(2518)).AIC
OLS(RtSPX[2:end], ones(2517)).AIC
OLS(RtSPX, ones(2518)).SC
OLS(RtSPX[2:end], ones(2517)).SC

# Index
OLS(RtIndex, ones(2518)).AIC
OLS(RtIndex[2:end], ones(2517)).AIC
OLS(RtIndex, ones(2518)).SC
OLS(RtIndex[2:end], ones(2517)).SC

# Heteroskedasticity

function Heteroskedasticity(y)

    v = values(y)
    n = length(v)
    X = ones(n)
    alpha = OLS(v, X).beta

    resid = OLS(v, X).e[1:n-1, :]
    resid2 = resid .^ 2
    nResid = length(resid)
    e = OLS(v, X).e[2:n]
    e2 = e .^ 2
    XResid = [ones(n - 1) resid2]

    phi = regress(e2, XResid).beta
    phiSE = regress(e2, XResid).betaSE

    test = phi[2] / phiSE[2]

    return (phi=phi, test=test)
end

Heteroskedasticity(RtAAPL).test
Heteroskedasticity(RtGOOG).test
Heteroskedasticity(RtIBM).test
Heteroskedasticity(RtMSFT).test
Heteroskedasticity(RtORCL).test
Heteroskedasticity(RtSPX).test
Heteroskedasticity(RtIndex).test

# The test statistics are larger than 1.96 
# and thus, we reject the null for all stocks

# Robust Standard Errors

function Robust(y)

    v = values(y)
    n = length(y)
    X = ones(n)
    alpha = OLS(v, X).beta

    eAlpha = diagm(OLS(v, X).e)
    e2Alpha = eAlpha .^ 2
    e2AlphaDiag = Diagonal(e2Alpha)

    alphaVar = inv(X'X) * (X' * e2AlphaDiag * X) * inv(X'X)
    alphaSE = sqrt(alphaVar)

    testAlpha = alpha / alphaSE

    XBeta = [ones(n - 1) v[1:n-1, :]]
    beta = OLS(v[2:n], XBeta).beta

    eBeta = diagm(OLS(v[2:n], XBeta).e)
    e2Beta = eBeta .^ 2
    e2BetaDiag = Diagonal(e2Beta)

    betaVar = inv(XBeta'XBeta) * (XBeta' * e2BetaDiag * XBeta) * inv(XBeta'XBeta)
    betaSE = sqrt(betaVar)

    testBeta = beta / betaSE

    return (alpha=alpha, beta=beta, testAlpha=testAlpha, testBeta=testBeta, 
    eAlpha = eAlpha, eBeta = eBeta, alphaVar = alphaVar, alphaSE = alphaSE, betaVar = betaVar, betaSE = betaSE)

end

Robust(RtAAPL)
Robust(RtGOOG)
Robust(RtIBM)
Robust(RtMSFT)
Robust(RtORCL)
Robust(RtSPX)
Robust(RtIndex)

# Forecasting
# For each asset, we choose Rt = alpha + beta Rt-1 + e

Robust(RtAAPL[2:1500])
Robust(RtGOOG[2:1500])
Robust(RtIBM[2:1500])
Robust(RtMSFT[2:1500])
Robust(RtORCL[2:1500])
Robust(RtSPX[2:1500])
Robust(RtIndex[2:1500])

# Apple
testAAPL = [ones(2517 - 1500) values(RtAAPL[1501:2517])]
betaAAPL = Robust(RtAAPL[2:1500]).beta
predAAPL = testAAPL * betaAAPL

plot(predAAPL, xlabel="Time", label="AAPL", title="Forecasting returns", ylabel="Predictions")

# Google
testGOOG = [ones(2517 - 1500) values(RtGOOG[1501:2517])]
betaGOOG = Robust(RtGOOG[2:1500]).beta
predGOOG = testGOOG * betaGOOG

plot(predGOOG, xlabel="Time", label="GOOG", title="Forecasting returns", ylabel="Predictions")

# IBM
testIBM = [ones(2517 - 1500) values(RtIBM[1501:2517])]
betaIBM = Robust(RtIBM[2:1500]).beta
predIBM = testIBM * betaIBM

plot(predIBM, xlabel="Time", label="IBM", title="Forecasting returns", ylabel="Predictions")


# Microsoft
testMSFT = [ones(2517 - 1500) values(RtMSFT[1501:2517])]
betaMSFT = Robust(RtMSFT[2:1500]).beta
predMSFT = testMSFT * betaMSFT

plot(predMSFT, xlabel="Time", label="MSFT", title="Forecasting returns", ylabel="Predictions")

# Oracle
testORCL = [ones(2517 - 1500) values(RtORCL[1501:2517])]
betaORCL = Robust(RtORCL[2:1500]).beta
predORCL = testORCL * betaORCL

plot(predORCL, xlabel="Time", label="ORCL", title="Forecasting returns", ylabel="Predictions")

# S&P 500
testSPX = [ones(2517 - 1500) values(RtSPX[1501:2517])]
betaSPX = Robust(RtSPX[2:1500]).beta
predSPX = testSPX * betaSPX

plot(predSPX, xlabel="Time", label="SPX", title="Forecasting returns", ylabel="Predictions")

# Index
testIndex = [ones(2517 - 1500) values(RtIndex[1501:2517])]
betaIndex = Robust(RtIndex[2:1500]).beta
predIndex = testIndex * betaIndex

plot(predIndex, xlabel="Time", label="Index", title="Forecasting returns", ylabel="Predictions")


function RMSE(forecast, realize)

    pred = values(forecast)
    val = values(realize)

    n = length(pred)
    w = (1 / n)

    a = (pred - val) .^ 2
    out = sum(a)

    se = sqrt(w * out)


    return (se = se)

end

RMSE(predAAPL, RtAAPL[1501:2517])
RMSE(predGOOG, RtGOOG[1501:2517])
RMSE(predIBM, RtIBM[1501:2517])
RMSE(predMSFT, RtMSFT[1501:2517])
RMSE(predORCL, RtORCL[1501:2517])
RMSE(predSPX, RtSPX[1501:2517])

# Forecasting volatilities

Robust(RtAAPL[1:1500]).beta
Robust(RtGOOG[1:1500]).beta
Robust(RtIBM[1:1500]).beta
Robust(RtMSFT[1:1500]).beta
Robust(RtORCL[1:1500]).beta
Robust(RtSPX[1:1500]).beta
Robust(RtIndex[1:1500]).beta

function Volatility(y)

    v = values(y)
    n = length(y)

    beta = Robust(v[1:1500]).beta

    e = Robust(v[1:1500]).eBeta
    e = diag(e)
    e2 = e .^ 2

    # Dynamic equation
    # e2[t] = mu + phi * e2[t-1] + u[t]

    resid = Robust(v).eBeta
    resid = diag(resid)
    resid2 = resid .^ 2

    coefficients = Robust(e2).beta
    mu = coefficients[1]
    phi = coefficients[2]

    eX = [ones(1017) resid2[1501:2517]]
    eBeta = OLS(e2[2:end], e2[1:end-1]).beta
    forecast = eX * eBeta

    return(beta = beta, mu = mu, phi = phi, eBeta = eBeta, forecast = forecast, e2 = e2)

end

vRtAAPL = Volatility(RtAAPL).forecast[:, 2]
vRtGOOG = Volatility(RtGOOG).forecast[:, 2]
vRtIBM = Volatility(RtIBM).forecast[:, 2]
vRtMSFT = Volatility(RtMSFT).forecast[:, 2]
vRtORCL = Volatility(RtORCL).forecast[:, 2]
vRtSPX = Volatility(RtSPX).forecast[:, 2]

# Apple
plot(vRtAAPL, xlabel = "Time", ylabel = "Predictions",
label = "AAPL", title = "Forecasting volatility of Apple")

# Google
plot(vRtGOOG, xlabel = "Time", ylabel = "Predictions",
label = "GOOG", title = "Forecasting volatility of Google")

# IBM
plot(vRtIBM, xlabel = "Time", ylabel = "Predictions",
label = "IBM", title = "Forecasting volatility of IBM")

# Microsoft
plot(vRtMSFT, xlabel = "Time", ylabel = "Predictions",
label = "MSFT", title = "Forecasting volatility of Microsoft")

# Oracle
plot(vRtORCL, xlabel = "Time", ylabel = "Predictions",
label = "ORCL", title = "Forecasting volatility of Oracle")

# S&P 500
plot(vRtSPX, xlabel = "Time", ylabel = "Predictions",
label = "SPX", title = "Forecasting volatility of the S&P 500")

# Robust CAPM

function CAPM(y)

    v = values(y)
    n = length(v)
    market = values(RtSPX)
    X = [ones(n) market]

    beta = regress(v, X).beta
    seOLS = regress(v, X).betaSE

    # Confidence Interval (OLS)
    Lower = beta[2] - (1.96 * seOLS[2])
    Upper = beta[2] + (1.96 * seOLS[2])

    ciOLS = [Lower Upper]

    HCSE = Robust(v).betaSE

    # Confidence Interval (HCSE)
    Low = beta[2] - (1.96 * HCSE)
    Up = beta[2] + (1.96 * HCSE)

    ciHCSE = [Low Up]

    return(beta = beta, seOLS = seOLS, 
    ciOLS = ciOLS, ciHCSE = ciHCSE)
end

CAPM(RtAAPL)
CAPM(RtGOOG)
CAPM(RtIBM)
CAPM(RtMSFT)
CAPM(RtORCL)
CAPM(RtSPX)