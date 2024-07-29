using DataFrames, Distributions, GLM, HypothesisTests, MarketData, Plots, TimeSeries, LinearAlgebra, Random, Statistics
println("Done!")

start = DateTime(2014, 5, 15)  #DateTime(YYYY, M, DD)
finish = DateTime(2024, 5, 15)  #DateTime(YYYY, M, DD)

# Getting the data for Microsoft
MSFT = yahoo(:MSFT, YahooOpt(period1 = start, period2 = finish))
PtMSFT = MSFT["Close"]
RtMSFT = diff(log.(PtMSFT))

# Market Index: S&P 500
SPX = yahoo("^GSPC", YahooOpt(period1=start, period2 = finish))
PtSPX = SPX["Close"]
RtSPX = diff(log.(PtSPX))

# OLS Regression
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

    betaSE = sqrt.(diag(inv(X'X)*s2))

    return (beta = beta,n = n, K = K,yHat = yHat,e = e,SSR = SSR,s2 = s2,s = s,yMean = yMean,TSS = TSS,RSq = RSq,Adj = Adj, betaSE = betaSE)
end

# OLS
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

    return (beta=beta,n = n,K = K,yHat = yHat,e = e,SSR = SSR,s2 = s2,s = s,YMean = yMean,TSS = TSS,RSq = RSq,Adj = Adj,AIC = AIC,SC = SC)
end

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

# Robust CAPM
function CAPM(y)

    v = values(y)
    n = length(v)
    market = values(RtSPX)
    X = [ones(n) market[1:n]]

    beta = regress(v, X).beta
    seOLS = regress(v, X).betaSE

    # Confidence Interval (OLS)
    Lower = beta[2] - (1.96 * seOLS[2])
    Upper = beta[2] + (1.96 * seOLS[2])

    ciOLS = [Lower Upper]

    return(beta = beta, seOLS = seOLS, 
    ciOLS = ciOLS)
end

# Q1 -----

# Exercise 1
n = size(RtMSFT, 1)
index = [ones(n) values(RtSPX)]

coefficents = zeros(n - 100, 2)


for i in 101:n
    # Recursive coefficients
    coefficents[i - 100, 1:2] = [CAPM(RtMSFT[1:i]).beta[1] CAPM(RtMSFT[1:i]).beta[2]]
end

# Alpha
plot(coefficents[:, 1],
 xlabel = "sample size", ylabel = "alpha",
 title = "Development of alpha",
 label = "")

# beta
plot(coefficents[:, 2],
 xlabel = "sample size", ylabel = "beta",
 title = "Development of beta",
 label = "")

# Exercise 2

Breakend = DateTime(2020, 02, 28)
Break = DateTime(2020, 02, 29)

# Finding the index of the break
MSFT1 = yahoo(:MSFT, YahooOpt(period1 = start, period2 = Breakend))
# Break: i = 1458
covid = [zeros(1457); ones(n - 1457)]

RtMSFT1 = RtMSFT[1:1457]
RtMSFT2 = RtMSFT[1458:2516]

RtSPX1 = RtSPX[1:1457]
RtSPX2 = RtSPX[1458:2516]

Market1 = [ones(1457) values(RtSPX1)]
Market2 = [ones(1059) values(RtSPX2)]

# Exercise 3
SSR1 = OLS(RtMSFT1, Market1).SSR
SSR2 = OLS(RtMSFT2, Market2).SSR

USSR = SSR1 + SSR2

# Exercise 4

RSSR = OLS(RtMSFT, index).SSR
isapprox(USSR, RSSR) # The sum of SSRs not equal to restricted SSR

F = ((RSSR - USSR) / 2) / (USSR / (n - 4))
# F = 4.79771324374717

quantile(FDist(2, n - 4), 0.95)
# c = 2.9993077318259718
# Since T > c, we can reject the null hypothesis of no break

# Q2 -----
println("Q2")
