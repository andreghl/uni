using Distributions, Random, DataFrames, Plots, LinearAlgebra, XLSX

function regress(y, X) 

    n = size(X, 1) # Sample size
    K = size(X, 2) # Number of regressors

    beta = inv(X'X)X'y # Estimated coefficients
    yHat = X * beta # Estimates for dependent
    e = y - (X*beta) # Residuals

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

    return(beta = beta, n, K, yHat, e, SSR, s2, s, yMean, TSS, RSq, Adj, betaSE)
end

# Exercise 2.10
Random.seed!(1967)

function Ex210(n, plotB=0)

beta = zeros(2, 10000)

for i in 1:10000
    
    x = rand(Normal(0, 2), n)
    b = ones(n)
    X = [b x]
    u = rand(Normal(0, 1), n)
    Beta = [5; 2]

    y = (X * Beta) + u

    estimates = regress(y, X)
    beta[:, i] = estimates.beta 
end

if plotB == 0
    histogram(beta[1, 1:10000], ylabel = "Frequency", xlabel = "Observations: $n", title = "Histogram of: beta0")
elseif plotB == 1
    histogram(beta[2, 1:10000], ylabel = "Frequency", xlabel = "Observations: $n", title = "Histogram of: beta1")
else
    println("Error! beta is incorrect")
end
end

# Exercise 2.10
Ex210(50, 0)

# Exercise 2.10
Ex210(50, 1)

# Exercise 2.10
Ex210(200, 0)

# Exercise 2.10
Ex210(200, 1)

# Exercise 2.10
Ex210(500, 0)

# Exercise 2.10
Ex210(500, 1)

# Exercise 2.10
Ex210(1000, 0)

# Exercise 2.10
Ex210(1000, 1)

# Exercise 2.11
Random.seed!(1967)

function Ex211(n, plotB=0)

    beta = zeros(2, 10000)
    
    for i in 1:10000
        
        x = rand(Normal(0, 2), n)
        b = ones(n)
        X = [b x]
        u = [rand(Normal(0, xi^2)) for xi in x]
        Beta = [5; 2]
    
        y = (X * Beta) + u
    
        estimates = regress(y, X)
        beta[:, i] = estimates.beta 
    end
    
    if plotB == 0
        histogram(beta[1, 1:10000], ylabel = "Frequency", xlabel = "Observations: $n", title = "Histogram of: beta0")
    elseif plotB == 1
        histogram(beta[2, 1:10000], ylabel = "Frequency", xlabel = "Observations: $n", title = "Histogram of: beta1")
    else
        println("Error! beta is incorrect")
    end
end

# Exercise 2.11
Ex211(50, 0)

# Exercise 2.11
Ex211(50, 1)

# Exercise 2.11
Ex211(200, 0)

# Exercise 2.11
Ex211(200, 1)

# Exercise 2.11
Ex211(500, 0)

# Exercise 2.11
Ex211(500, 1)

# Exercise 2.11
Ex211(1000, 0)

# Exercise 2.11
Ex211(1000, 1)

# Exercise 2.6

xl = DataFrame(XLSX.readtable("ME-data-2-03.xlsx", "Sheet1"))
data = Float64.(xl)

scatter(data[:, 1], data[:, 2], xlabel = "fluoride", ylabel = "caries", )

# Exercise 2.6

n = 18
i = ones(n)
x = data[:, 1]
X = [i x]
y = data[:, 2]

# Exercise 2.6 (Case 3)

println("The RSquared is:", regress(y, X).RSq)
println("The Ajusted is:", regress(y, X).Adj)

# Exercise 2 (Case 3)

x2 = x .* x
X = [i x x2]

println("The RSquared is:", regress(y, X).RSq)
println("The Ajusted is:", regress(y, X).Adj)

# Exercise 2 (Case 3)

O = [zeros(17) ; 1]
X = [i x x2 O]

println("The RSquared is:", regress(y, X).RSq)
println("The Ajusted is:", regress(y, X).Adj)

# Exercise 2 (Case 3)

X = [i x O]

println("The RSquared is:", regress(y, X).RSq)
println("The Ajusted is:", regress(y, X).Adj)

X = [i x x2 O] # The third model should be the best

println(regress(y, X).betaSE)

# Exercise 3 (Case 3)

X = [i x]

AIC = log(regress(y, X).s2) + (2*regress(y, X).K / regress(y, X).n)

# Exercise 3 (Case 3)

SC = log(regress(y, X).s2) + (log(regress(y, X).n)*regress(y, X).K / regress(y, X).n)

# Exercise 3 (Case 3)

X = [i x x2 ]

AIC = log(regress(y, X).s2) + (2*regress(y, X).K / regress(y, X).n)

# Exercise 3 (Case 3)

SC = log(regress(y, X).s2) + (log(regress(y, X).n)*regress(y, X).K / regress(y, X).n)

# Exercise 3 (Case 3)

X = [i x x2 O]

AIC = log(regress(y, X).s2) + (2*regress(y, X).K / regress(y, X).n)

# Exercise 3 (Case 3)

SC = log(regress(y, X).s2) + (log(regress(y, X).n)*regress(y, X).K / regress(y, X).n)

# Exercise 3 (Case 3)

X = [i x O]

AIC = log(regress(y, X).s2) + (2*regress(y, X).K / regress(y, X).n)

# Exercise 3 (Case 3)

SC = log(regress(y, X).s2) + (log(regress(y, X).n)*regress(y, X).K / regress(y, X).n)