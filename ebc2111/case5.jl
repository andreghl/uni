# Case of Wed 5th of June 2024
# By André, Claudia, and Lotte

using DataFrames, Distributions, GLM, HypothesisTests, MarketData, Plots, TimeSeries, LinearAlgebra, Random, Statistics
println("Packages loaded!")

# Question 1  
# -> Exercise 4.4 in the book
# For answers. See notebook!

# Question 2
# Chose the Pareto Distribution
# To get the formula in the book:
# set Pareto.alpha = theta (in the book)
# set Pareto.theta = 1

n = 250
theta = 3
x = rand(Pareto(theta, 1), n)

lnx = log.(x)

n / sum(lnx)

# Question 3
Random.seed!(10)

n = 100
beta = [500; 100]
iota = ones(n)
educ = rand(Uniform(0, 5), n)
eps = rand(Normal(0, 400), n)
X = [iota educ]

function OLS(y, X)

    beta = inv(X'X)X'y
    yHat = X * beta
    e = y - (X * beta) 

    return(beta = beta, yHat = yHat, e = e)
    
end

T = 10000
estimates = zeros(T)



for i in 1:T

    # educ = rand(Uniform(0, 5), n)
    eps = rand(Normal(0, 400), n)
    # X = [iota educ]
    wage = X*beta .+ eps

    estimates[i] = OLS(wage, X).beta[2]
end

estimates

# Histogram of estimated beta
Plot1 = histogram(estimates, bins = 100,
xlabel = "Estimates", ylabel = "Frequency",
title = "Distribution of Beta", label = "eps")

for i in 1:T

    # educ = rand(Uniform(0, 5), n)
    eps = rand(Normal(0, 400), n)
    eps2 = eps .^ 2
    # X = [iota educ]
    wage = X*beta .+ eps2

    estimates[i] = OLS(wage, X).beta[2]
end

estimates

# Histogram of estimated beta
Plot2 = histogram(estimates, bins = 100,
xlabel = "Estimates", ylabel = "Frequency",
title = "Distribution of Beta", label = "eps2")

# Question 4
Random.seed!(10)

n = 100
beta = [500; 100]
iota = ones(n)
educ = rand(Uniform(0, 5), n)
eps = rand(Normal(0, 400), n)
X = [iota educ]
wage = X*beta .+ eps

println("Estimated Coefficent:", OLS(wage, X).beta[2])
residuals = OLS(wage, X).e

# Bootstrap
function Bootstrap(y, x, N = 100)

beta1 = zeros(N)
beta2 = zeros(N)
e = zeros(length(y), N)


    for i in 1:N

        y = sample(y, length(y), replace = true)
        x = sample(x, length(x), replace = true)
        X = [ones(length(x)) x]

        beta = OLS(y, X).beta
        resid = OLS(y, X).e

        beta1[i] = beta[1]
        beta2[i] = beta[2]
        e[:, i] = resid
    end

    betaBoot = [mean(beta1); mean(beta2)]

    e = vec([e[i, j] for i in 1:100, j in 1:N])
    
    return(beta = betaBoot, beta1 = beta1, beta2 = beta2, e = e)
 
end

Random.seed!(10)
Bootstrap(wage, educ, 10)

Bootstrap(wage, educ, 499).beta2

OLS(wage, X)

histogram(Bootstrap(wage, educ, 499).beta2)
println("No satisfying answer found for Q4")

# Question 5
# Question 5
alpha = 0.5
rho = 0.8
n = 10
rep = 10000

# Exercise 5
epsilon = zeros(n, rep)

for i in 1:rep
    epsilon[:, i] = rand(Normal(0, 1), n)
end

epsilon

# Exercise 5
yt = zeros(n, rep)

for r in 1:rep
    for i in 2:n

    yt[i, r] = alpha .+ rho .* yt[i-1, r] .+ epsilon[i - 1, r]
    end
end

yt

# Exercise 5
rhoEstimate = zeros(rep)

for i in 1:rep

    rhoEstimate[i] = OLS(yt[2:end, i], [ones(n - 1) yt[1:end - 1, i]]).beta[2]
end

# Histogram of estimated beta
Plot4 = histogram(rhoEstimate,
xlabel = "Rho", ylabel = "Frequency",
title = "Distribution of Rho")

function q5(n = 10, rep = 10000)

    alpha = 0.5
    rho = 0.8

    epsilon = zeros(n, rep)

    for i in 1:rep
    epsilon[:, i] = rand(Normal(0, 1), n)
    end

    yt = zeros(n, rep)

    for r in 1:rep
        for i in 2:n

    yt[i, r] = alpha .+ rho .* yt[i-1, r] .+ epsilon[i - 1, r]
        end
    end

    rhoEstimate = zeros(rep)

    for i in 1:rep

    rhoEstimate[i] = OLS(yt[2:end, i], [ones(n - 1) yt[1:end - 1, i]]).beta[2]
    end

    # Histogram of estimated beta
    Plot = histogram(rhoEstimate,
    xlabel = "Rho", ylabel = "Frequency",
    title = "Distribution of Rho")

    return(Plot)
    
end

# Exercise 5
q5(25)

# Exercise 5
q5(100)

# Exercise 5
q5(500)

