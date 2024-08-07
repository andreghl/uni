using LinearAlgebra, GLM, Distributions, Plots, Statistics, Random

# Dependent variable
y = [1649; 1435; 1864; 1892; 1913]

# Independent variables

W = [13; 9; 17; 19; 20]
i = ones(5)

# Matrix

X = [i W]

# Exercise 1

beta = inv(X'X)X'y

# Exercise 2

e = y .- (X*beta)

# Exercise 2 

error = inv(i'i)i'e

# Exercise 2

SSR = e'e

# Exercise 3

mean = inv(i'i)i'y

# Exercise 3

TSS = (y .- mean)'*(y .- mean)

# Exercise 3

ESS = TSS - SSR

# Exercise 4

RSquared = 1 - (SSR/TSS)

# Exercise 4

adjRSquared = 1 - (SSR/(5 - 2))/(TSS/(5 - 1))

# Exercise 5

W2 = W .^ 2

# Exercise 5

X = [i W W2]

# Exercise 6

beta = inv(X'X)X'y

# Exercise 7

e = y .- (X*beta)

# Exercise 7

SSR = e'e

# Exercise 7

RSquared = 1 - (SSR/TSS)

# Exercise 7

adjRSquared = 1 - (SSR/(5 - 3))/(TSS/(5 - 1))

# Exercise 1

Random.seed!(666) # Should be number of the beast

i = ones(100)

# Exercise 1

Random.seed!(666)

eps = rand(Normal(0, 4), 100)

# Exercise 1

Random.seed!(666)

educ = rand(Uniform(0, 15), 100)

# Exercise 1

X = [i educ]

beta = [500; 100]

wage = (X*beta) .+ eps

# Exercise 2

scatter(educ, wage)

# Exercise 3

coeff = inv(X'X)X'wage

# Exercise 4

e = wage .- (X*coeff)

# Exercise 4

SSR = e'e

# Exercise 4

mean = inv(i'i)i'wage

TSS = (wage .- mean)'*(wage .- mean)

# Exercise 4

RSquared = 1 - (SSR/TSS)

# Exercise 4

adj = 1 - (SSR/(5 - 2))/(TSS/(5 - 1))

# Exercise 5

WageHat = (X*coeff) .+ e

# Exercise 5

x = range(0, 100, length=100)

plot(x, [wage, WageHat])

# Exercise 6

sqrtEduc = educ .^ (1/2)

# Exercise 6

X = [i educ sqrtEduc]

# Exercise 6

coeffi = inv(X'X)X'wage

# Exercise 7

e = wage .- (X*coeffi)

# Exercise 7

SSR = e'e

mean = inv(i'i)i'wage

TSS = (wage .- mean)'*(wage .- mean)

# Exercise 7

RSquared = 1 - (SSR/TSS)

# Exercise 7

adj = 1 - (SSR/(5 - 3))/(TSS/(5 - 1))

# Exercise 8

educBeta = inv(i'i)i'educ

# Exercise 8

resEduc = educ .- (i*educBeta)

# Exercise 8

sqrteducBeta = inv(i'i)i'sqrtEduc

# Exercise 8

resSqrtEduc = sqrtEduc .- (i * sqrteducBeta)

# Exercise 8

wageBeta = inv(i'i)i'wage

# Exercise 8

resWage = wage .- (i * wageBeta)

# Exercise 8

X = [resEduc resSqrtEduc]

betaFV = inv(X'X)X'resWage

# Exercise 2

Random.seed!(666)

educ = rand(Uniform(0, 15), 100)

eps1 = rand(Normal(0, 4), 50)
eps2 = rand(Normal(0, 25), 50)

beta = [500; 100]

eps = vcat(eps1, eps2)

# Exercise 2

X = [i educ]

wage = X*beta .+ eps

# Exercise 3

scatter(eps)

# Exercise 4

coefficients = inv(X'X)X'wage

# Exercise 5

one = ones(50)

zero = zeros(50)

male = vcat(one, zero)

female = vcat(zero, one)

# Exercise 5

X = [i educ male female]

# Exercise 5

X = [i educ male]

# Exercise 5

OLS = inv(X'X)X'wage