# 1. Matrices for y and W:

y = [1649, 1435, 1864, 1892, 1913]

W = [13, 9, 17, 19, 20]

# 2. Plot y and W against time

time = [1, 2, 3, 4, 5]

using Plots
plot(time, y)

plot(time, W)

# 3. Make a scatter plot between y and W , interpret the potential relationship

scatter(W, y)

# 4. Generate a column vector i with 5 ones and create the 5 * 2 matrix of regressors X = (i : W)

i = [1, 1, 1, 1, 1]     # The column vector

X = hcat(i, W) # The 5 by 2 matrix of regressor

# 5. Compute X'X and interpret the 4 elements of that matrix

A = X'X

# 6. Compute the determinant and the trace of X'X

using LinearAlgebra
det(A) # The det is 416

tr(A)  # The trace is 1305

# 7. Compute (X'X)^-1 and check whether (X'X)^-1 *(X'X) = is

B = inv(A)

I = A*B

# 8. Determine mathematically (X'X)'. Compute it in Julia to verify your results.

A'

# 9. Compute on the data \bar{y} using ONLY the vectors y and i

X = hcat(y, i)

A = X'X

# mean = A[2,1] / A[2, 2] # -> 8753 / 5 = 1750.6

mean = (i'y)/(i'i)

# 10. Compute TSS = (y - mean)'(y -  mean)

T = (y .- mean)

TSS = T'T

# Estimate the parameters by OLS using the 2 by 1 vector 

X = hcat(i, W)
beta0 = inv(X'X)X'y

# 3. Compare with a Julia OLS routine of your choice.

using DataFrames, GLM

y = Int64.(y)
i = Int64.(i)
W = Int64.(W)

df = DataFrame(y = y, i = i, W = W)

model1 = lm(@formula(y ~ W), df)

# 4. Estimate the parameters by OLS using the scalar. Namely do not include an intercept. Compare with the correct model.

beta1 = inv(W'W)W'y

# 5. Compute the vector of residuals e0 and e1 with beta0 and beta1. Compute e0 and e1. Interpret.

e0 = y .- X*beta0

inv(i'i)i'e0

# 5. Compute the vector of residuals e0 and e1 with beta0 and beta1. Compute e0 and e1. Interpret.

e1 = y .- W*beta1

inv(i'i)i'e1

# 6. Using the complete model with 'beta0', compute e0'*X

e0' * X

# 7. With your estimated equation using matrices, forecast using Julia and OLS formula the consumption for Saturday and Sunday using the data on weather.

we = [13, 10]

forecast = beta0[1] .+ (beta0[2] * we)