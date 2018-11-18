#Problem 1
#1)
# Generate x in a form of the matrix so we can easily use it later
x <- matrix(runif(1000 * 1000, min = 0, max = 6), nrow = 1000)
# Epsilons are generated much in the same way
eps <- matrix(rnorm(1000 * 1000, sd = x), nrow = 1000)
# Y is defined according to the formula
y <- 1 + 2 * x + eps

# Predefining a space for the output 
# so as to increase efficiency
# Theta matrix of our estimated parametrs
theta <- matrix(nrow = 1000, ncol = 2)
# Matrix of R squared from each simulation
r_sqr <- matrix(nrow = 1000, ncol = 1)
# Matrix of SSR from each simulation
ssr <- matrix(nrow = 1000, ncol = 1)

# Actual simulation
for (i in 1:1000) {
  theta[i, ] <- lm(y[, i] ~ x[, i])$coefficients
  r_sqr[i, ] <- summary(lm(y[, i] ~ x[, i]))$r.squared
  # resid <- sum_matrix$residuals
  # resid_sqr <- resid^2
  ssr[i, ] <- sum((summary(lm(y[, i] ~ x[, i]))$residuals) ^ 2)
}

#Plotting the graphs
par(mfrow = c(1, 2))
hist(r_sqr, main = "Dist. of R^2 in simulation", xlab = "R^2")
hist(ssr, main = "Dist. of SSR in simulation", xlab = "Sum of Squared Residuals")

# Coefficients resulting from the simulation
alpha_hat <- mean(theta[, 1])
# Intercept point estimate
alpha_hat
beta_hat <- mean(theta[, 2])
# Slope point estimate
beta_hat

#2)
# Generalized view of what we had in 1)
sim_ols <- function(n, n_sims, var_vec = x^2 , ...) {
  # Model pre-definition
  x <- matrix(runif(n * n_sims, min = 0, max = 6), nrow = n)
  eps <- matrix(rnorm(n * n_sims, sd = sqrt(var_vec)), nrow = n)
  y <- 1 + 2 * x + eps
  theta <- matrix(nrow = n_sims, ncol = 2)
  r_sqr <- matrix(nrow = n_sims, ncol = 1)
  ssr <- matrix(nrow = n_sims, ncol = 1)
  
  # Model estimation
  for (i in 1:n_sims) {
    theta[i, ] <- lm(y[, i] ~ x[, i])$coefficients
    r_sqr[i, ] <- summary(lm(y[, i] ~ x[, i]))$r.squared
    # resid <- sum_matrix$residuals
    # resid_sqr <- resid^2
    ssr[i, ] <- sum((summary(lm(y[, i] ~ x[, i]))$residuals) ^ 2)
  }
  
  # Histograms
  par(mfrow = c(1, 2))
  hist(r_sqr, main = "Dist. of R^2 in simulation", xlab = "R^2", ...)
  hist(ssr, main = "Dist. of SSR in simulation", xlab = "Sum of Squared Residuals", ... )
  
  # Estimated coefficients
  alpha_hat <- mean(theta[, 1])
  alpha_hat
  beta_hat <- mean(theta[, 2])
  beta_hat
}