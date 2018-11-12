#Problem 1
#1)
x <- matrix(runif(1000 * 1000, min = 0, max = 6), nrow = 1000)
#x_sqr <- x*x
eps <- matrix(rnorm(1000 * 1000, sd = x), nrow = 1000)
y <- 1 + 2 * x + eps
theta <- matrix(nrow = 1000, ncol = 2)
#matrix of our estimated parametrs
r_sqr <- matrix(nrow = 1000, ncol = 1)
ssr <- matrix(nrow = 1000, ncol = 1)

for (i in 1:1000) {
  theta[i, ] <- lm(y[, i] ~ x[, i])$coefficients
  r_sqr[i, ] <- summary(lm(y[, i] ~ x[, i]))$r.squared
  # resid <- sum_matrix$residuals
  # resid_sqr <- resid^2
  ssr[i, ] <- sum((summary(lm(y[, i] ~ x[, i]))$residuals) ^ 2)
}
par(mfrow = c(1, 2))
hist(r_sqr, main = "Dist. of R^2 in simulation", xlab = "R^2")
hist(ssr, main = "Dist. of SSR in simulation", xlab = "Sum of Squared Residuals")
alpha_hat <- mean(theta[, 1])
alpha_hat
beta_hat <- mean(theta[, 2])
beta_hat

#2)
sim_ols <- function(n, n_sims, var_vec = x^2 , ...) {
  x <- matrix(runif(n * n_sims, min = 0, max = 6), nrow = n)
  #x_sqr <- x*x
  eps <- matrix(rnorm(n * n_sims, sd = sqrt(var_vec)), nrow = n)
  y <- 1 + 2 * x + eps
  theta <- matrix(nrow = n_sims, ncol = 2)
  #matrix of our estimated parametrs
  r_sqr <- matrix(nrow = n_sims, ncol = 1)
  ssr <- matrix(nrow = n_sims, ncol = 1)
  
  for (i in 1:n_sims) {
    theta[i, ] <- lm(y[, i] ~ x[, i])$coefficients
    r_sqr[i, ] <- summary(lm(y[, i] ~ x[, i]))$r.squared
    # resid <- sum_matrix$residuals
    # resid_sqr <- resid^2
    ssr[i, ] <- sum((summary(lm(y[, i] ~ x[, i]))$residuals) ^ 2)
  }
  par(mfrow = c(1, 2))
  hist(r_sqr, main = "Dist. of R^2 in simulation", xlab = "R^2", ...)
  hist(ssr, main = "Dist. of SSR in simulation", xlab = "Sum of Squared Residuals", ... )
  alpha_hat <- mean(theta[, 1])
  alpha_hat
  beta_hat <- mean(theta[, 2])
  beta_hat
}