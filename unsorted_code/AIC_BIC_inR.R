mod <- lm(mpg ~ wt + hp, data = mtcars)

k <- length(coef(mod))+1 # number of parameters plus variance term
n <- length(residuals(mod)) # number of observations
sigma2 <- sum(residuals(mod)^2) / n


log_lik<- -n / 2 * log(2 * pi * sigma2) - sum(residuals(mod)^2) / (2 * sigma2)

# check of correctness
logLik(mod) == log_lik

my_AIC <- 2 * k - (2 * log_lik)
my_BIC <- k * log(n) - 2 * log_lik

my_AIC == AIC(mod)
my_BIC == BIC(mod)
