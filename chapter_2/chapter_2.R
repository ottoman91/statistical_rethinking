#chapter 2

#R Code 2.1
ways <- c(0, 3, 8, 9, 0)
ways / sum(ways)

#R Code 2.2
dbinom(6, size = 9, prob = 0.5)

# R Code 2.3
# Using grid approximation to calculate posterior probabilities for the globe tossing example

#define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

#define prior
prior <- rep(1, 20)

#compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

#compute product of likelihood and prior
unstd.posterior <- prior * likelihood

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)


# R code 2.4
#display the posteriors calculated in R code 2.3
plot(
  p_grid,
  posterior,
  type = "b",
  xlab = "Probability of Water",
  ylab = "Posterior Probability"
  )
mtext("20 points")

#R Code 2.5
# Replicating the plots in Figure 2.5 in the book, in chapter 2

prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom(6, size = 9, prob = p_grid)
unstd.posterior <- prior * likelihood
posterior <- unstd.posterior / sum(unstd.posterior)
plot(
  p_grid,
  posterior,
  type = "b",
  xlab = "Probability of Water",
  ylab = "Posterior Probability"
)
mtext("20 points, plot with a step prior")

prior <- exp(-5 * abs(p_grid - 0.5))
likelihood <- dbinom(6, size = 9, prob = p_grid)
unstd.posterior <- prior * likelihood
posterior <- unstd.posterior / sum(unstd.posterior)
plot(
  p_grid,
  posterior,
  type = "b",
  xlab = "Probability of Water",
  ylab = "Posterior Probability"
)
mtext("20 points, plot with a peaked prior")

# R Code 2.6
library(rethinking)

globe.qa <- rethinking::map(
  alist(
    #binomial likelihood
    w ~ dbinom(9, p),
    #uniform prior
    p ~ dunif(0,1)
  ),
  data = list(w = 6)
)

#display summary of quadratic expression
precis(globe.qa)

#R Code 2.7
# visualizing accuracy of quadratic approximation compared to exact posterior distribution

#analytical calculation
w <- 6
n <- 9

curve(dbeta(x, w + 1, n - w + 1), from = 0, to = 1)

#quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE)


