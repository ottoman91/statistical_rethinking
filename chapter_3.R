# R code 3.2
# reminder on how to generate samples from grid approximation

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

#R code 3.3
# extract 1000 samples of parameter values from the posterior
samples <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)

#R Code 3.4
plot(samples)

# R code 3.5
#plot density estimates of the samples
library(rethinking)
dens(samples)

#R code 3.6
#posterior probabilyt that proportion of water is less than 0.5
sum(posterior[p_grid < 0.5])

#R cODE 3.7
# find out posterior probability that proportion of water is less than 0.5 by samples
sum(samples < 0.5) / 1e4

#R code 3.9
# find out boundaries of lower 80% posterior probabilities
quantile(samples, 0.8)

#R code 3.10
# find out boundaries of middle 80% posterior probability distribution
quantile(samples, c(0.10, 0.90))

# R Code 3.11
#
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

# R Code 3.12
# Calculate 50% percentile interval(PI)
PI(samples, prob = 0.5)

#R Code 3.13
# Calculate Highest Posterior density interval
HPDI(samples, prob = 0.5)
