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

# R Code 3.14
#Report parameter with highest posterior probability (MAP)
p_grid[which.max(posterior)]

# R Code 3.15
chainmode(samples, adj = 0.01)

#R Code 3.16
# calculating expected loss in a loss function that implies loss is proportional to distance between predicted value and actual value
# assumed p = 0.5
sum(posterior * abs(0.5 - p_grid))

# R code 3.17
loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))
p_grid[which.min(loss)]

# R Code 3.20
#Sample values based on probability from binomial distribution
dbinom(0:2, size = 2, prob = 0.7)

# R Code 3.21
#Sample data
rbinom(1, size = 2, prob = 0.7)

# R Code 3.22
rbinom(10, size = 2, prob = 0.7)

# R Code 3.23
dummy_w <- rbinom(1000, size = 2, prob = 0.7)
table(dummy_w) / 1e5

#R Code 3.24
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab = "dummy water count")

# R code 3.26
# calculating posterior predictive distribution of number of water samples from samples
w <- rbinom(1e4, size = 9, prob = samples)
