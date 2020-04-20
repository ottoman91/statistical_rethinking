# code from chapter 5, multivariate linear models

#R Code 5.1

#load data
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

#standardise predictor
d$MedianAgeMarriage.s <-
  (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)

#fit model
m5.1 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA * MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)

#compute percentile interval of mean
MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)

#plot it all
plot(Divorce ~ MedianAgeMarriage.s, data = d, col = rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)
