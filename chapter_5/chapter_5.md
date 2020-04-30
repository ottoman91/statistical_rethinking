Chapter 5
================
Usman Khaliq
2020-04-29

``` r
# Libraries
library(tidyverse)
library(rethinking)
```

R Code 5.1

``` r
data("WaffleDivorce")
d <- WaffleDivorce

#standardise predictor
d$MedianAgeMarriage.s <- 
  (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage) 

#fit model
m5.1 <- rethinking::map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA * MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = d
)
```

R Code 5.2

Now, plot the raw data, draw the posterior mean regression line, and
draw the shaded confidence region.

``` r
#compute percentile interval of mean
MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)

#plot all
plot(Divorce ~ MedianAgeMarriage.s, data = d, col = rangi2)
abline(m5.1)
```

    ## Warning in abline(m5.1): only using the first two of 3 regression coefficients

``` r
shade(mu.PI, MAM.seq)
```

![](chapter_5_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

R Code 5.3 Now, lets fit a regression line for relationship between the
rate of marriage and the divorce rate.

``` r
d$Marriage.s <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)

m5.2 <- rethinking::map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR * Marriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
```

``` r
#compute percentile interval of mean
MAM.seq <- seq(from = -2.5, to = 3, length.out = 30)
mu <- link(m5.2, data = data.frame(Marriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)

#plot
plot(Divorce ~ Marriage.s, data = d, col = rangi2)
abline(m5.2)
```

    ## Warning in abline(m5.2): only using the first two of 3 regression coefficients

``` r
shade(mu.PI, MAM.seq)
```

![](chapter_5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

R Code 5.4

Lets fit a miltivariate model for association between marriage rate and
age of marriage on divorce rates.

``` r
m5.3 <- rethinking::map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR * Marriage.s + bA * MedianAgeMarriage.s,
    a ~ dnorm(0, 10),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)

precis(m5.3)
```

    ##             mean        sd       5.5%      94.5%
    ## a      9.6839836 0.2036147  9.3585680 10.0093992
    ## bR    -0.1321705 0.2794292 -0.5787523  0.3144113
    ## bA    -1.1347446 0.2797178 -1.5817877 -0.6877015
    ## sigma  1.4400606 0.1443474  1.2093656  1.6707556

Lets visualize the above posterior distribution estimates

``` r
plot(precis(m5.3))
```

![](chapter_5_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

The above plot can be interpreted as saying that once the median age of
marriage for a state is known, little or no additinal predictive power
is gained by also knowing the rate of marrianges in that state.

R Code 5.6

Lets calculate the predictor variable residuals for the marraige rates.

``` r
m5.4 <- rethinking::map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b * MedianAgeMarriage.s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
```

R Code 5.7

Now, calculate the residuals by subtracting observed marriage rate in
each state from predicted marriage rate.

``` r
#compute expected value of MAP, for each state
mu <- coef(m5.4)['a'] + coef(m5.4)['b'] * d$MedianAgeMarriage.s

#compute residual for each state
m.resid <- d$Marriage.s - mu
```

R Code 5.8

Lets plot the relationship between the residuals and the age of
marriage.

``` r
plot(Marriage.s ~ MedianAgeMarriage.s, d, col = rangi2)
abline(m5.4)
```

    ## Warning in abline(m5.4): only using the first two of 3 regression coefficients

``` r
#loop over states
for (i in 1:length(m.resid)) {
  #location of line segment
  x <- d$MedianAgeMarriage.s[i]
  #observed endpoint of line segment
  y <- d$Marriage.s[i]
  #draw the line segment
  lines(
    c(x, x),
    c(mu[i], y),
    lwd = 0.5,
    col = col.alpha("black", 0.7)
  )
}
```

![](chapter_5_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

R Code 5.9

Lets plot a counterfactual plot for the divorce model, with this model
showcasing the impact of the rate of marriage on divorce rate(while
keeping other parameters constant)

``` r
#prepare new counterfactual data
A.avg <- mean(d$MedianAgeMarriage.s)
R.seq <- seq(from = -3, to = 3, length.out = 30)

pred.data <- data.frame(
  Marriage.s = R.seq,
  MedianAgeMarriage.s = A.avg
)

#compute counterfactual mean divorce (mu)
mu <- link(m5.3, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

#simulate counterfactual divorce outcomes
R.sim <- sim(m5.3, data = pred.data, n = 1e4)

R.PI <- apply(R.sim, 2, PI)

#display predictions, hiding raw data with type = "n"
plot(Divorce ~ Marriage.s, data = d, type = "n")
mtext("MedianAgeMarriage.s = 0")
lines(R.seq, mu.mean)
shade(mu.PI, R.seq)
shade(R.PI, R.seq)
```

![](chapter_5_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Now, lets plot a counterfactual plot for the divorce model, with this
model showcasing the impact of the median age of marriage on divorce
rate(while keeping other parameters constant)

``` r
#prepare new counterfactual data
R.avg <- mean(d$Marriage.s)
A.seq <- seq(from = -3, to = 3, length.out = 30)

pred.data <- data.frame(
  Marriage.s = R.avg,
  MedianAgeMarriage.s = A.seq
)

#compute the counterfactual mean (mu)
mu <- link(m5.3, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

#simulate counterfactual divorce outcomes
R.sim <- sim(m5.3, data = pred.data, n = 1e4)
R.PI <- apply(R.sim, 2, PI)

#display predictions
plot(Divorce ~ MedianAgeMarriage.s, data = d, type = "n")
mtext("Marriage.s = 0")
lines(R.seq, mu.mean)
shade(mu.PI, R.seq)
shade(R.PI, R.seq)
```

![](chapter_5_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

R Code 5.11

Lets simulate predictions, averaging over the posterior.

``` r
#call link without specifying new data so that it uses original data
mu <- link(m5.3) 

#summarise samples across cases
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

#simulate observations 
divorce.sim <- sim(m5.3, n = 1e4)
divorce.PI <- apply(divorce.sim, 2, PI)
```

5 Code 5.12

Now, lets plot predictions against observations

``` r
plot(
  mu.mean ~ d$Divorce,
  col = rangi2,
  ylim = range(mu.PI),
  xlab = "Observed divorce",
  ylab = "Predicted divorce"
)
abline(a = 0, b = 1, lty = 2)
for(i in nrow(d))
  lines(rep(d$Divorce[i], 2), c(mu.PI[1, i], mu.PI[2, i]), col = rangi2)
```

![](chapter_5_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

The moderl underpredicts for states with very high divorce rates and
overpredicts for states with very low divorce rates.

R Code 5.14

Compute residuals.

``` r
#compute residuals
divorce.resid <- d$Divorce - mu.mean

#get ordering by divorce rate
o <- order(divorce.resid)

#make the plot
dotchart(divorce.resid[o], labels = d$Loc[o], xlim = c(-6, 5), cex = 0.6)
abline(v = 0, col = col.alpha("black", 0.2))
for(i in 1:nrow(d)){
  j <- o[i] #which state in order
  lines(d$Divorce[j] - c(mu.PI[1, j], mu.PI[2, j]), rep(i, 2))
  points(
    d$Divorce[j] - c(divorce.PI[1, j], divorce.PI[2, j]),
    rep(i, 2),
    pch = 3,
    cex = 0.6,
    col = "gray"
  )
}
```

![](chapter_5_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

R Code 5.16

``` r
data(milk)
d <- milk
glimpse(d)
```

    ## Rows: 29
    ## Columns: 8
    ## $ clade          <fct> Strepsirrhine, Strepsirrhine, Strepsirrhine, Strepsirr…
    ## $ species        <fct> Eulemur fulvus, E macaco, E mongoz, E rubriventer, Lem…
    ## $ kcal.per.g     <dbl> 0.49, 0.51, 0.46, 0.48, 0.60, 0.47, 0.56, 0.89, 0.91, …
    ## $ perc.fat       <dbl> 16.60, 19.27, 14.11, 14.91, 27.28, 21.22, 29.66, 53.41…
    ## $ perc.protein   <dbl> 15.42, 16.91, 16.85, 13.18, 19.50, 23.58, 23.46, 15.80…
    ## $ perc.lactose   <dbl> 67.98, 63.82, 69.04, 71.91, 53.22, 55.20, 46.88, 30.79…
    ## $ mass           <dbl> 1.95, 2.09, 2.51, 1.62, 2.19, 5.25, 5.37, 2.51, 0.71, …
    ## $ neocortex.perc <dbl> 55.16, NA, NA, NA, NA, 64.54, 64.54, 67.64, NA, 68.85,…

Hypothesis to test: primates with larger brains produce more energetic
milk so that their braiins can grow quickly.

Variables of interest: kcal.per.g : Kilocalories of energy per gram of
milk mass: Average female body mass, in kg neocortex.perc: percentage of
total brain mass that is neocortex mass

``` r
dcc <-
  d %>% 
  drop_na()
```

R Code 5.20

``` r
m5.5 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = dcc
)
```

Now, lets observe the quadratic approximate posterior

``` r
precis(m5.5, digits = 3)
```

    ##              mean         sd         5.5%      94.5%
    ## a     0.353338939 0.47071829 -0.398959803 1.10563768
    ## bn    0.004503205 0.00694034 -0.006588799 0.01559521
    ## sigma 0.165702598 0.02841440  0.120290904 0.21111429

R Code 5.23

Lets now plot the predicted mean and the 89% interval for the mean

``` r
np.seq <- 0:100
pred.data <- data.frame(neocortex.perc = np.seq)

mu <- link(m5.5, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, col = rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)
```

![](chapter_5_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

From the above, we can see that the MAP line is weakly positive.
However, the error range is quite high.

``` r
dcc$log.mass <- log(dcc$mass)

m5.6 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm * log.mass,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = dcc
) 

precis(m5.6)
```

    ##              mean         sd        5.5%        94.5%
    ## a      0.70513320 0.04870546  0.62729247 0.7829739218
    ## bm    -0.03167421 0.02028326 -0.06409078 0.0007423665
    ## sigma  0.15686198 0.02689762  0.11387438 0.1998495678

Now, lets plot this relationship.

``` r
np.seq <- -100:100
pred.data <- data.frame(log.mass = np.seq)

mu <- link(m5.6, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc, col = rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)
```

![](chapter_5_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

Stronger negative relationship, but the confidence interval is still
quite wide.

R Code 5.26

Now, add both predictor variables at the same time.

``` r
m5.7 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc + bm * log.mass,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), 
  data = dcc
) 

precis(m5.7)
```

    ##              mean          sd        5.5%       94.5%
    ## a     -1.08441273 0.467550779 -1.83164918 -0.33717628
    ## bn     0.02791702 0.007272544  0.01629409  0.03953995
    ## bm    -0.09634892 0.022453741 -0.13223433 -0.06046351
    ## sigma  0.11479063 0.019680911  0.08333674  0.14624453

Now, we can see that the estimate association of both predictors with
the outcome variables has increased. Lets visualise this relationship.

R Code 5.27

``` r
mean.log.mass <- mean(log(dcc$mass))
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc = np.seq,
  log.mass = mean.log.mass
)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)
```

![](chapter_5_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
mean.neocortex.perc <- mean(dcc$neocortex.perc)
np.seq <- -100:100
pred.data <- data.frame(
  neocortex.perc = mean.neocortex.perc,
  log.mass = np.seq
)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)
```

![](chapter_5_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

The phenomena above can be described as follows:

  - The two variables(neocortex size and body mass) are correlated with
    the outcome, but in opposite directions
  - Both of the explanatory variables are positively correlated with
    each other
  - Due to this, they tend to cancel each other out, unless both of the
    are explictly accounted for in a regression model.