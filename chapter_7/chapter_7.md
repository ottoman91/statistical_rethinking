Chapter 7 - Interactions
================
Usman Khaliq
2020-05-10

``` r
# Libraries
library(tidyverse)
library(rethinking)
# Parameters

#===============================================================================

# Code
```

R 7.1 Lets explore regressions of terrain ruggedness against economic
performance in Africa and outside of Africa

``` r
data("rugged")
d <- rugged
#transform outcome to log scale
d$log_gdp <- log(d$rgdppc_2000)

#remove countries which do not have GDP data
dd <- 
  d %>%
  drop_na(rgdppc_2000)

#split countries on whether they are in africa or not in africa
d.A1 <- 
  dd  %>% 
  filter(cont_africa == 1)

d.A0 <- 
  dd %>% 
  filter(cont_africa == 0)
```

R Code 7.2

Fit regression models to the data above.

``` r
#African nations
m7.1 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d.A1
) 

#non-African nations
m7.2 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d.A0
) 
```

``` r
#define sequence of rugged terrain values to make predictions on
rugged_seq_africa <- 
  seq(from = min(d.A1$rugged), to = max(d.A1$rugged), length.out = 30) 

mu_africa <- link(m7.1, data = data.frame(rugged = rugged_seq_africa))
mu.mean <- apply(mu_africa, 2, mean)
mu.PI <- apply(mu_africa, 2, HPDI, prob = 0.89) 


plot(log_gdp ~ rugged, data = d.A1, col = col.alpha(rangi2, 0.5))
mtext("Africa")
#draw MAP line
lines(rugged_seq_africa, mu.mean)

#draw HPDI region for line
shade(mu.PI, rugged_seq_africa)
```

![](chapter_7_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#define sequence of rugged terrain values to make predictions on
rugged_seq_not_africa <- 
  seq(from = min(d.A0$rugged), to = max(d.A0$rugged), length.out = 30) 

mu_not_africa <- link(m7.2, data = data.frame(rugged = rugged_seq_not_africa))
mu.mean <- apply(mu_not_africa, 2, mean)
mu.PI <- apply(mu_not_africa, 2, HPDI, prob = 0.89) 


plot(log_gdp ~ rugged, data = d.A0, col = col.alpha(rangi2, 0.5))
mtext("not Africa")
#draw MAP line
lines(rugged_seq_not_africa, mu.mean)

#draw HPDI region for line
shade(mu.PI, rugged_seq_not_africa)
```

![](chapter_7_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Its not a good idea to split data, like we split the data above. The
following are the reasons for it:

1)  Some parameters, such as sigma, were assumed to not depend on a
    country’s Africa identity. By splitting the data, we are hurting the
    accuracy of such estimates because we are now making two less
    accurate estimates instead of pooling all data into making one solid
    estimate.

2)  In order to make probability estimates about the `cont_africa`
    variable, we need to include it in the data that the model works on.
    Unless our model analyses all the data, it canot make a solid
    estimate of the uncertainty in the probability that `cont_africa`
    has on the outcome variable.

3)  We might need to use information criteria to compare how different
    models treat different continents. In order for the estimate to be
    as accurate as possible, the models should read all the data.

4)  In multilevel models, we can in fact learn across different
    categories. Therefore, it makes sense to not split data before
    inputting it into the model.

Lets see if adding the categorical variable(dummy variable)
`cont_africa` will reveal the reverse slope in the model.

R Code 7.3

The following is a simple linear regression model for `log_gdp` on
ruggedness over the entire dataset

``` r
m7.3 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)
```

The second is a model that includes a dummy variable for African
countries.

R Code 7.4

``` r
m7.4 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged + bA * cont_africa,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)
```

Now, lets compare the WAIC values of these models

R Code 7.5

``` r
compare(m7.3, m7.4)
```

    ##          WAIC       SE    dWAIC      dSE    pWAIC       weight
    ## m7.4 476.2279 15.22097  0.00000       NA 4.319099 1.000000e+00
    ## m7.3 539.5817 13.29388 63.35378 14.95569 2.709898 1.749451e-14

From the above, we can see that all weight is assigned to model m7.4.
The standard error in the difference of WAIC between the two models is
15, but the difference in WAIC values is 63, which implies a 95%
confidence interval difference of 63 +- 30. Thus, we can gauge from
these numbers that the dummy variable is picking up some important
information since m7.4 is superior to the simple model.

Now, lets plot the posterior prediction of m7.4 to see whether it
achieves different slopes within and outside of Africa.

R Code 7.6

``` r
rugged.seq <- seq(from = -1, to = 8, by = 0.25)

#compute mu over samples, fixing cont_africa = 0
mu.NotAfrica <- 
  link(m7.4, data = data.frame(cont_africa = 0, rugged = rugged.seq))

#compute mu over samples, fixing cont_africa = 1
mu.Africa <- 
  link(m7.4, data = data.frame(cont_africa = 1, rugged = rugged.seq))

#summarise to means and intervals 
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob = 0.97)
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)
```

``` r
plot(log_gdp ~ rugged, data = dd) 

#plot the regression lines
lines(rugged.seq, mu.NotAfrica.mean)
lines(rugged.seq, mu.Africa.mean, col = col.alpha(rangi2, 0.5))

#draw HPDI region for lines
shade(mu.NotAfrica.PI, rugged.seq)
shade(mu.Africa.PI, rugged.seq, col = col.alpha(rangi2, 0.5))
mtext("Africa is in Blue, Other Countries in Black")
```

![](chapter_7_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

From the above, we can see that by just adding a dummy variable to the
model, the model still predicted a poor relationship between ruggedness
and a country’s GDP. What the model only did was that it predicted a
lower average GDP values for African countries, which can be shown from
the fact that the regression line for Africa is lower.

Now, instead, lets add a linear model that models the linear interaction
effect between ruggedness and African nations. In short, the
relationship between GDP and ruggedness will be conditional on whether a
country is in Africa or not.

Yi \~ Normal(µi, σ) \[likelihood\] µi = α + γiRi + βAAi \[linear model
of µ\] γi = βR + βARAi \[linear model of slope\]

The third line above is the linear interaction effect between ruggedness
and whether a country is in Africa or not.

Lets model the above linear interaction.

R Code 7.7

``` r
m7.5 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + gamma * rugged + bA * cont_africa,
    gamma <- bR + bAR * cont_africa,
    a ~ dnorm(8, 100),
    bA ~ dnorm(0, 1),
    bR ~ dnorm(0, 1),
    bAR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)
```

Now, lets compare this model using WAIC values with the previous models.

``` r
compare(m7.3, m7.4, m7.5)
```

    ##          WAIC       SE     dWAIC       dSE    pWAIC       weight
    ## m7.5 469.7356 15.07908  0.000000        NA 5.362823 9.721928e-01
    ## m7.4 476.8441 15.33568  7.108519  6.166715 4.638870 2.780719e-02
    ## m7.3 539.6894 13.24913 69.953827 15.163243 2.744424 6.272952e-16

Model 7.5 has a weight of 0.97. However, the weight of 0.03 that is
given to model m7.4 shows that there is slight overfitting happening in
m7.5. Also, the difference in the standard errors between the top two
models is almost the same. This phenonema might also be there because
there are only a limited number of African countries, and hence there is
some sparsity in the data.
