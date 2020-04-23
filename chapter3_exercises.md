Chapter 3 Exercises
================
Usman Khaliq
2020-04-22

``` r
# Libraries
library(tidyverse)
library(rethinking)
```

``` r
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
```

Use the values in `samples` to answer the questions that follow.

3E1. How much posterior probability lies below p = 0.2?

``` r
sum(samples < 0.2) / 1e4 * 100
```

    ## [1] 0.04

0.04% of posterior probability is below 0.2

3E2. How much posterior probability lies above p = 0.8?

``` r
sum(samples > 0.8) / 1e4 * 100
```

    ## [1] 11.16

11.16% of posterior probability lies above 0.8

3E3. How much posterior probability lies between p = 0.2 and p = 0.8?

``` r
sum(samples > 0.2 & samples < 0.8) / 1e4 * 100
```

    ## [1] 88.8

88.8% of posterior probabilities lie between 0.2 and 0.8

3E4. 20% of the posterior probability lies below which value of p?

``` r
quantile(samples, 0.2)
```

    ##       20% 
    ## 0.5185185

20% of posterior probabilities lie below value of 0.5185185

3E5. 20% of the posterior probability lies above which value of p?

``` r
quantile(samples, 0.8)
```

    ##       80% 
    ## 0.7557558

20% of posterior probabilities lie above value of 0.7557558

3E6. Which values of p contain the narrowest interval equal to 66% of
the posterior probability?

``` r
HPDI(samples, prob = 0.66)
```

    ##     |0.66     0.66| 
    ## 0.5085085 0.7737738

0.5085085 and 0.7737738 contain 60% of the posterior probability values.

3E7. Which values of p contain 66% of the posterior probability,
assuming equal posterior probability both below and above the interval?

``` r
PI(samples, prob = 0.60)
```

    ##       20%       80% 
    ## 0.5185185 0.7557558

0.5185185 and 0.7557558 contain 60% of the posterior probability

3M1. Suppose the globe tossing data had turned out to be 8 water in 15
tosses. Construct the posterior distribution, using grid approximation.
Use the same flat prior as before.

``` r
p_grid_3m1 <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid_3m1)
posterior_3m1 <- prior * likelihood
posterior_3m1 <- posterior_3m1 / sum(posterior_3m1)
plot(posterior_3m1 ~ p_grid_3m1, type = "l")
```

![](chapter3_exercises_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

3M2. Draw 10,000 samples from the grid approximation from above. Then
use the samples to calculate the 90% HPDI for p.

``` r
samples <- 
  sample(
    p_grid_3m1,
    prob = posterior_3m1,
    size = 10000,
    replace = TRUE
  ) 

HPDI(samples, prob = 0.90)
```

    ##      |0.9      0.9| 
    ## 0.3293293 0.7167167

The 90% HPDI for p is betwen 0.3413412 and 0.7307307.

3M3. Construct a posterior predictive check for this model and data.
This means simulate the distribution of samples, averaging over the
posterior uncertainty in p. What is the probability of observing 8 water
in 15 tosses?

``` r
posterior_pred_check <- rbinom(10000, size = 15, prob = samples)
sum(posterior_pred_check == 8) / 10000 
```

    ## [1] 0.1444

The probability of observing 8 water in 15 tosses is 15.06%

3M4. Using the posterior distribution constructed from the new (8/15)
data, now calculate the probability of observing 6 water in 9 tosses.

``` r
posterior_pred_check <- rbinom(10000, size = 9, prob = samples)
sum(posterior_pred_check == 6) / 10000 
```

    ## [1] 0.1751

The probability of observing 6 water in 9 tosses is 17.93%

3M5. Start over at 3M1, but now use a prior that is zero below p = 0.5
and a constant above p = 0.5. This corresponds to prior information that
a majority of the Earth’s surface is water. Repeat each problem above
and compare the inferences. What difference does the better prior make?
If it helps, compare inferences (using both priors) to the true value p
= 0.7.

``` r
p_grid_3m5 <- seq(from = 0, to = 1, length.out = 1000)
prior_3m5 <- ifelse(p_grid_3m5 < 0.5, 0, 1)
likelihood <- dbinom(8, size = 15, prob = p_grid_3m5)
posterior_3m5 <- prior_3m5 * likelihood
posterior_3m5 <- posterior_3m5 / sum(posterior_3m5)
plot(posterior_3m5 ~ posterior_3m5, type = "l")
```

    ## Warning in plot.formula(posterior_3m5 ~ posterior_3m5, type = "l"): the formula
    ## 'posterior_3m5 ~ posterior_3m5' is treated as 'posterior_3m5 ~ 1'

![](chapter3_exercises_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
samples_3m5 <- 
  sample(
    p_grid_3m5,
    prob = posterior_3m5,
    size = 10000,
    replace = TRUE
  ) 

HPDI(samples_3m5, prob = 0.90)
```

    ##      |0.9      0.9| 
    ## 0.5005005 0.7117117

The 90% HPDI for p is betwen 0.5005005 and 0.7127.

``` r
posterior_pred_check_3m5 <- rbinom(10000, size = 15, prob = samples_3m5)
sum(posterior_pred_check_3m5 == 8) / 10000 
```

    ## [1] 0.1589

The probability of observing 8 water in 15 tosses is 15.21%
