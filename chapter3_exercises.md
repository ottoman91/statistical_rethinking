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
