Chapter 4
================
Usman Khaliq
2020-04-25

``` r
# Libraries
library(tidyverse)
library(rethinking)
```

R Code 4.1

``` r
pos <- replicate(1000, sum(runif(16, -1, 1)))
hist(pos)
```

![](chapter_4_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
plot(density(pos))
```

![](chapter_4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

R Code 4.2

``` r
prod(1 + runif(12, 0, 0.1))
```

    ## [1] 1.786865

R code 4.3

``` r
growth <- replicate(1000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)
```

![](chapter_4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

R Code 4.4

Small effects that multiple together are approximately additive. Lets
explore this phenomenon visually.

``` r
big <- replicate(1000, prod(1 + runif(12, 0, 0.5)))
small <- replicate(1000, prod(1 + runif(12, 0, 0.01)))
```

``` r
dens(big)
```

![](chapter_4_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
dens(small)
```

![](chapter_4_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

R Code 4.5

Large deviations tend to produce Gaussean distributions at the log scale

``` r
log.big <- replicate(1000, log(prod(1 + runif(12, 0., 0.5))))
dens(log.big)
```

![](chapter_4_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

R Code 4.7

``` r
data("Howell1")
d <- Howell1
glimpse(d)
```

    ## Rows: 544
    ## Columns: 4
    ## $ height <dbl> 151.7650, 139.7000, 136.5250, 156.8450, 145.4150, 163.8300, 14…
    ## $ weight <dbl> 47.82561, 36.48581, 31.86484, 53.04191, 41.27687, 62.99259, 38…
    ## $ age    <dbl> 63.0, 63.0, 65.0, 41.0, 51.0, 35.0, 32.0, 27.0, 19.0, 54.0, 47…
    ## $ male   <int> 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0,…

Filter out heights of adults only from the dataset, since height is
strongly correlated with age before adulthood

``` r
d2 <- 
  d %>% 
  filter(age >= 18) 

glimpse(d2)
```

    ## Rows: 352
    ## Columns: 4
    ## $ height <dbl> 151.7650, 139.7000, 136.5250, 156.8450, 145.4150, 163.8300, 14…
    ## $ weight <dbl> 47.82561, 36.48581, 31.86484, 53.04191, 41.27687, 62.99259, 38…
    ## $ age    <dbl> 63.0, 63.0, 65.0, 41.0, 51.0, 35.0, 32.0, 27.0, 19.0, 54.0, 47…
    ## $ male   <int> 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1,…

``` r
d2 %>% 
  ggplot(aes(height)) +
  geom_density()
```

![](chapter_4_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Adult height seems to be normally distributed, perhaps because height is
a sum of many small growth factors.
