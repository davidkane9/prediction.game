
<!-- README.md is generated from README.Rmd. Please edit that file -->
prediction.game
===============

<!-- badges: start -->
<!-- badges: end -->
The goal of prediction.game is to provide a function for the interactive testing --- as a demonstration during class --- of competing answers to a statistical question.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("davidkane9/prediction.game")
```

Example
-------

This is a basic example. The data is a vector of length 10. The contest is to pick a number which is closest to the result of a random sample (of size 1) from that data. One person guesses 3. Another person guesses 7. We run the context 5 times. In the example, each guess "won" twice and there was one tie.

``` r
library(prediction.game)

set.seed(10)

play(data = 1:10, n = 5, guess_1 = 3, guess_2 = 7, sample, size = 1)
#> # A tibble: 5 x 4
#>   guess_1 guess_2 answer winner 
#>     <dbl>   <dbl>  <dbl> <chr>  
#> 1       3       7      6 guess_2
#> 2       3       7      4 guess_1
#> 3       3       7      5 tie    
#> 4       3       7      7 guess_2
#> 5       3       7      1 guess_1
```
