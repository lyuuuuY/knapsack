
# knapsack


<!-- badges: start -->
<!-- badges: end -->

This package implements three algorithms for solving the knapsack problem: brute force, dynamic programming, and greedy algorithms. 

## Installation

You can install the development version of knapsack from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("lyuuuuY/knapsack")
```

## Example

This is a basic example which shows you how to solve a knapsack problem with three
different approaches.

``` r
  library(knapsack)
  library(future)
  library(future.apply)
  
  #seting an example
  
  items <- data.frame(weight = c(2, 3, 4), value = c(3, 4, 5))
  W <- 5
  
  #Using_brute_force_algorithm
  
  brute_force_knapsack(items, W, parallel = FALSE)
  brute_force_knapsack(items, W, parallel = FALSE)
  
  #Using_dynamic_algorithm
  
  Dynamic(items, W)
  Dynamic_optimized_v1(items, W)
  
  #using_greedy_algorithm
  
  greedy_knapsack(items, W)
  greedy_knapsack_v2(items, W)

```

