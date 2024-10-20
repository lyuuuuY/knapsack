#' Optimized Dynamic Programming
#'
#'  This function solves the knapsack problem using an optimized dynamic 
#'  programming approach.It returns the maximum value that can be achieved 
#'  within the weight limit and the indices of the selected items.
#'
#' @param x data.frame where each row represents an item with its weight and value.
#' @param W A numeric value representing maximum capacity of knapsack.
#'
#' @return final A list shows the maximum knapsack value and the elements.
#' 
#' @export
#'
#' 
#' @examples
#'  items <- data.frame(weight = c(2, 3, 4), value = c(3, 4, 5))
#'  W <- 5
#'  Dynamic_optimized_v1(items, W)
#' 
#' 
Dynamic_optimized_v1 <-
  function(x,W){
    if(is.data.frame(x)==FALSE){
      stop()
    }
    if(W<0){
      stop()
    }
    elements <- c() 
    dp <- rep(0, W + 1)
    keep <- matrix(0, nrow = nrow(x) + 1, ncol = W + 1)
    new_x <- rbind(c(0,0),x) 
    
    
   
    for (i in 1:nrow(x)) {
      weight <- x[i, 1]
      value <- x[i, 2]
      if (weight <= W) {
        for (j in seq(W, weight, by = -1)) {
          if (dp[j + 1] < dp[j - weight + 1] + value) {
            dp[j + 1] <- dp[j - weight + 1] + value
            keep[i + 1, j + 1] <- 1 
          }
        }
      }
    }
    max_value <- round(dp[W + 1])
    
    col_dp <- W + 1
    for (i in seq(nrow(x), 1, by = -1)) {
      if (keep[i + 1, col_dp] == 1) { 
        elements <- c(elements, i) 
        col_dp <- col_dp - x[i, 1] 
      }
    }
    final <- list(
      value = max_value,
      elements = elements
    )
    return(final)
  }



