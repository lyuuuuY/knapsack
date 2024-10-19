#' Dynamic Programming
#'
#'  This function solves the knapsack problem using a a two-dimensional dynamic 
#'  programming approach. It returns the maximum value that can be achieved 
#'  within the weight limit and the indices of the selected items.
#'
#' @param x data.frame where each row represents an item with its weight and value.
#' @param W A numeric value representing maximum capacity of knapsack.
#'
#' @return final A list shows the maximum knapsack value and the elements.
#' 
#' @export
#' 
#' @examples
#'  items <- data.frame(weight = c(2, 3, 4), value = c(3, 4, 5))
#'  W <- 5
#'  Dynamic(items, W)
#'
Dynamic <-
function(x,W){
  if(is.data.frame(x)==FALSE){
    stop()
  }
  if(W<0){
    stop()
  }
  elements <- c() 
  dp <- matrix(0,nrow = nrow(x)+1, ncol = W+1) 
  new_x <- rbind(c(0,0),x) 
  
  for (i in 2:nrow(dp)) {
    for (j in 2:ncol(dp)) {
      if(new_x[i,1]<=j){                                                    
        dp[i,j] <- max(dp[i-1,j], dp[i - 1, j - new_x[i, 1]] + new_x[i, 2]) 
      }
      else{
        dp[i,j] <- dp[i-1,j]
      }
    }
  }  
  m <- nrow(new_x) 
  col_dp <- W+1
  while (m>0) {
    b <- min(which(dp == max(dp[1:m,col_dp]), arr.ind = TRUE)[, "row"])    
    if((b-1)>0){
      elements <- c(elements,b-1)
    }
    m <- b-1
    col_dp <- col_dp-x[b-1,1]
  }  
  final <- list(
    value = round(max(dp)),
    elements = elements
  )
  return(final)
}



