#' Greedy Algorithm
#'
#'  This function uses greedy algorithm to solve knapsack problem by calculating 
#'  the unit weight value of each item and sort the items according to this value 
#'  to choose the item with high value.
#'  
#' @param x data.frame where each row represents an item with its weight and 
#'   value.
#' @param W A numeric value representing maximum capacity of knapsack.
#'
#' @return final  A list shows the maximum knapsack value and the elements.
#' 
#' @export
#' 
#' @examples
#' items <- data.frame(weight = c(2, 3, 4), value = c(3, 4, 5))
#' W <- 5
#' greedy_knapsack(items, W)
#' 
greedy_knapsack <-
function(x,W){
  if(is.data.frame(x)==FALSE){
    stop()
  }
  if(W<0){
    stop()
  }
  n_row <- nrow(x)
  rownames(x) <- c(1:n_row) #提前定好名字，因为重新排序后难定位
  vm <- x[,2]/x[,1] #求单位重量的价值
  new_x <- cbind(x,vm) #新矩阵
  sort_x <- new_x[order(new_x[,3],decreasing = TRUE),] #按vm排序
  weight <- 0
  value <- 0
  elements <- c()
  for (i in 1:nrow(sort_x)) {
    if((weight+sort_x[i,1])<=W){
      weight <- weight+sort_x[i,1]
      value <- value+sort_x[i,2]
      elements <- c(elements,rownames(sort_x[i,]))
    }
    else {
      break
    }
  }
  final <- list(
    value = round(value),
    elements = as.numeric(elements)
  )
  return(final)
}


