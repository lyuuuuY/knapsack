#' Optimized Greedy Algorithm
#'
#'  This function solves the knapsack problem using an optimized greedy algorithm 
#'  by reducing Unnecessary Variables and matrices.
#' 
#' @param x data.frame data.frame where each row represents an item with its 
#'   weight and value.
#' @param W A numeric value representing maximum capacity of knapsack.
#'
#' @return final A list shows the maximum knapsack value and the elements.
#' 
#' @export
#' 
#' @examples
#' items <- data.frame(weight = c(2, 3, 4), value = c(3, 4, 5))
#' W <- 5
#' greedy_knapsack_v2(items, W)
#'
greedy_knapsack_v2 <-
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
    x$vm <- vm  # 直接在数据框中添加新列
    sort_x <- x[order(x$vm, decreasing = TRUE), ] #按vm排序
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

