#' Title
#'
#' @param x 
#' @param W 
#'
#' @return final
#' @export
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


