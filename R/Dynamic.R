#' Dynamic 
#'
#' @param x data.frame cx with two variables v and w
#' @param W Weight
#'
#' @return final
#' @export
#'
Dynamic <-
function(x,W){
  if(is.data.frame(x)==FALSE){
    stop()
  }
  if(W<0){
    stop()
  }
  elements <- c() #初始化elements
  dp <- matrix(0,nrow = nrow(x)+1, ncol = W+1) #设定dp矩阵，行列多加1方便逻辑运算
  new_x <- rbind(c(0,0),x) #同理存放物品的矩阵也加一行（0，0）方便运算

  
  # i循环：以行为单位，即以每个物品为单位
  # j循环：以1——W为单位，遍历循环判断能不能装入当前重量的背包里
  for (i in 2:nrow(dp)) {
    for (j in 2:ncol(dp)) {
      if(new_x[i,1]<=j){                                                    #bottleneck1
        dp[i,j] <- max(dp[i-1,j], dp[i - 1, j - new_x[i, 1]] + new_x[i, 2]) #bottleneck2
      }
      else{
        dp[i,j] <- dp[i-1,j]
      }
    }
    
  }  
  m <- nrow(new_x) 
  col_dp <- W+1
  while (m>0) {
    b <- min(which(dp == max(dp[1:m,col_dp]), arr.ind = TRUE)[, "row"])    #bottleneck3
    if((b-1)>0){
      elements <- c(elements,b-1)
    }
    m <- b-1
    col_dp <- col_dp-x[b-1,1]
  }  #有点难解释，可以运行一下看看输出的dp矩阵是什么样子，倒推求elements
  final <- list(
    value = round(max(dp)),
    elements = elements
  )
  return(final)
}





