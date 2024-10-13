#' Title
#'
#' @param x 
#' @param W 
#'
#' @return final
#' @export
#'
brute_force_knapsack <-
function(x,W){
  if(is.data.frame(x)==FALSE){
    stop()
  }
  if(W<0){
    stop()
  }
  item <- nrow(x) #获取物品数量
  bits <- list()
  for (i in 0:(2^item - 1)) {
    bits_0 <- as.integer(head(intToBits(i), item))
    bits[[i + 1]] <- bits_0
  }   #将2^n种组合以二进制方式表示
  bits_mt <- do.call(rbind, bits) #变成矩阵
  
  final_v <- 0 #初始化value

  for (j in 1:nrow(bits_mt)) {
    temp_w <- as.numeric(bits_mt[j,]%*%x[,1])  
    temp_v <- as.numeric(bits_mt[j,]%*%x[,2])  
    
    if(temp_w<=W && temp_v>=final_v ){
      final_v <- temp_v
      elements <- which(bits_mt[j,]==1) 
    }
  }       #计算每种组合的W和V，比较取最优
  final <- list(
    value = final_v,
    elements = elements
  )
return(final)
}
