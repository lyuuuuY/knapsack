#' Title
#'
#' @param x 
#' @param W 
#' @param parallel 
#' @import future.apply
#' @import future
#'
#' @return final
#' @export
#'
brute_force_knapsack <-
function(x,W,parallel=FALSE){
  if(is.data.frame(x)==FALSE){
    stop()
  }
  if(W<0){
    stop()
  }
  
  plan(multisession, workers = 8)  
  options(future.globals.maxSize = 1024 * 1024 * 1024)  # 1GB
  
  item <- nrow(x) #获取物品数量
  n_combinations <- 2^item 

  chunk_size <- 260000
  split_bits <- split(1:n_combinations, ceiling(seq_along(1:n_combinations) / chunk_size))
  
  if(parallel==TRUE){ 
    
    calculation <- function(row){
      final_v <- 0
      final_elements <- NULL
      
      for (i in row) {
        bits_chunk <- as.integer(intToBits(i)[1:item])
        temp_w <- as.numeric(bits_chunk %*% x[, 1])
        temp_v <- as.numeric(bits_chunk %*% x[, 2])
        if (temp_w <= W && temp_v >= final_v) {
          final_v <- temp_v
          final_elements <- which(bits_chunk == 1)
        }
      }
      return(list(value = final_v, elements = final_elements))
    }
    results <- future_lapply(split_bits, calculation,future.globals = list(x = x, W = W, item = item))
    final_v <- 0
    final_elements <- NULL
    for (res in results) {
      if (res$value > final_v) {
        final_v <- res$value
        final_elements <- res$elements
      }
    }
    
    return(list(value = final_v, elements = final_elements))
  }     
  else{
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
}
