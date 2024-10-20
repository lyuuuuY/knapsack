#' Brute Force Search
#'
#'  This function executes brute force search by default to solve knapsack problem,
#'  and used `future` and `future.apply` package to  operate in parallel mode to 
#'  speed up the computation.
#'
#' @param x data.frame where each row represents an item with its weight and value.
#' @param W A numeric value representing maximum capacity of knapsack.
#' @param parallel A logical value indicating whether to use parallel processing. 
#'   Default is FALSE
#'   
#' @import future.apply
#' @import future
#' @importFrom utils head
#' @importFrom stats runif
#'
#' @return final A list shows the maximum knapsack value and the elements.
#' 
#' @export
#' 
#' 
#'   
brute_force_knapsack <-
function(x,W,parallel=FALSE){
  if(is.data.frame(x)==FALSE){
    stop()
  }
  if(W<0){
    stop()
  }
  
  plan(multisession, workers = 2)  
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
    
    return(list(value = round(final_v), elements = final_elements))
  }     
  else{
    bits <- list()
    for (i in 0:(2^item - 1)) {
      bits_0 <- as.integer(head(intToBits(i), item))
      bits[[i + 1]] <- bits_0
    }   
    bits_mt <- do.call(rbind, bits) 
    final_v <- 0 
    
    for (j in 1:nrow(bits_mt)) {
      temp_w <- as.numeric(bits_mt[j,]%*%x[,1])  
      temp_v <- as.numeric(bits_mt[j,]%*%x[,2])  
      
      if(temp_w<=W && temp_v>=final_v ){
        final_v <- temp_v
        elements <- which(bits_mt[j,]==1) 
      }
    }       
    final <- list(
      value = round(final_v),
      elements = elements
    )
    return(final)
  }
}

