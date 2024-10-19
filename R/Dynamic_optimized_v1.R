#' Dynamic optimized
#'
#' @param x 
#' @param W 
#'
#' @return
#' @export
#'
#' @examples
Dynamic_2 <- function(x, W) {
  if (is.data.frame(x) == FALSE) {
    stop("Input must be a data frame.")
  }
  if (W < 0) {
    stop("Weight capacity must be non-negative.")
  }
  elements <- integer(0) # 初始化elements
  dp <- matrix(0, nrow = nrow(x) + 1, ncol = W + 1) # 设定dp矩阵
  new_x <- rbind(c(0, 0), x) # 添加一个初始物品（0，0）
  
  # 填充dp矩阵
  for (i in 2:nrow(dp)) {
    for (j in 1:ncol(dp)) { # 修正从1开始遍历列
      if (new_x[i, 1] <= j) { # 检查当前物品是否可以装入背包
        dp[i, j] <- max(dp[i - 1, j], dp[i - 1, j - new_x[i, 1]] + new_x[i, 2])
      } else {
        dp[i, j] <- dp[i - 1, j]
      }
    }
  }
  
  # 反向查找选中的物品
  m <- nrow(new_x)
  col_dp <- W
  
  # 初始化最大值为 dp[m, col_dp]，也就是最优解的值
  current_max_value <- dp[m, col_dp]
  
  while (m > 1 && col_dp > 0) {
    # 如果当前行的最大值和上一行不同，则当前物品被选中
    if (current_max_value != dp[m - 1, col_dp]) {
      elements <- c(elements, m - 1) # 添加被选中的物品索引
      col_dp <- col_dp - new_x[m, 1] # 更新容量
      current_max_value <- dp[m - 1, col_dp] # 更新当前最大值
    }
    m <- m - 1
  }
  
  final <- list(
    value = round(dp[nrow(x) + 1, W]), # 最优解的总价值
    elements = elements # 选中的物品
  )
  return(final)
}





