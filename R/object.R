#' Title
#'
#' @param n 
#'
#' @return knapsack_objects
#' @export
#'
object <-
function(n){
  RNGversion(min(as.character(getRversion()),"3.5.3"))
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=runif(n = n, 0, 10000)
    )
  invisible(knapsack_objects)
}
