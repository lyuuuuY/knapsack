#' Generate Knapsack Objects
#'
#'  This function generates a data frame of knapsack objects with random weights
#'  and values.
#' 
#' @importFrom stats runif
#'
#' @param n An integer specifying the number of objects to generate.
#'
#' @return knapsack_objects A data frame containing the generated knapsack 
#'   objects with columns for weight (`w`) and value (`v`).
#'   
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

