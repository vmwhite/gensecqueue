#' Calculate the probability that a type R customer has a positive delay
#'
#' @param m := number of restricted servers
#' @param n := the number of general use servers
#' @param K := truncation parameter
#' @param prob_vec := state probability matrix
#'
#' @return p_B_r value
#' @export
#'
#' @examples
calc_p_B_r <- function(m,n,K,prob_vec){
  matrix_size <- K + 1
  size <- length(prob_vec) / matrix_size
  p_B_r<- 0

  for (i in (m):(size-1)){
    for(j in (n):(K)){
      ## add one since R indexes at 1
      p_B_r<- p_B_r + prob_vec[i+1,j+1]
    }
  }
  return(p_B_r)
}
