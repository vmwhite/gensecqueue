#' Calculate the probability a type G customer is delayed while a restricted server is free
#' @param m := number of restricted servers
#' @param n := the number of general use servers
#' @param prob_vec := state probability matrix
#'
#' @return p_B_r value
#' @export
#'
#' @examples
calc_alpha <- function(m,n,prob_vec){
  size <- length(prob_vec)
  alpha<- 0

  for (i in (0):(m)){
    for(j in (n):(K)){
      ## add one since R indexes at 1
      alpha<- alpha + p[i+1,j+1]
    }
  }
}
