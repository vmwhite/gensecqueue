#' Calculate the probability a type G customer is delayed while a restricted server is free
#' @param m := number of restricted servers
#' @param n := the number of general use servers
#' @param prob_vec := state probability matrix
#'
#' @return probability there is a delay of a general customer when a restricted server is free
#' @export
#'
#' @examples
calc_alpha <- function(m,n,prob_vec,K){
  size <- length(prob_vec)
  alpha<- 0

  for (i in (0):(m)){
    for(j in (n):(K)){
      ## add one since R indexes at 1
      alpha<- alpha + prob_vec[i+1,j+1]
    }
  }
return(alpha)
}
