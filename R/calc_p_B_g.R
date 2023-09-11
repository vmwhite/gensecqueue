#' Calculate The probability that a type G customer has a positive delay
#'
#' @param n := the number of general use servers
#' @param K := truncation parameter
#' @param prob_vec := state probability matrix
#'
#' @return p_B_g value := the probability all G servers are busy
#' @export
#'
#' @examples
calc_p_B_g <- function(K,n,prob_vec){
  size <- length(prob_vec)
  p_B_g<- 0

  for (i in (0):(size-1)){
    for(j in (n):(K)){
      #add one since R indexes at 1
      p_B_g<- p_B_g + p[i+1,j+1]
    }
  }
}
