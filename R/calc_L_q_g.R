#' Calculate the steady-state number of customers in the "general queue"
#'
#' @param K :=truncation parameter
#' @param n := number of general servers
#' @param prob_vec := state probability matrix
#'
#' @return L_q_g value
#' @export
#'
#' @examples
#' K <- 14
#' n <- 2
#' prob_vec <- []
#' calc_L_q_r(K,m,prob_vec)
calc_L_q_r <- function(K,m,prob_vec){
  size <- length(prob_vec)
  L_q_r <- 0
  for(i in (0):(size-1)){
    for(j in (n+1):(K)){
      #add one to for loops since R indexes at one
      val <- (j - n)*p[i+1,j+1]
      L_q_r <-  L_q_r + val
    }
  }
  return(L_q_r)
}
