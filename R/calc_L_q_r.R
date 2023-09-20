#' Calculate the steady-state number of customers in the "restricted queue"
#'
#' @param K :=truncation parameter
#' @param m := number of restricted servers
#' @param prob_vec := state probability matrix
#'
#' @return L_q_r value
#' @export
#'
#' @examples
#' K <- 14
#' m <- 2
#' prob_vec <- []
#' calc_L_q_r(K,m,prob_vec)
calc_L_q_r <- function(K,m,prob_vec){
  size <- length(prob_vec)
  L_q_r <- 0
  for(i in (m+1):(size-1)){
    for(j in (n):(K)){
      #add one to for loops since R indexes at one
      val <- (i - m)*p[i+1,j+1]
      L_q_r <-  L_q_r + val
    }
  }
return(L_q_r)
}