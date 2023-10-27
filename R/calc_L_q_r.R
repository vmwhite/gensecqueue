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
calc_L_q_r <- function(K,m,n,prob_vec){
  matrix_size <- K+1
  size <- length(prob_vec) / matrix_size
  L_q_r <- 0

  if (m+1+1 > (size)){
    L_q_r <- 0
  }else{
    for(i in (m+1+1):(size)){
      for(j in (n+1):(K+1)){
        #add one to for loops since R indexes at one
        val <- ((i-1) - m)*prob_vec[i,j]
        L_q_r <-  L_q_r + val
      }
    }
  }
return(L_q_r)
}
