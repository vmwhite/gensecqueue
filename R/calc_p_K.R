#' Calculate p_K
#'
#' @param m := number of restricted servers
#' @param n := number of general servers
#' @param prob_vec := state probability matrix
#' @param size := size of probability matrix
#' @param N := Maximum length of general queue
#'
#' @return
#' @export
#'
#' @examples
calc_p_K <- function(m,n,prob_vec,size, N){
  p_K <- 0
  # add 1 since R indexes at 1
  j <- N+1
  ########
  for(i in (1):(size)){
      val <- prob_vec[i,j]
      p_K <- p_K + val
  }
  return(p_K)
}
