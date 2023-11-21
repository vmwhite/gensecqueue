#' Calculate additional stationary transition probabilities above boundary behavior
#'
#' @param vec <- vector of X_i to add transition probabilities to
#' @param tolerance <- tolerance to stop loop at
#' @param time_limit <- time limit in seconds
#'
#' @return returns expanded transtion probabilty vector
#' @export
#'
#' @examples
#' #' K <- 13
#' r <- 10
#' s <- 5
#' lambda <- .2
#' p <-.9
#' lambda_aux <- (p-1) * lambda
#' lambda_p <- p * lambda
#' mu_p <-.4
#' mu_aux <-.5
#' A <- calc_A_k(K,s,r,lambda,lambda_aux,lambda_p,mu_p,mu_aux, p)
#' R <- calc_R(A, K,s)
#' B <-  calc_B_ki(K,s,r,lambda,lambda_aux,lambda_p,mu_p,mu_aux, p)
#' vec <- calc_X(K,s,r, A,B,R)
#' normalize_vector(vec, matrix_size, R)
normalize_vector <- function(vec, matrix_size, R, tolerance, x_i_thres ) {
  # Get the start time
  start_time <- Sys.time()
  new_X <- 100
  while ( 1 - sum(vec, na.rm=TRUE) > tolerance &&  all(new_X < x_i_thres )) {
    new_X <- t(tail(vec,(matrix_size))) %*% R
    new_X[is.nan(new_X)] <- 0
    for (i in 1:ncol(new_X)){
      vec <- rbind(vec, new_X[i] )
    }
  }
  return(vec)
}
