#' Solve for stationary probability vector X
#'
#' @param K := truncation parameter
#' @param n := number of general servers
#' @param m := number of restricted servers
#' @param A :=  A_m matrix that represents non-boundary behavior
#' @param B := matrix B of the states with boundary behavior
#' @param m := m matrix
#'
#' @return stationary probability vector X
#' @export
#'
#' @examples
#' K <- 13
#' m <- 10
#' n <- 5
#' lambda <- .2
#' p <-.9
#' lambda_aux <- (p-1) * lambda
#' lambda_p <- p * lambda
#' mu_p <-.4
#' mu_aux <-.5
#' A <- Calc_Am(K,s,r,lambda,lambda_aux,lambda_p,mu_p,mu_aux, p)
#' m <- Calc_R(A, K,s)
#' B <-  Calc_Bmn(K,s,r,lambda,lambda_aux,lambda_p,mu_p,mu_aux, p)
#' X_i <- Calc_X(K,s,r, A,B,R)
calc_X <- function(K,m,n, A,B,R, tolerance = 0.00001, x_i_thres = 0.00001){
  #Calculate the truncated m by m Generator matrix
  G <- trunc_G(K,n,m, A,B,R)
  #transpose G to turn into a system of equations
  G_trans <- t(G)
  #Add in total probability equation
  G_trans <-last_row_G(G_trans, K, R,m)

  #RHS
  matrix_size <- K + 1
  b <- matrix(0, nrow=((m+1)*matrix_size),ncol=1)
  b <- rbind(b,1)


  # Solve over-defined system
  X <- solve_prob_matrix(G_trans,b)

  # add additional rows to X
  X <- normalize_vector(X,matrix_size,R, tolerance,x_i_thres)

  ## reformat to x_ij
  X_i <- reformat_X(X, matrix_size)
  return(X_i)
}
