#' Append last row to truncated G matrix
#'
#' @param G := truncated Generator matrix
#' @param K := max queue length
#' @param R := intermediate matrix
#' @param m := number of servers
#'
#' @return appended truncated generator matrix
#' @export
#'
#' @examples
#' G <- matrix
#' K <- 10
#' R <- matrix
#' m <- 3
#' G_final <- last_row_G(G, K, R, m)
last_row_G <- function(G, K, R, m){

  # dummy x variable
  matrix_size <- K + 1
  X_m <- matrix(1, nrow = 1, ncol=matrix_size)

  # Add in final equation as a new row in G
  row_list <- c()
  I <- diag(1,matrix_size)
  a_Xm <- X_m %*% t(solve(I - R)) # transpose so that the variable are aligned


  ## append final equation
  #x_0*e + x_1*e +.. x_m(I-R)^{-1}*e = 1
  for (i in 1:((m + 1) * matrix_size)){
    if (i <(((m * matrix_size)+1))){
      # e is the column vector with all its components equal to 1
      row_list<- append(row_list, 1)
      num <- 1
    }else{
      row_list<- append(row_list, a_Xm[num])
      num <- num+1
    }
  }

  G <- rbind(G,row_list)


  return(new_G)
}
