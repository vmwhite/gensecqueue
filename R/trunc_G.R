#' Full Generator matrix
#'
#' @param K := truncation parameter
#' @param m := number of restricted servers
#' @param n := number of general servers
#' @param A :=  A_m matrix that represents non-boundary behavior
#' @param B := matrix B of the states with boundary behavior
#' @param R := R matrix
#'
#' @return Full Generator matrix
#' @export
#'
#' @examples
#' K <- 13
#' r <- 10
#' s <- 5
#' lambda <- .2
#' p <-.9
#' lambda_aux <- (p-1) * lambda
#' lambda_p <- p * lambda
#' mu_p <-.4
#' mu_aux <-.5
#' A <- Calc_Am(K,s,r,lambda,lambda_aux,lambda_p,mu_p,mu_aux, p)
#' R <- Calc_R(A, K,s)
#' B <-  Calc_Bmn(K,s,r,lambda,lambda_aux,lambda_p,mu_p,mu_aux, p)
#' trunc_G(K,s,r, A,B,R)
trunc_G <- function(K,n,m, A,B,R) {
  #Calculate the truncated r by r Generator matrix as a_irow_icol_jrow_jcol
  matrix_size <- (K) + 1
  aij <- matrix(0, nrow=1,ncol=matrix_size)
  a_irow_icol_jrow_jcol <- array(c(aij), c(m+1, m+1, matrix_size, matrix_size))
  inner_col2 <- 0
  A_count <- 0

  for(i in (0:m-1)){
    for (j in (0:m)){
      k <- i - j + 1
      if (m >= 0 && k >= 0){
        a_irow_icol_jrow_jcol[i+1,j+1,,] <- B[k+1,i+1,,]
      }
    }
  }
  ### summation for final row
  row <- m+1
  inner_col <- 1
  A_count <- 0
  for (col in 1:(m+1)){
    r_row_sum <- 0
    ## first summation
    if (col == (m-n-1)+1){
      if (K-2*n > 0){
        for (i in 0:(K-2*n)){
          r_row_sum <- r_row_sum + (matrix_power(R,i) %*% B[(n+i+2)+1,(m+i)+1,,])
        }
      }else if (K-2*n == 0){
        r_row_sum <- r_row_sum + (matrix_power(R,0) %*% B[(n+0+2)+1,(m+0)+1,,])
      }
      ## last summation
    }else if(col == (m)+1 && (K-n+2-1) >= 0){
      for (i in 0:(K-n+2-1)){
        r_row_sum <- r_row_sum + (matrix_power(R,i) %*% A[(i+1)+1,,])
      }
      ### (r-s-1) + 1 to (r) + 1 summation ####
    }else if (col < ((m) +1) && (col>((m-n-1)+1) )){
      ## sum of the B's
      B_count <- max(K-n+1 + 1 - A_count, 2+1)
      if (B_count == 3){
        if (K-2*n == 0){
          r_row_sum <- r_row_sum + (matrix_power(R,0) %*% B[2+1,(m+0)+1,,])
        }else if (K-2*n > 0){
          for (i in 0:(K-2*n)){
            r_row_sum <- r_row_sum + (matrix_power(R,i) %*% B[2+1,(m+i)+1,,])
          }
        }
      }else{
        if (K-2*n == 0){
          r_row_sum <- r_row_sum + (matrix_power(R,0) %*% B[(n+0+1)+1,(m+0)+1,,])
        }else if (K-2*n > 0){
          for (i in 0:(K-2*n)){
            r_row_sum <- r_row_sum + (matrix_power(R,i) %*% B[(n+i+1)+1,(m+i)+1,,])
          }
        }
      }
      ## sum the A's
      max <- (K-n-m)
      if (((K-n-m) <= (K-2*n +1)) && A_count == 0 && (K-n+2) >= 0 ){
        r_row_sum <- r_row_sum + (matrix_power(R,(K-2*n+1)) %*% A[(K-n+2)+1,,])
      }else if ((A_count) == 0 && (K-n+2) >= 0){
        r_row_sum <- r_row_sum + (matrix_power(R,(K-2*n+1)) %*% A[(K-n+2)+1,,])
      }else if ((K-n+2) >= 0){
        for (i in (K-2*n +1):(K-2*n +1 + A_count)){
          if (i <= K-n-m){
            r_row_sum <- r_row_sum + (matrix_power(R,i) %*% A[(m+i +2)+1,,])
          }
        }
      }
      A_count <- A_count + 1
    }
    a_irow_icol_jrow_jcol[row,col,,] <- r_row_sum
  }
  ### restructure the matrix to solve
  # Reshape the matrix to a 2-dimensional matrix of size (m+1)*matrix_size by (m+1)*matrix_size
  # Assuming r and matrix_size are already defined
  G <- matrix(0, nrow = ((m + 1)*matrix_size), ncol = ((m + 1)*matrix_size ))
  # Calculate the number of rows and columns in a_irow_icol_jrow_jcol
  G_rows <- nrow(a_irow_icol_jrow_jcol)
  G_cols <- ncol(a_irow_icol_jrow_jcol)

  # Calculate the number of blocks in the reshaped matrix
  blocks_rows <- matrix_size
  blocks_cols <-  matrix_size

  for (i in 1:(m+1)) {
    for (j in 1:(m+1)) {
      # Calculate the indices for block assignment
      start_row <- (i - 1) * blocks_rows + 1
      end_row <- i * blocks_rows
      start_col <- (j - 1) * blocks_cols + 1
      end_col <- j * blocks_cols

      # Assign the block from a_irow_icol_jrow_jcol to a
      G[start_row:end_row, start_col:end_col] <- a_irow_icol_jrow_jcol[i, j, , ]
    }
  }
  return(G)
}
