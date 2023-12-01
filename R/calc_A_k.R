#' Calculating A matrices
#'
#' @param K := truncation parameter
#' @param m := number of restricted servers
#' @param n := number of general servers
#' @param lam := arrival rate of all customers
#' @param lam_R := arrival rate of customers of type restricted
#' @param lam_G := arrival rate of customers of type general
#' @param mu_G := general server rate
#' @param mu_R := restricted server rate
#' @param p := percentage of customers that are of type restricted
#'
#' @return matrix B of the states with boundary behavior
#' @export
#'
#' @examples
#' #' K <- 13
#' m <- 10
#' n <- 5
#' lambda <- .2
#' p <-.9
#' lambda_aux <- (p-1) * lambda
#' lambda_p <- p * lambda
#' mu_g <-.4
#' mu_r <-.5
calc_A_k <- function(K,m,n,lam,lam_R,lam_G,mu_R,mu_G, p){
  q <- 1 - p
  # + 1 since r indexes at 1
  matrix_size <- (K) + 1
  k_amax <- max((K-n+2)+1,n+1)
  A_k <- matrix(0, nrow=matrix_size, ncol=matrix_size)
  A <- array(c(A_k), c(k_amax, matrix_size, matrix_size))
  for (i in matrix_size:2*matrix_size){
    for (k in 1:k_amax){
      ii_two <- ((i-1) - (k -1)+ 1) + 1
      for (j in 1:matrix_size){
        #### if all R and G servers are busy #####
        if((i-1) >= m && (j-1) >= n){
          ## transition from state  (i,) to (i+1,)
          if( ii_two  == i + 1 ){
            A[k,j,j] = lam
          }
          ### AND NO restricted queue ###
          if( (i-1) == m ){
            ## transition from state  (i,) to (i,)
            if ( ii_two == i ){
              A[k,j,j-1] = n*mu_G
              ## transition from state  (i,) to (i-1,)
            } else if ( ii_two == i- 1){
              A[k,j,j] = m*mu_R
            }
            ### AND Restricted queue but NO General queue ###
          }else if( (i-1)> m && (j-1)==n ){
            ## transition from state  (i,) to (i-1,j)
            if ( ii_two == i- 1){
              A[k,j,j] = n*mu_G + m*mu_R*p
            }
            ## transition from (i,j) to  (i - kay -1, j+kay), kay ==0 is to (i-1,j) state
            for (kay in 1:((i-1)-m)){
              if ( ii_two == i- kay - 1){
                # if R in queue
                if(kay < ((i - 1)-m) && (j-1+kay) < K){
                  A[k,j,j+kay] = m*mu_R*(q^kay)*p
                  # if no R in queue
                }else if (kay == ((i-1) - m) || (j-1+kay) == K){
                  A[k,j,j+kay] = (m)*mu_R*q^kay
                }
              }
            }
            ### AND Restricted queue AND General queue#####
          }else if( ((i-1)>m) && ((j-1)>n) ){
            ## transition from state  (i,j) to (i,j-1)
            if ( ii_two == i){
              A[k,j,j-1] = n*mu_G
              ## transition from state  (i,j) to (i-1,j)
            }else if ( ii_two == (i -1)) {
              if (j == K + 1){ #truncated
                A[k,j,j] = m*mu_R
              }else{
                A[k,j,j] = m*mu_R*p
              }
            }
            ## transition from (i,j) to  (i - kay -1, j+kay), kay > 0 since there is Restricted queue
            for (kay in 1:((i-1)-m)){
              if (ii_two == i- kay - 1) {
                if( kay < (i-1)-m && (j+ kay) <= K ){
                  A[k,j,j+kay] = (m)*mu_R*q^kay*p
                }else if ( kay == (i-1)-m && (j + kay) <= K){
                  A[k,j,j+kay] =  (m)*mu_R*q^kay
                }else if((j-1+kay) - K == 0){
                  ## type change
                  A[k,j,K+1] =  (m)*mu_R*q^kay
                }
              }
            }
          }
        }
      }
    }
  }


  ## get negative values
  k_2 = 2
  for (row in 1:matrix_size){
    for (col in 1:matrix_size){
      if (row == col){
        for (k in 1:k_amax){
          for (col_2 in 1:matrix_size){
            if ((k_2!= k || col!=col_2)){
              A[k_2,row,col] =  A[k_2,row,col]-A[k,row,col_2]

            }
          }
        }
      }
    }
  }
  return(A)
}
