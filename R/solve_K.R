#' Solve for Truncation parameter K ######
#' @param lambda := arrival rate of all customers
#' @param n := the number of general use servers
#' @param m := the number of restricted servers
#' @param p := the probability a customer requires restricted service
#' @param mu_g := the service rate of general servers
#' @param mu_r := the service rate of restricted server
#' @param N := temporary starting truncation parameter
#'
#' @return final K truncation parameter with key metric stability
#' @export
#'
#' @examples
#' r <- 10
#' s <- 5
#' lambda <- .2
#' p <-.9
#' lambda_aux <- (p-1) * lambda
#' lambda_p <- p * lambda
#' mu_p <-.4
#' mu_aux <-.5
#' K <- Solve_K(s,r,lambda,lambda_aux,lambda_p,mu_p,mu_aux, p, N = s+1)
Solve_K <- function(m,n,lambda,mu_g,mu_r, p, N) {
  lambda_r <- lambda*p
  lambda_g <- lambda*(1-p)
  N <- max(m+1, 11, N) # K > s
  L_r_N_val <- 1
  L_g_N_val <- 1
  L_r_N_1 <- .0001
  L_g_N_1 <-.0001

  while ( L_r_N_val > 0.02 || L_g_N_val> 0.02  ){
    N <- N + 1
    # caclulate input parameters
    A <- calc_A_k(N,m,n,lambda,lambda_r,lambda_g,mu_r,mu_g, p)
    R <- calc_R(A,N,n)
    B <- calc_B_ki(N,m,n,lambda,lambda_r,lambda_g,mu_r,mu_g, p)
    prob_vec <- calc_X(N,m,n,A,B,R)

    #calculate new vaules and difference
    matrix_size <- N+1
    size <- length(prob_vec) / matrix_size
    #calc L_r and L_q
    L_r <- 0
    L_g <- 0
    if (m+1+1 > (size)){
      for(i in (1):(m+1)){
        for(j in (1):(N+1)){
          #add one to for loops since R indexes at one
          val <- (i-1)*prob_vec[i,j]
          L_r <-  L_r + val
          val_j <- ((j-1))*prob_vec[i,j]
          L_g <- L_g + val_j
        }
      }
    }else{
      for(i in (m+1+1):(size)){
        for(j in (n+1):(N+1)){
          #add one to for loops since R indexes at one
          val <- ((i-1))*prob_vec[i,j]
          L_r <-  L_r + val
          val_j <- ((j-1))*prob_vec[i,j]
          L_g <- L_g + val_j
        }
      }
      for(i in (1):(m+1)){
        for(j in (1):(N+1)){
          #add one to for loops since R indexes at one
          val <- (i-1)*prob_vec[i,j]
          L_r <-  L_r + val
          val_j <- ((j-1))*prob_vec[i,j]
          L_g <- L_g + val_j
        }
      }
    }

    #calculate difference
    L_r_N_val <- abs(L_r_N_1 - L_r) / L_r
    L_g_N_val <- abs(L_g_N_1 - L_g) / L_g

    print(paste0("N = ", N, ", L_r_diff =", L_r_N_val, ", L_g_diff = ", L_g_N_val))
    # reset last values
    L_r_N_1 <- L_r
    L_g_N_1 <- L_g
  }
    K <- N
return(K)
}
