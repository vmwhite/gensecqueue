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
solve_K <- function(m,n,lambda,mu_g,mu_r, p, N=m) {
  lambda_r <- lambda*p
  lambda_g <- lambda*(1-p)
  N <- max(m, n) # K > m+1, starting N will be m+1 if m is max
  # initalizing
  N_r <- 100
  N_g <- 100
  L_r_N_val <- 1
  L_g_N_val <- 1
  L_r_N_1 <- .0001
  L_g_N_1 <-.0001

  while ( (L_r_N_val > 0.02 || L_g_N_val> 0.02) && N < 14 ){
    N <- N + 1
    # caclulate input parameters
    A <- calc_A_k(N,m,n,lambda,lambda_r,lambda_g,mu_r,mu_g, p)
    R <- calc_R(A,N,n)
    B <- calc_B_ki(N,m,n,lambda,lambda_r,lambda_g,mu_r,mu_g, p)
    prob_vec <- calc_X(N,m,n,A,B,R)

    #calculate new vaules and difference
    matrix_size <- N+1
    size <- length(prob_vec) / matrix_size

    #calc p_K
    p_K <- calc_p_K(m,n,prob_vec,size,N)

    #calc L_r
    if (L_r_N_val <= 0.02){
      N_r <- min(N-1,N_r)
      L_r <- L_r_N_1
    }else{
      L_r <- calc_L_r(m,n,prob_vec,size,N)
    }

    #calc L_g
    if (L_g_N_val <= 0.02){
      N_g <- min(N-1,N_g)
      L_g <- L_g_N_1
    }else{
      L_g <- calc_L_g(m,n,prob_vec,size,N)
    }

    #calculate difference
    L_r_N_val <- abs(L_r_N_1 - L_r) / L_r
    L_g_N_val <- abs(L_g_N_1 - L_g) / L_g
    #print(prob_vec)
    print(paste0("N = ", N," , sum prob_vec = ", round(sum(prob_vec),2)," , p_K = ", round(p_K,4), ", L_r_diff = ", round(L_r_N_val,2), ", L_g_diff = ", round(L_g_N_val,2), " , L_r = ", round(L_r,2) , "  at K = ", min(N,N_r), " L_g = ", round(L_g,2)
                 , "  at K = ", min(N,N_g) ))

    # reset last values
    L_r_N_1 <- L_r
    L_g_N_1 <- L_g
  }


    K <- N
return(K)
}
