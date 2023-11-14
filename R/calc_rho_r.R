#' Calculate The probability tbat a type R customer has a positive delay
#'
#' @param m := number of restricted servers
#' @param n := the number of general use servers
#' @param p_B_R := probability of delay for R customers
#' @param p_B_G := probability of delay for G customers
#' @param mu_R := Restricted server service rate
#' @param mu_G := General server service rate
#' @param lam_r := arrival rate of R customers
#'
#' @return Utilization of restricted servers
#' @export
#'
#' @examples
calc_rho_r <- function( K, p, n, m, p_B_R, p_B_G, mu_R, mu_G, lam_r, prob_vec){
  rho_r <- 0
  #probability service of restricted calls when at least one R server is free
  p_1 <- (1-p_B_R)
  #probability when there is a restricted queue and all servers are busy
  p_2 <- (p_B_R *p_B_G)
    # probability R finishes before G
    R_fin <- (m*mu_R) / (m*mu_R + n*mu_G)
    # probability G finishes before R
    G_fin<- (n*mu_G) / (m*mu_R + n*mu_G)
    #summation in ith spot in queue and R finishes before ith general servers.max restricted queue length is K-n
    p_3 <- 0
    matrix_size <- K+1
    size <- (length(prob_vec)/matrix_size)-(m+1)
    if (size > 0) {
    for(i in 1:(size)){
      for(j in 1:matrix_size){
          prob_length <- prob_vec[i+m+1,j]
      }
      p_3 <- p_3 + (prob_length*R_fin*(G_fin^(i-1)))
    }

    }
  #summing it all together
    rho_r <- (lam_r*(p_1 + p_2*p_3)) /  (m*mu_R)
  return (rho_r)
}

