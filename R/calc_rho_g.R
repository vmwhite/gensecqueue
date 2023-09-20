#' Calculate The probability tbat a type R customer has a positive delay
#'
#' @param m := number of restricted servers
#' @param n := the number of general use servers
#' @param p_B_R := probability of delay for R customers
#' @param p_B_G := probability of delay for G customers
#' @param mu_R := Restricted server service rate
#' @param mu_G := General server service rate
#' @param lam_r := arrival rate of R customers
#' @param lam_g := arrival rate of G customers
#'
#' @return utilization of general servers
#' @export
#'
#' @examples
calc_rho_g <- function( K, p, X, n, m, p_B_R, p_B_G, mu_R, mu_G, lam_r, lam_g){
    rho_g <- 0
    #service of all general customers
    rho_g <- lam_g / (n*mu_G)
    #probability service of restricted calls when at least one G server is free and no R server is free
    p_1 <- (1-p_B_G)*p_B_R
    #probability when there is a restricted queue and all servers are busy
    p_2 <- (p_B_R *p_B_G)
    # probability R finishes before G
    R_fin <- (n*mu_G) / (m*mu_R + n*mu_G)
    # probability G finishes before R
    G_fin<- (m*mu_R) / (m*mu_R + n*mu_G)
    #summation in ith spot in queue and R finishes before ith general servers
    p_3 <- (1-p)^(K+1)
    for(i in 1:(K)){
      p_3 <- p_3 + (((1-p)^(i-1))*p*(G_fin^(i)))
    }
    p_4 <- 0
    for (j in (n:len(X))){
      p_4 <- p_4 + X[k,j]
    }
  #summing it all together
    rho_g <- rho_g + ((lam_r*(p_1 + p_2*p_3+ p_4)) /  (n*mu_G))
  return (rho_g)
}
