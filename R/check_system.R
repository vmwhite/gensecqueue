#' Check for steady state system conditions
#'
#' @param lam := arrival rate of all customers
#' @param n := the number of general use servers
#' @param m := the number of restricted servers
#' @param p := the probability a customer requires restricted service
#' @param mu_g := the service rate of general servers
#' @param mu_r := the service rate of restricted servers
#'
#' @return string indicating steady state exists or not
#' @export
#'
#' @examples
check_system <- function(lam,n,m, p, mu_g, mu_r) {
lam_g <- (1-p)*lam
rho_g <- lam_g / ( n*mu_g)
rho_r <-  lam/(m*mu_r + n*mu_g)

  if (rho_g > 1){
    paste0("ERROR - utilization of general servers servers is ", rho_g, " must be below 1. please adjust n, p, or lambda lower and/or mu_g higher.")
  }else if (rho_r > 1){
    paste0("ERROR - utilization of restricted servers is ", rho_r, " must be below 1. please adjust lambda lower and/or m, n, mu_g, or mu_r higher.")
  }else{
    paste0("Calculating Metrics - model inputs are valid.")
  }
}
