#' Calculate The probability tbat a type R customer has a positive delay
#'
#' @param m := number of restricted servers
#' @param n := the number of general use servers
#' @param K := truncation parameter
#' @param prob_vec := state probability matrix
#'
#' @return p_B_r value
#' @export
#'
#' @examples
calc_rho_g <- function(){
    size <- length(prob_vec)
    rho_g <- 0
    # when all general customers (j <= n) and restricted calls when R servers adn busy and at least one general is free
    #service of general customers
    rho_g <- lam_g / (n*mu_g)
    rho_g <- rho_G + lam_r
}


