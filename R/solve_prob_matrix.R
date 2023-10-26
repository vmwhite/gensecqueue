#' Solve Probability Matrix
#'
#' @param G := Truncated Generator Matrix
#' @param b := Right hand side values
#'
#' @return probability matrix
#' @export
#'
#' @examples
#' add example from test
#' G <- matrix(0)
#' b <- matrix(0)
#' X <- solve_prob_matrix(G,b)
solve_prob_matrix <- function(G,b) {
  t <- try(X <- qr.solve(G,b))
  if("try-error" %in% class(t)){
    print(paste0("Using lsfit for solving for probaility transition matrix instead"))
    X <- lsfit(G, b)
    X <- X$coefficients
    # remove first intercept coefficient
    X <- X[-1]
  }
  return(X)
}
