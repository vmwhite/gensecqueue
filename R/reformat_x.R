#' Reformat probability matrix from X[k] to X[i,j]
#'
#' @param X := probability matrix
#' @param matrix_size := size of reformatted matrix
#'
#' @return := re-sized probability matrix
#' @export
#'
#' @examples
#'
reformat_X <- function(X, matrix_size){
  X_i <- x_ij()
  X_i <- matrix(0, nrow = max(length(X), nrow(X))/matrix_size, ncol=matrix_size) #depending on solution method use nrow or length
  count <- 1
  for (i in 1:(max(length(X), nrow(X))/matrix_size)){
    for(j in 1:matrix_size){
      X_i[i,j] <- X[count]
      count <- count+1
    }
  }
  X_i[is.na(X_i)] <- 0
  return(X_i)
}
