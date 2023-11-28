#' Calculate L_r the average number of customers in the restricted system
#'
#' @param m := number of restricted servers
#' @param n := number of general servers
#' @param prob_vec := state probability matrix
#' @param size := size of probability matrix
#' @param N := Maximum length of general queue
#'
#' @return
#' @export
#'
#' @examples
calc_L_r <- function(m,n,prob_vec,size, N){
  #add one to for loops since R indexes at one
  L_r <- 0
  ##########
  if (m+1+1 > (size)){
    # first sum only
    for(i in (1):(m+1)){
      for(j in (1):(N+1)){
        val <- (i-1)*prob_vec[i,j]
        L_r <-  L_r + val
      }
    }
  #########
  }else if (m+1+1 == (size)){
    # first sum
    for(i in (1):(m+1)){
      for(j in (1):(N+1)){
        val <- (i-1)*prob_vec[i,j]
        L_r <-  L_r + val
      }
    }
    # second sum
    i <- m+1+1
    for(j in (n+1):(N+1)){
      val <- (i-1)*prob_vec[i,j]
      L_r <-  L_r + val
    }
  ########
  }else{
    # first sum
    for(i in (1):(m+1)){
      for(j in (1):(N+1)){
        val <- (i-1)*prob_vec[i,j]
        L_r <-  L_r + val
      }
    }
    # second sum
    for(i in (m+1+1):(size)){
      for(j in (n+1):(N+1)){
        val <- ((i-1))*prob_vec[i,j]
        L_r <-  L_r + val
      }
    }

  }
return(L_r)
}
