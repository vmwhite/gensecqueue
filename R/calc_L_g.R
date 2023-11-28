#' Calculate L_g the average number of customers in the general system
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
calc_L_g <- function(m,n,prob_vec,size, N){
  #add one to for loops since R indexes at one
  L_g <- 0
  ########
  if ((m+1+1) > (size)){
    # first sum only
    for(i in (1):(m+1)){
      for(j in (1):(N+1)){
        val <- ((j-1))*prob_vec[i,j]
        L_g <- L_g + val
      }
    }
  #######
  }else if ((m+1+1) == (size)){
    # first sum
    for(i in (1):(m+1)){
      for(j in (1):(N+1)){
        #add one to for loops since R indexes at one
        val <- ((j-1))*prob_vec[i,j]
        L_g <- L_g + val
      }
    }
    # second sum
    i <- (m+1+1)
    for(j in (n+1):(N+1)){
      #add one to for loops since R indexes at one
      val <- ((j-1))*prob_vec[i,j]
      L_g <- L_g + val
    }
  }else{
    # first sum
    for(i in (m+1+1):(size)){
      for(j in (n+1):(N+1)){
        #add one to for loops since R indexes at one
        val <- ((j-1))*prob_vec[i,j]
        L_g <- L_g + val
      }
    }
    # second sum
    for(i in (1):(m+1)){
      for(j in (1):(N+1)){
        #add one to for loops since R indexes at one
        val <- ((j-1))*prob_vec[i,j]
        L_g <- L_g + val
      }
    }
  }
  return(L_g)
}
