#' Calculating B matrices
#'
#' @param K := truncation parameter
#' @param k := number of restricted servers
#' @param i := number of general servers
#' @param lambda := arrival rate of all customers
#' @param lambda_r := arrival rate of customers of type restricted
#' @param lambda_g := arrival rate of customers of type general
#' @param mu_g := general server rate
#' @param mu_r := restricted server rate
#' @param p := percentage of customers that are of type restricted
#'
#' @return matrix B of the states with boundary behavior
#' @export
#'
#' @examples
#' #' K <- 13
#' r <- 10
#' s <- 5
#' lambda <- .2
#' p <-.9
#' lambda_aux <- (p-1) * lambda
#' lambda_p <- p * lambda
#' mu_g <-.4
#' mu_r <-.5
#' Calc_Bmn(K,s,r,lambda,lambda_aux,lambda_p,mu_g,mu_r, p)
Calc_B_ki <- function(K,m,n,lam,lam_R,lam_G,mu_R,mu_G, p){
  q <- 1 - p
  # + 1 since r indexes at 1
  matrix_size <- (K) + 1
  k_bmax <- max((K-n+2)+1,m+1)
  i_bmax<- max( matrix_size,(m+K - n)+1, m)
  B_ki <- matrix(0, nrow=matrix_size, ncol=matrix_size)
  B <- array(c(B_ki), c(k_bmax , i_bmax , matrix_size, matrix_size))
  for (k in 1:k_bmax){
    for (i in 1:i_bmax){
      ii <- i
      #transition from state (i,) to  state(i - k + 1,)
       ii_two <- i - k + 1 +1 #add one since R indexes at 1
      for (j in 1:matrix_size){
        #### if at least one R server is free #####
        if((i-1) < m){
          # for transitions from state  (i,) to (i+1,)
          if( ii_two  == ii + 1 ){
            if (j-1 < K){
              B[k,i,j,j] = lam_R
            }else{
              B[k,i,j,j] = lam
            }
            # for transitions from state  (i,) to (i,)
          }else if ( ii_two == i){
            #B[m,n,j,j] = lambda # not for B's but for A's yes
             B[k,i,j,j-1] = (j-1)*mu_G
            if((j - 1) < K){
             B[k,i,j,j+1] = lam_G
            }
            # for transitions from state  (i,) to (i-1,)
          } else if ( ii_two == ii - 1){
           B[k,i,j,j] = (i-1)*mu_R# -1 since R indexes ii at 1
          }
          #### if all p servers are busy, at least 1 aux server is free, and there is a primary queue #####
        }else if(((i-1) + min((j-1),s)) > r && (j-1) < s){
          # for transitions from state  (i,) to (i+1,)
          if( ii_two  == ii + 1 ){
           B[k,i,j,j] = lambda
            # for transitions from state  (i,) to (i,)
          }else if ( ii_two == i){
           B[k,i,j,j-1] = (j-1)*mu_r*p
            # for transitions from state  (i,) to (i-1,)
          } else if ( ii_two == ii - 1){
           B[k,i,j,j] = (r-(j-1))*mu_g*p + (j-1)*mu_r*q  # -1 since R indexes j at 1
            # for transitions from state  (i,) to (i-2,)
          }else if ( ii_two == ii - 2){
           B[k,i,j,j+1] =(r-(j-1))*mu_g*q # -1 since R indexes j at 1
          }
          #### if all p servers are busy, at least 1 aux server is free, and there is NO primary queue #####
        }else if((i-1) + min((j-1),s) == r && (j-1) < s){
          # for transitions from state  (i,) to (i+1,)
          if( ii_two  == ii + 1 ){
           B[k,i,j,j] = lambda
            # for transitions from state  (i,) to (i,)
          }else if ( ii_two == i){
           B[k,i,j,j-1] = (j-1)*mu_r
            # for transitions from state  (i,) to (i-1,)
          } else if ( ii_two == ii - 1){
           B[k,i,j,j] = (i-1)*mu_g  # -1 since R indexes ii at 1
          }
          #### if all p and q servers are busy #####
        }else if((i-1) + min((j-1),s) >= r && (j-1) >= s){
          # for transitions from state  (i,) to (i+1,)
          if( ii_two  == ii + 1 ){
            if( (i - 1) <= r - s - 1 && (j-1) < K ){
             B[k,i,j,j] = lambda_p
            }else{
             B[k,i,j,j] = lambda
            }
            # else transitions
          }else{
            #### AND NO primary queue #####
            if( (i-1) == r-s && (j-1)>=s ){
              # for transitions from state  (i,) to (i,)
              if ( ii_two == ii ){
                if ((j-1)+(i-1) == (r+s)){
                 B[k,i,j,j-1] = s*mu_r*p
                }else{
                 B[k,i,j,j-1] = s*mu_g
                }

                #if((j-1) < K){
                #B[k,i,j,j+1] =lambda_aux
                #}
                # for transitions from state  (i,) to (i-1,)
              } else if ( ii_two == ii - 1){
               B[k,i,j,j] = (r-s)*mu_g
              }
              #### AND primary queue but NO aux queue#####
            }else if( (i-1)>= r-s && (j-1)==s ){
              # for transitions from state  (i,) to (i,)
              if ( ii_two == i){
               B[k,i,j,j-1] = s*mu_r*p
                # for transitions from state  (i,) to (i-1,)
              } else if ( ii_two == ii - 1){
               B[k,i,j,j] = (r-s)*mu_g*p + s*mu_r*q
                # transition from (i,j) to  (i - k -1, j+k), k ==0 is to (i-1) state
              }else{
                for (k in 1:((i-1)-r+s)){
                  if( k < (i-1) - r + s &&   ii_two == ii - k - 1 && (((j-1)+k < K))){
                   B[k,i,j,j+k] = (r-s)*mu_g*(q^k)*p
                  }else if (k <= (i-1) - r + s &&  ii_two == ii - k - 1 && (j-1)+ k <= K){
                   B[k,i,j,j+k] = (r-s)*mu_g*q^k
                  }
                }
              }
              #### AND primary queue AND aux queue#####
            }else if( (i-1)> r-s && (j-1)>s ){
              # for transitions from state  (i,) to (i,)
              if ( ii_two == i){
               B[k,i,j,j-1] =s*mu_r
                # transition from (i,j) to  (i - k -1, j+k), k > 0 since there is a primary queue
              }else{
                for (k in 0:((i-1)-r+s)){
                  if( k < (i-1) - r + s &&   ii_two == ii - k - 1 && (((j-1)+k < K) )){
                   B[k,i,j,j+k] = (r-s)*mu_g*(q^k)*p
                  }else if (k <= (i-1) - r + s &&  ii_two == ii - k - 1 && (j-1)+ k <= K){
                   B[k,i,j,j+k] =  (r-s)*mu_g*q^k
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  m_i_i = 2
  for (row in 1:matrix_size){
    for (col in 1:matrix_size){
      if (row == col){
        for (n in 1:i_bmax){
          for (m in 1:k_bmax){
            for (col_2 in 1:matrix_size){
              if ((m_i_i!= k || col!=col_2)){
                B[m_i_i,n,row,col] =  B[m_i_i,n,row,col]-B[k,i,row,col_2]
              }
            }
          }
        }
      }
    }
  }
  return(B)
}
