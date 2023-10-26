#' Calculate performance metrics of a queueing system with General and Restricted Servers
#'
#' @param lam := arrival rate of all customers
#' @param n := the number of general use servers
#' @param m := the number of restricted servers
#' @param p := the probability a customer requires restricted service
#' @param mu_g := the service rate of general servers
#' @param mu_r := the service rate of restricted servers
#'
#' @return data frame of key performance metrics
#' @export
#'
#' @examples
gensecqueue <- function(lam,n,m, p, mu_g, mu_r){
  #store inputs
  metrics <- list("lambda","general servers", "restrcted servers", "p", "mu_g", "mu_r")
  results <- list(lam, n, m , p ,mu_g, mu_r)

  ### check to make sure inputs are valid
  skip<- FALSE
  if (grepl(  "ERROR",print(check_system(lam,n,m, p, mu_g, mu_r)), fixed = TRUE) == TRUE){
    skip <- TRUE
  }
  #Solve for metrics for valid system
  if (skip == FALSE){
    ## intermediate calculations
    lam_g <- lam*(1-p)
    lam_r <- lam*p

    ## Find Transition Probability Matrix
    A<- calc_A_k(K,m,n,lam,lam_r, lam_g, mu_g, p)
    B<- Calc_B_ki(K,m,n,lam,lam_r, lam_g, mu_r, mu_g, p)
    R <- calc_R(A,K,n)
    prob_vec <- calc_X(K,m,n,A,B,R)

    ## Length of queues
      #steady-state number of customers in the "restricted queue"
      L_q_r <- calc_L_q_r(K,m,prob_vec)
      results <- append(results, L_q_r)
      #steady-state number of customers in the "general queue"
      L_q_g <- calc_L_q_g(K,n,prob_vec)
      results <- append(results, L_q_g)
      #Steady-state number of customers waiting in queue
      L_q <- L_q_r + L_q_g
      results <- append(results, L_q)

    ## Mean Delay
      #the mean delay for customers of type restricted
      W_q_r <- L_q_r / lam
      results <- append(results, W_q_r)
      #the mean delay for customers of type general
      W_q_g <- (L_q_r / lam) + (L_q_g/ lam_g)
      results <- append(results, W_q_g)

    ## Probability of Positive Delay
      p_B_r <- calc_p_B_r(m,n, K, prob_vec)
      results <- append(results, p_B_r)
      p_B_g <- calc_p_B_g(K,n,prob_vec)
      results <- append(results, p_B_g)

    ## Probability a general customer is delayed when a restricted server is available
      alpha <- calc_alpha(m,n,prob_vec,K)
      results <- append(results, alpha)

    ## Vehicle Utilization
      # general utilization
      rho_g <- calc_rho_g(K, p,prob_vec, n, m, p_B_R, p_B_G, mu_R, mu_G, lam_r, lam_g)
      results <- append(results, rho_g)
      # restricted utilization
      rho_r <- calc_rho_r( K, p, n, m, p_B_R, p_B_G, mu_R, mu_G, lam_r)
      results <- append(results, rho_r)

  #add dummy results for invalid system
  }else{
    for (i in 1:10){
      results <- append(results, "unstable")
    }
  }

  #create DF of results
  DF <- data.frame(results)
  #set column names
  metrics <- append(metrics, "L_q_r" )
  metrics <- append(metrics, "L_q_g" )
  metrics <- append(metrics, "L_q" )
  metrics <- append(metrics, "W_q_r" )
  metrics <- append(metrics, "W_q_g" )
  metrics <- append(metrics, "p_B_r" )
  metrics <- append(metrics, "p_B_g" )
  metrics <- append(metrics,  "alpha" )
  metrics <- append(metrics,  "rho_g" )
  metrics <- append(metrics,  "rho_r" )
  colnames(DF) <- metrics
  return(DF)
}
