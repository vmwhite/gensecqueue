test_that("Check_Results", {
  ########### Testing Table 2
  # I am having particular issues with P_B_r calculation and alpha. also multi-linearity when m=n
  K <- 1
  m <- c(3,4) #automatic servers
  n <- 8-m #general servers
  rho <- 0.95
  mu_r <- 1/8 #in sec
  mu_g <- 1/10 #in sec
  p <-  c(.52, .75)
  lam <- rho*8*mu_g
  count <- 1

  ## Expected results
  E_num_qeu <- c(5.13,4.21,11.09,3.31)
  E_delay <-c(6.75, 5.53, 14.59,4.36) #in sec
  W_q_g <-  c(8.62,6.35,26.53,5.94)
  W_q_r <- c(4.97,5.25,3.52,3.83)
  p_B_g <- c(.77,.68,.83,.68)
  p_B_r <- c(.58,.61,.48,.54)
  alpha <- c(.14,.03,.32,.07)


  for ( auto_lanes in m){
    for ( per in p){

      # Solve
      expect_no_error(gensecqueue(lam,8-auto_lanes,auto_lanes,per,mu_g,mu_r,K))
      results<- gensecqueue(lam,8-auto_lanes,auto_lanes,per,mu_g,mu_r,K)

      #check solution<
      expect_equal(as.double(unclass(results[10])[1])*(per) + as.double(unclass(results[11])[1])*(1-per), E_delay[count]) # W_q_g*(1-per) + W_q_r*per = E_delay
      results <- round(results,2)
      expect_equal(as.double(unclass(results[9])[1]), E_num_qeu[count])
      expect_equal(as.double(unclass(results[11])[1]), W_q_g[count])
      expect_equal(as.double(unclass(results[10])[1]), W_q_r[count])
      expect_equal(as.double(unclass(results[13)[1]), p_B_g[count])
      expect_equal(as.double(unclass(results[12)[1]), p_B_r[count])
      expect_equal(as.double(unclass(results[14)[1]), alpha[count])
      count <- count + 1
    }
  }
})
