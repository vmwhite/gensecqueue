test_that("Check_Results", {
  ########### Testing Table 1
  m <- c(1,3,5) #automatic servers
  n <- 5 #general servers
  rho <- c(.4,.6,.8)
  p <-  c(.2,.4,.6,.8)
  lam <- 1
  count <- 1

  ## Expected results
  rho_G <- c()
  K <-c(8,8,8,8, 11,8,8,8, 13,8,8,8,
        9,8,8,7, 14,9,8,8, "unstable",14,8,8,
        11,8,8,8,">25", 13,8,8, "unstable", ">25", 11, 8)
  rho_k<-  c()

  for (auto_lanes in m){
    for (rho_val in rho){
      mu_r <- lam / (rho_val*(auto_lanes+n))
      mu_g <- mu_r
      for ( per in p){
        # Solve
        expect_no_error(gensecqueue(lam,n,auto_lanes, per, mu_g, mu_r))
        results<- gensecqueue(lam,n,auto_lanes,per,mu_g,mu_r)

        #check solution<
        #        results <-round(results,4)
        #        expect_equal(results[10], rho_K[count])

        #        results <-round(results,2)
        #        expect_equal(results[11], rho_g[count])

        #        results <- round(results,0)
        #expect_equal(results[9], K[count])



        count <- count + 1
      }
    }

  }




})
