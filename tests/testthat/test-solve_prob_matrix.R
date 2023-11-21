test_that("Probability Matrix Solution", {
  lam <- 1
  mu <- 3
  alpha <- 2
  G <- matrix(c(-lam,lam,0,0,0,1,
                mu,-(lam+mu),lam,0,0,1,
                0, mu+alpha,-(mu+alpha+lam),lam,0,1,
                0, 0, mu+(2*alpha), - ((lam/2) + mu + (2*alpha)), lam/2,1,
                0, 0,0, mu+(3*alpha), -(mu+3*alpha),1), nrow=5, ncol=6,byrow = TRUE)
  G <- t(G)
  b <- matrix(c(0,0,0,0,0,1),nrow=6,ncol=1,byrow = TRUE)
  X_test <- solve_prob_matrix(G,b)
  X_test2 <- solve_prob_matrix(G[-5,],t(t(b[-2,1])))
  expect_equal(X_test2, X_test)

  # get raw example solution one that requires lsfit, one that doesnt
  solution <- matrix(c(0.7091932458,
                     0.2363977486,
                     0.0472795497,
                     0.0067542214,
                     0.0003752345),nrow=5, ncol=1,byrow = TRUE)

  expect_equal(solution, X_test)

  ########### second test
  K <- 5
  m <- 2
  n <- 2
  q <- .2
  lam <- 1
  mu_r <- 5
  mu_g <- 7
  p <- 1-q
  lam_r <- lam*q
  lam_g <- lam*p
  matrix_size <- K +1
  A<- calc_A_k(K,m,n,lam,lam_r, lam_g, mu_r, mu_g, p)
  B<- calc_B_ki(K,m,n,lam,lam_r, lam_g, mu_r, mu_g, p)
  R <- calc_R(A,K,n)
  G <- trunc_G(K,m,n, A,B,R)
  G_trans <- t(G)
  #Add in total probability equation
  G_trans <-last_row_G(G_trans, K, R,m)

  #RHS
  matrix_size <- K + 1
  b <- matrix(0, nrow=((m+1)*matrix_size),ncol=1)
  b <- rbind(b,1)

  # Solve over-defined system
  expect_no_error(solve_prob_matrix(G_trans,b))

  #### test
  K <- 5
  m <- 5
  n <- 2
  lam <- 1
  mu_r <- 5
  mu_g <- 7
  p <-  .75

  lam_r <- lam*q
  lam_g <- lam*p
  matrix_size <- K +1
  A<- calc_A_k(K,m,n,lam,lam_r, lam_g, mu_r, mu_g, p)
  B<- calc_B_ki(K,m,n,lam,lam_r, lam_g, mu_r, mu_g, p)
  R <- calc_R(A,K,n)
  G <- trunc_G(K,n,m, A,B,R)
  G_trans <- t(G)
  #Add in total probability equation
  G_trans <-last_row_G(G_trans, K, R,m)

  #RHS
  matrix_size <- K + 1
  b <- matrix(0, nrow=m+1,ncol=1)
  b <- rbind(b,1)

  # Solve over-defined system
  expect_no_error(solve_prob_matrix(G_trans,b))

})
