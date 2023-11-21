test_that("appended G trunc last row check", {
  ## test params
  K <- 5
  m <- 2
  n <- 2
  q <- .2
  lam <- 1
  mu_R <- 5
  mu_G <- 7
  p <- 1-q
  lam_R <- lam*q
  lam_G <- lam*p
  matrix_size <- K +1

  #### validated matrices
  A <- calc_A_k(K,m,n,lam,lam_R,lam_G,mu_R,mu_G, p)

  B <- calc_B_ki(K,m,n,lam,lam_R,lam_G,mu_R,mu_G, p)

  R <- calc_R(A,K,n)

  G <- trunc_G(K,n,m, A,B,R)
  G_trans <- t(G)

  # example final
  X_m <- matrix(1, nrow = 1, ncol=matrix_size)

  ## append final equation
  #x_0*e + x_1*e +.. x_m(I-R)^{-1}*e = 1
  I <- diag(1,matrix_size)
  a_Xm <- X_m %*% t(solve(I - R)) # transpose so that the variable are aligned
  G_ex<- rbind(G_trans, c(1,1,1,1,1,1,1,1,1,1,1,1, a_Xm))

  # test function
  G_test <- last_row_G(G_trans, K, R, m)

  expect_equal(G_ex, G_test)
})
