test_that("check L_q_r calculation", {
  ## test params
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
  prob_vec <- calc_X(K,m,n,A,B,R)

  ##Example 1 value
  ex1 <-  prob_vec[m+1+1,n+1]*(m+1-m) + prob_vec[m+1+1,n+1+1]*(m+1-m) + prob_vec[m+1+1,n+2+1]*(m+1-m) + prob_vec[m+1+1,n+3+1]*(m+1-m)
  ##Test 1 Value
  L_q_r <- calc_L_q_r(K,m,n,prob_vec)
  ## Test 1
  expect_equal(ex1, L_q_r )


})
