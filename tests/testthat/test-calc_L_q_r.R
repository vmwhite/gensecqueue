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
  ex <- 0
  ##Test 1 Value
  L_q_r <- calc_L_q_r(K,m,n,prob_vec)
  ## Test 1
  expect_equal(ex, L_q_r )

  ##Example 2 value
  prob_vec <- calc_X(K,m,n,A,B,R, 0.000000000001,  0.000000000001)
  ex <- prob_vec[m+1+1,n+1]*(m+1-m) + prob_vec[m+1+1,n+1+1]*(m+1-m) + prob_vec[m+1+1,n+2+1]*(m+1-m) + prob_vec[m+1+1,n+3+1]*(m+1-m) +
    prob_vec[m+2+1,n+1]*(m+2-m) + prob_vec[m+2+1,n+1+1]*(m+2-m) + prob_vec[m+2+1,n+2+1]*(m+2-m) + prob_vec[m+2+1,n+3+1]*(m+2-m) +
    prob_vec[m+3+1,n+1]*(m+3-m) + prob_vec[m+3+1,n+1+1]*(m+3-m) + prob_vec[m+3+1,n+2+1]*(m+3-m) + prob_vec[m+3+1,n+3+1]*(m+3-m) +
    prob_vec[m+4+1,n+1]*(m+4-m) + prob_vec[m+4+1,n+1+1]*(m+4-m) + prob_vec[m+4+1,n+2+1]*(m+4-m) + prob_vec[m+4+1,n+3+1]*(m+4-m)
    # first sum is i = 3 to inf, add one to all due to indexing
    #second sum is 2 to 5,  add one to all due to indexing
  ##Test 2 Value
  L_q_r <- calc_L_q_r(K,m,n,prob_vec)
  ## Test 1
  expect_equal(ex, L_q_r )

})
