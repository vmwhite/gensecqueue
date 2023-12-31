test_that("test the probability that a type R customer has a positive delay caclulation", {
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
  ex <- prob_vec[m+1,n+1] + prob_vec[m+1,n+1+1] + prob_vec[m+1,n+2+1] + prob_vec[m+1,n+3+1] +
        prob_vec[m+1+1,n+1] + prob_vec[m+1+1,n+1+1] + prob_vec[m+1+1,n+2+1] + prob_vec[m+1+1,n+3+1]

  ##Test 1 Value
  p_B_r <- calc_p_B_r(m,n,K,prob_vec)

  ## Test 1
  expect_equal(ex, p_B_r)


})
