test_that("test G utilization calculation works", {
  ## test params
  K <- 5
  m <- 2
  n <- 2
  q <- .8
  lam <- 1
  mu_r <- 5
  mu_g <- 7
  p <- 1-q
  lam_r <- lam*q
  lam_g <- lam*p
  matrix_size <- K +1
  A<- calc_A_k(K,m,n,lam,lam_r, lam_g, mu_r, mu_g, p)
  B<- Calc_B_ki(K,m,n,lam,lam_r, lam_g, mu_r, mu_g, p)
  R <- calc_R(A,K,n)
  prob_vec <- calc_X(K,m,n,A,B,R, .0000000001)
  p_B_r <- calc_p_B_r(m,n, K, prob_vec)
  p_B_g <- calc_p_B_g(K,n,prob_vec)
  # probability R finishes before G
  R_fin <- (n*mu_g) / (m*mu_r + n*mu_g)
  # probability G finishes before R
  G_fin<- (m*mu_r) / (m*mu_r + n*mu_g)

  ##Example 1 value
  ex <- (lam_g) / (n*mu_g)
  ex <- ex + ((lam_r * p_B_r*(1 - p_B_g) ) / (n*mu_g))
  #queue length 1
  p_l_1 <- prob_vec[4,3] +prob_vec[4,4]+prob_vec[4,5]+prob_vec[4,6]
  ex <- ex + ((lam_r*p_B_r*p_B_g*(p_l_1*G_fin ))/(n*mu_g))
  #queue length 2
  p_l_2 <- prob_vec[5,3] +prob_vec[5,4]+prob_vec[5,5]+prob_vec[5,6]
  ex <- ex + ((lam_r*p_B_r*p_B_g*(p_l_2*G_fin*G_fin))/(n*mu_g))

  ##Test 1 Value
  rho_g <- calc_rho_g( K, p, n, m, p_B_r, p_B_g, mu_r, mu_g, lam_r, lam_g, prob_vec)

  ## Test 1
  expect_equal(ex*10000000000000, rho_g*10000000000000)

})
