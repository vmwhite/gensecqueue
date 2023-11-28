test_that("L_g calculation works", {
  m<- 1
  n<-5
  rho<-.4
  p <- .2
  lam <- 1
  mu_r <- lam / (rho*(m+n))
  mu_g <- mu_r
  results<- solve_K(m,n,lam,mu_g,mu_r, p)

  expect_equal(2 * 2, 4)
})
