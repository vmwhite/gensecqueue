test_that("Check_Results", {
  ########### second test
  K <- 5
  m <- 5
  n <- 2
  lam <- 1
  mu_r <- 5
  mu_g <- 7
  p <-  .75
  restults<- gensecqueue(lam,n,m,p,mu_g,mu_r)
  # Solve
  expect_no_error(gensecqueue(lam,n,m,p,mu_g,mu_r,K))
})
