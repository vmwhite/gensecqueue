test_that("multiplication of calc_mu works", {
  mu_ki <- calc_mu(1,2,3,4)
  check <- (1*3) + (2*4)
  expect_equal(mu_ki, check)
})
