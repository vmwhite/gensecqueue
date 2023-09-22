test_that("check alpha calculation is correct", {
  alpha <- calc_alpha_k(k,p,q)
  test_value <- p*(q^k)
  expect_equal(test_value, alpha)
})
