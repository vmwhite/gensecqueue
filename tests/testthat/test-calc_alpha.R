test_that("alpha is valid", {
  #expect_equal(2 * 2, 4)
  #X <- calc_X()
  alpha <- calc_alpha(1,1,c(.1,.1,.1),1)
  expect_that( object = alpha, condition = "<1" )
  expect_that( object = is.numeric(alpha), condition = equals(TRUE) );
})
