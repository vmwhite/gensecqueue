test_that("reformat works", {

  X <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), nrow=18, ncol=1, byrow = TRUE)
  X_test <- reformat_X(X, matrix_size=6)


  X_ij <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), nrow=3, ncol=6, byrow = TRUE)

   expect_equal(X_ij, X_test)
})
