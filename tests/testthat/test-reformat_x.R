test_that("reformat works", {

  X <- matrix()
  X_test <- reformat_X(X)


  X_ij <-   matrix(c(0,0,0,0, 0, 0,
                     0,0,0,0, 0, 0,
                     0,0,0, 0, 0, 0,
                     0,0,0,  0, 0, 0,
                     0,0,0,  0,0, 0,
                     0,0,0,0, 0,0)
                   , nrow = 6, ncol = 6, byrow = TRUE)

   expect_equal(X_ij, X_test)
})
