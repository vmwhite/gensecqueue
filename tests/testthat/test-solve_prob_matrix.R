test_that("Probability Matrix Solution", {
  G <- matrix()
  b <- matrix()
  X_test <- solve_prob_matrix(G,b)

  # get raw example solution one that requires lsfit, one that doesnt
  solution <- matrix()

  expect_equal(solution, X_test)
})
