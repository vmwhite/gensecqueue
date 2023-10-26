test_that("Probability Matrix Solution", {
  lam <- 1
  mu <- 3
  alpha <- 2
  G <- matrix(c(-lam,lam,0,0,0,1,
                mu,-(lam+mu),lam,0,0,1,
                0, mu+alpha,-(mu+alpha+lam),lam,0,1,
                0, 0, mu+(2*alpha), - ((lam/2) + mu + (2*alpha)), lam/2,1,
                0, 0,0, mu+(3*alpha), -(mu+3*alpha),1), nrow=5, ncol=6,byrow = TRUE)
  G <- t(G)
  b <- matrix(c(0,0,0,0,0,1),nrow=6,ncol=1,byrow = TRUE)
  X_test <- solve_prob_matrix(G,b)
  X_test2 <- solve_prob_matrix(G[-5,],t(t(b[-2,1])))
  expect_equal(X_test2, X_test)

  # get raw example solution one that requires lsfit, one that doesnt
  solution <- matrix(c(0.7091932458,
                     0.2363977486,
                     0.0472795497,
                     0.0067542214,
                     0.0003752345),nrow=5, ncol=1,byrow = TRUE)

  expect_equal(solution, X_test)
})
