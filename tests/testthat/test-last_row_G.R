test_that("appended G trunc last row check", {

  G_test <-   matrix(c(0,0,0,0, 0, 0,
                       0,0,0,0, 0, 0,
                       0,0,0, 0, 0, 0,
                       0,0,0,  0, 0, 0,
                       0,0,0,  0,0, 0,
                       0,0,0,0, 0,0)
                     , nrow = 6, ncol = 6, byrow = TRUE)
  R <- matrix(c(0,0,0,0, 0, 0,
                0,0,0,0, 0, 0,
                0,0,0, 0, 0, 0,
                0,0,0,  0, 0, 0,
                0,0,0,  0,0, 0,
                0,0,0,0, 0,0)
              , nrow = 6, ncol = 6, byrow = TRUE)
  m <- 3
  K <- 3
  G_test <- last_row_G(G, K, R, m)

  # example final
  G<-   matrix(c(0,0,0,0, 0, 0,
                 0,0,0,0, 0, 0,
                 0,0,0, 0, 0, 0,
                 0,0,0,  0, 0, 0,
                 0,0,0,  0,0, 0,
                 0,0,0,0, 0,0)
               , nrow = 6, ncol = 6, byrow = TRUE)


  expect_equal(G, G_test)
})
