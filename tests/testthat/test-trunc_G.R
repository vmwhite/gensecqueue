test_that("Check Valid G trunc matrix", {
  ## test params
  K <- 5
  m <- 2
  n <- 2
  q <- .2
  lam <- 1
  mu_R <- 5
  mu_G <- 7
  p <- 1-q
  lam_R <- lam*q
  lam_G <- lam*p
  matrix_size <- K +1

  #### validated matrcies
  A <- calc_A_k(K,m,n,lam,lam_R,lam_G,mu_R,mu_G, p)

  B <- calc_B_ki(K,m,n,lam,lam_R,lam_G,mu_R,mu_G, p)

  R <- calc_R(A,K,n)

  ## put into form of G
  aij <- matrix(0, nrow=1,ncol=matrix_size)
  G_T <- matrix(0, nrow = ((m + 1)*matrix_size), ncol = ((m + 1)*matrix_size ))
  zero_matrix = matrix(c(0,0,0,0, 0, 0,
                         0,0,0,0, 0, 0,
                         0,0,0, 0, 0, 0,
                         0,0,0,  0, 0, 0,
                         0,0,0,  0,0, 0,
                         0,0,0,0, 0,0)
                       , nrow = 6, ncol = 6, byrow = TRUE)
  X <- R%*% B[4,4,,] +  R%*% R%*%B[5,5,,] +  R%*% R%*% R%*%B[6,6,,]
  Y<- R%*% B[3,4,,] +  R%*% R%*%B[4,5,,] +  R%*% R%*% R%*%B[5,6,,] +R%*% R%*% R%*% R%*%A[6,,]
  ## possible error in Green 1993 might sum to 0 instead, trying new first then will go back if necessary
  Z <- A[2,,] + R%*% A[3,,] +  R%*% R%*%A[4,,] +  R%*% R%*% R%*%A[5,,] +R%*% R%*% R%*% R%*%A[6,,]

  G_col_1 <- do.call(rbind, list( B[2,1,,], B[3,2,,], zero_matrix ))
  G_col_2 <- do.call(rbind, list(  B[1,1,,], B[2,2,,],  B[3,3,,] + X ))
  G_col_3 <- do.call(rbind, list(zero_matrix, B[1,2,,], B[2,3,,] + Y))
  G_T <- do.call(cbind, list(   G_col_1 ,   G_col_2 ,   G_col_3  ))

  ## Check G
  G <- trunc_G(K,n,m, A,B,R)
  expect_equal(G_T, G)



})
