test_that("Test B matrix calculations", {
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


  ##### hard coded matrices from Green 1993 page 175-176
  B_00 = matrix(c(lam_R,0,0, 0, 0, 0,
                  0,lam_R,0,  0, 0, 0,
                  0,0,lam_R,  0, 0, 0,
                  0,0,0,lam_R, 0, 0,
                  0,0,0,0,lam_R, 0,
                  0,0,0,0,0,lam)
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_10 = matrix(c(-(lam+(0*mu_R)),lam_G,0, 0, 0, 0,
                  mu_G,-(lam +calc_mu(0,1, mu_R,mu_G)),lam_G,  0, 0, 0,
                  0,2*mu_G,-(lam +calc_mu(0,2, mu_R,mu_G)),  lam_G, 0, 0,
                  0,0, 2*mu_G,-(lam +calc_mu(0,2, mu_R,mu_G)),  lam_G, 0,
                  0,0,0,2*mu_G,-(lam +calc_mu(0,2, mu_R,mu_G)),  lam_G,
                  0,0,0,0,2*mu_G,-(lam +calc_mu(0,2, mu_R,mu_G)))
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_11 = matrix(c(-(lam+(1*mu_R)),lam_G,0, 0, 0, 0,
                  mu_G,-(lam +calc_mu(1,1, mu_R,mu_G)),lam_G,  0, 0, 0,
                  0,2*mu_G,-(lam +calc_mu(1,2, mu_R,mu_G)),  lam_G, 0, 0,
                  0,0, 2*mu_G,-(lam +calc_mu(1,2, mu_R,mu_G)),  lam_G, 0,
                  0,0,0,2*mu_G,-(lam +calc_mu(1,2, mu_R,mu_G)),  lam_G,
                  0,0,0,0,2*mu_G,-(lam +calc_mu(1,2, mu_R,mu_G)))
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_12 = matrix(c(-(lam+(2*mu_R)),lam,0, 0, 0, 0,
                  mu_G,-(lam +calc_mu(2,1, mu_R,mu_G)),lam,  0, 0, 0,
                  0,2*mu_G,-(lam +calc_mu(2,2, mu_R,mu_G)),  0, 0, 0,
                  0,0, 2*mu_G,-(lam +calc_mu(2,2, mu_R,mu_G)),  0, 0,
                  0,0,0,2*mu_G,-(lam +calc_mu(2,2, mu_R,mu_G)),  0,
                  0,0,0,0,2*mu_G,-(lam +calc_mu(2,2, mu_R,mu_G)))
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_21 = matrix(c(mu_R,0,0, 0, 0, 0,
                  0,mu_R,0,  0, 0, 0,
                  0,0,mu_R,  0, 0, 0,
                  0,0,0,mu_R, 0, 0,
                  0,0,0,0,mu_R, 0,
                  0,0,0,0, 0,mu_R)
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_22 = matrix(c(2*mu_R,0,0, 0, 0, 0,
                     0,2*mu_R,0,  0, 0, 0,
                     0,0,2*mu_R,  0, 0, 0,
                     0,0,0,2*mu_R, 0, 0,
                     0,0,0,0, 2*mu_R, 0,
                     0,0,0,0, 0, 2*mu_R)
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_23 = matrix(c(0,0,0,0, 0, 0,
                  0,0,0,0, 0, 0,
                  0,0,2*mu_R*p+2*mu_G, 0, 0, 0,
                  0,0,0,  2*mu_R*p, 0, 0,
                  0,0,0,  0, 2*mu_R*p, 0,
                  0,0,0,0, 0, 2*mu_R)
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_33 = matrix(c(0,0,0,0, 0, 0,
                  0,0,0,0, 0, 0,
                  0,0,0, 2*mu_R*q, 0, 0,
                  0,0,0, 0, 2*mu_R*q,  0,
                  0,0,0,  0, 0, 2*mu_R*q,
                  0,0,0,0, 0, 0)
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_34 = matrix(c(0,0,0,0, 0, 0,
                  0,0,0,0, 0, 0,
                  0,0,0, 2*mu_R*calc_alpha_k(1,p,q), 0, 0,
                  0,0,0,  0, 2*mu_R*calc_alpha_k(1,p,q), 0,
                  0,0,0,  0, 0, 2*mu_R*q,
                  0,0,0,0, 0, 0)
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_44 = matrix(c(0,0,0,0, 0, 0,
                  0,0,0,0, 0, 0,
                  0,0,0,0, 2*mu_R*(q^2), 0,
                  0,0,0, 0,0, 2*mu_R*(q^2),
                  0,0,0,  0,0, 0,
                  0,0,0,0, 0, 0)
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_45 = matrix(c(0,0,0,0, 0, 0,
                  0,0,0,0, 0, 0,
                  0,0,0, 0, 2*mu_R*calc_alpha_k(2,p,q), 0,
                  0,0,0,  0, 0, 2*mu_R*(q^2),
                  0,0,0,  0, 0, 0,
                  0,0,0,0, 0, 0)
                , nrow = 6, ncol = 6, byrow = TRUE)
  B_55 = matrix(c(0,0,0,0, 0, 0,
                  0,0,0,0, 0, 0,
                  0,0,0, 0, 0, 2*mu_R*(q^3),
                  0,0,0,  0, 0, 0,
                  0,0,0,  0,0, 0,
                  0,0,0,0, 0,0)
                , nrow = 6, ncol = 6, byrow = TRUE)

  B <- Calc_B_ki(K,m,n,lam,lam_R,lam_G,mu_R,mu_G, p)

#Check values -- Note: R indexes at 1
  expect_equal(B[1,1,,], B_00)
  expect_equal(B[2,1,,], B_10)
  expect_equal(B[2,2,,], B_11)
  expect_equal(B[2,3,,], B_12)
  expect_equal(B[3,2,,], B_21)
  expect_equal(B[3,3,,], B_22)
  expect_equal(B[3,4,,], B_23)
  expect_equal(B[4,4,,], B_33)
  expect_equal(B[4,5,,], B_34)
  expect_equal(B[5,5,,], B_44)
  expect_equal(B[5,6,,], B_45)
  expect_equal(B[6,6,,], B_55)
})
