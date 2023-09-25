  test_that("check A_k matrix", {
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

    ##### hard coded matrices from Green 1993 page 176-177
    A_0 <-    matrix(c(lam,0,0, 0, 0, 0,
                      0,lam,0,  0, 0, 0,
                      0,0,lam,  0, 0, 0,
                      0,0,0,lam, 0, 0,
                      0,0,0,0,0, 0,
                      0,0,0,0,0,0)
                    , nrow = 6, ncol = 6, byrow = TRUE)
    A_1 <-    matrix(c(-(lam+calc_mu(2,2)),0,0, 0, 0, 0,
                       2*mu_G,-(lam+calc_mu(2,2)),0,  0, 0, 0,
                       0,2*mu_G,-(lam+calc_mu(2,2)),  0, 0, 0,
                       0,0,2*mu_G,-(lam+calc_mu(2,2)), 0, 0,
                       0,0,0,0,0, 0,
                       0,0,0,0,0,0)
                     , nrow = 6, ncol = 6, byrow = TRUE)
    A_2 <-    matrix(c(2*mu_R*p + 2*mu_G,0,0, 0, 0, 0,
                       0,2*mu_R*p,0,  0, 0, 0,
                       0,0,2*mu_R*p,  0, 0, 0,
                       0,0,0,2*mu_R, 0, 0,
                       0,0,0,0,0, 0,
                       0,0,0,0,0,0)
                     , nrow = 6, ncol = 6, byrow = TRUE)
    A_3 <-    matrix(c(0,2*mu_R*calc_alpha_k(1,p,q),0, 0, 0, 0,
                       0,0,2*mu_R*calc_alpha_k(1,p,q),  0, 0, 0,
                       0,0,0, 2*mu_R*q, 0, 0,
                       0,0,0,0, 0, 0,
                       0,0,0,0,0, 0,
                       0,0,0,0,0,0)
                     , nrow = 6, ncol = 6, byrow = TRUE)

    A_4 <-    matrix(c(0,0, 2*mu_R*calc_alpha_k(2,p,q), 0, 0, 0,
                       0,0,0, 2*mu_R*(q^2),  0, 0,
                       0,0,0, 0, 0, 0,
                       0,0,0,0, 0, 0,
                       0,0,0,0,0, 0,
                       0,0,0,0,0,0)
                     , nrow = 6, ncol = 6, byrow = TRUE)
    A_5 <-    matrix(c(0,0,0, 2*mu_R*(q^3), 0, 0,
                       0,0,0,  0, 0, 0,
                       0,0,0,0, 0, 0,
                       0,0,0,0, 0, 0,
                       0,0,0,0,0, 0,
                       0,0,0,0,0,0)
                     , nrow = 6, ncol = 6, byrow = TRUE)

  #### calc A_k
    A_k <- calc_A_k()
    expect_equal(A[1,,], A_0)
    expect_equal(A[2,,], A_1)
    expect_equal(A[3,,], A_2)
    expect_equal(A[4,,], A_3)
    expect_equal(A[5,,], A_4)
    expect_equal(A[6,,], A_5)
})
