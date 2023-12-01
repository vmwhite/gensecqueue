test_that("calculating X_{i+1} = X_i%*% R works", {
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

    #### validated matrices
    A <- calc_A_k(K,m,n,lam,lam_R,lam_G,mu_R,mu_G, p)

    B <- calc_B_ki(K,m,n,lam,lam_R,lam_G,mu_R,mu_G, p)

    R <- calc_R(A,K,n)

    G <- trunc_G(K,n,m, A,B,R)
    G_trans <- t(G)
    G_final <- last_row_G(G_trans, K, R, m)

    b <- matrix(0, nrow=((m+1)*matrix_size),ncol=1)
    b <- rbind(b,1)

    vec <- solve_prob_matrix(G_final,b)

    # example final
    X_ex <- t(tail(vec, n=K+1)) %*% R
    X_ex <- as.numeric(unlist(rbind(X_ex)))
    # test function
    X_test <- normalize_vector(vec,matrix_size,R)
    X_test <- reformat_X(X_test,matrix_size)
    X_test_sub <- tail(X_test, n= 1)
    X_test_sub <- as.numeric(unlist(unclass(cbind.data.frame(X_test_sub))))

    #first  generated row is equal
    expect_equal(X_ex, X_test_sub)

})
