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
    temp <-tail(X_ex, n=K+1) %*% R
    dimnames(temp) <- NULL
    X_ex <- rbind(X_ex, temp)
    for (i in 1:3){
      temp <-tail(X_ex, n=1) %*% R
      dimnames(temp) <- NULL
      X_ex <- rbind(X_ex, temp)
    }

    # test function
    X_test <- normalize_vector(vec,matrix_size,R, 1e-13, 10e-5)
    X_test <- reformat_X(X_test,matrix_size)
    X_test_sub <- tail(X_test, n= 5)
    colnames(X_test_sub) <- NULL
    rownames(X_test_sub) <- NULL
    dimnames(X_test_sub) <- NULL

    #first five generated rows are equal
    expect_equal(X_ex, X_test_sub)

})
