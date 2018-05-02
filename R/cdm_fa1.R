## File Name: cdm_fa1.R
## File Version: 0.05

#--- function for unidimensional factor analysis
cdm_fa1 <- function(Sigma, method=1, maxit=50, conv=1E-5)
{
    # Sigma is a correlation matrix
    I <- ncol(Sigma)
    L <- matrix(.6, nrow=I, ncol=1)
    iter <- 0
    iterate <- TRUE
    #--- begin iterations
    while (iterate){
        L0 <- L
        #--- method = 1 -> different item loadings
        if (method == 1){
            for (ll in 1:I){
                y <- Sigma[ll,-ll]
                x <- L[-ll,1]
                L[ll,1] <- sum(x*y) / sum(x^2)
            }
        }
        #--- method = 2 -> equal item loadings
        if (method == 2){
            val <- ( sum(Sigma) - I ) / ( I^2 - I )
            L <- matrix( sqrt(val) , nrow=I, ncol=1)
        }
        iter <- iter + 1
        parchange <- max( abs(L0 - L))
        if (iter >= maxit){ iterate <- FALSE }
        if (parchange < conv){ iterate <- FALSE }
    }
    #--- output
    res <- list(iter=iter, L=L, parchange=parchange)
    return(res)
}
