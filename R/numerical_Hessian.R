## File Name: numerical_Hessian.R
## File Version: 0.27

##############################################################################
# numerical computation of the Hessian matrix
numerical_Hessian <- function(par, FUN, h=1E-5, gradient=FALSE,
        hessian=TRUE, diag_only=FALSE, ... )
{

    NP <- length(par)

    f0 <- FUN( x=par, ... )
    fh <- rep(NA,NP)    # f(x+h)
    f2h <- rep(NA,NP)   # f(x+2*h)
    hess <- matrix(NA,nrow=NP, ncol=NP)
    fhh <- hess

    #** select h parameters according to size of parameters
    abs_par <- abs(par)
    hvec <- h * ifelse( abs_par > 1, abs_par, 1 )

    #--- loop for computing f(x+h)
    for (ii in 1:NP){
        par1 <- par
        par1[ii] <- par[ii] + hvec[ii]
        fh[ii] <- FUN( x=par1, ...)
    }

    #--- computation of the gradient
    if (gradient){
        grad1 <- res <- ( fh - f0 ) / hvec
    }

    #------
    # second partial derivatives
    # d F / dx dy
    # dF/dx=g(x,y)=( F(x+h,y) - F(x,y) ) / h
    # (dF/dx)/dy=( g(x,y+h) - g(x,y) ) / h
    #       =( F(x+h,y+h) - F(x,y+h) - F(x+h,y) + F(x,y+h) ) / h^2

    #---- hessian
    if ( hessian ){

        fh1 <- matrix( fh, nrow=NP, ncol=NP, byrow=TRUE)
        fh2 <- matrix( fh, nrow=NP, ncol=NP, byrow=FALSE)

        #--- computation f(x+2*h)
        for (ii in 1:NP){
            par1 <- par
            par1[ii] <- par[ii] + 2*hvec[ii]
            f2h[ii] <- FUN( x=par1, ... )
        }
        #--- computation f(x+h,y+h)
        if ( ! diag_only ){
            for (ii in 1:NP){
                for (jj in 1:NP){
                    if (ii < jj){
                        par1 <- par
                        par1[ii] <- par[ii] + hvec[ii]
                        par1[jj] <- par[jj] + hvec[jj]
                        fhh[ii,jj] <- fhh[jj,ii] <- FUN( x=par1, ... )
                    }
                }
            }
        }
        h_squared <- outer( hvec, hvec )
        hess <- ( fhh - fh1 - fh2 + f0 ) / h_squared
        diag(hess) <- ( f2h - 2*fh + f0)/ hvec^2
        res <- hess
    }

    if ( gradient & hessian ){
        res <- list( "grad"=grad1, "hessian"=hess, "value"=f0)
    }
    return(res)
}
##############################################################################
