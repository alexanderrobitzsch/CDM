## File Name: numerical_Hessian_partial.R
## File Version: 0.03

numerical_Hessian_partial <- function(par , FUN , h = 1E-5, coordinate = 1, ... )
{
    ii <- coordinate
    f0 <- FUN( x = par, ... )

    #** select h parameters according to size of parameters
    abs_par <- abs(par)
    hvec <- h * ifelse( abs_par > 1 , abs_par , 1 )

    #--- loop for computing f(x+h)
    par1 <- par
    par1[ii] <- par[ii] + hvec[ii]
    f1 <- FUN( x = par1 , ...)
    #--- computation x-h
    par1[ii] <- par[ii] - hvec[ii]
    f2 <- FUN( x = par1 , ...)
    #--- gradient and Hessian
    grad <- (f1 - f2 ) / (2*h)
    hessian <- (f1 + f2 -2*f0) / h^2
    #--- output
    res <- list( f0=f0, f1=f1, f2=f2, grad=grad, hessian=hessian)
    return(res)
}
