## File Name: numerical_gradient.R
## File Version: 0.12

numerical_gradient <- function(par, FUN, h=1E-5, ...)
{
    val <- FUN(par, ...)
    NV <- length(val)
    NP <- length(par)
    mat <- matrix(NA, nrow=NV, ncol=NP)
    for (coord in 1:NP){
        res0 <- numerical_Hessian_partial(par=par, FUN=FUN, h=h, coordinate=coord, ...)
        mat[,coord] <- res0$grad
    }
    return(mat)
}
