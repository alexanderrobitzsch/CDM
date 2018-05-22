## File Name: cdm_fit_normal.R
## File Version: 0.04


cdm_fit_normal <- function(x, w)
{
    theta.k <- x
    if ( is.vector(theta.k) ){
        theta.k <- matrix( theta.k, ncol=1 )
    }
    D <- ncol(theta.k)
    #---- unidimensional model
    if (D==1){
        mg <- sum( theta.k[,1] * w )
        sdg <- sum( theta.k[,1]^2 * w ) - mg^2
    }
    #---- multidimensional model
    if (D>1){
        mean.gg <- rep(0,D)
        Sigma.gg <- diag(0,D)
        for (dd in 1:D){
            mean.gg[dd] <- sum( w * theta.k[,dd] )
        }
        for (dd1 in 1:D){
            for (dd2 in dd1:D){
                Sigma.gg[dd1,dd2] <- sum( w * (theta.k[,dd1] - mean.gg[dd1] )*(theta.k[,dd2] - mean.gg[dd2] ) )
                Sigma.gg[dd2,dd1] <- Sigma.gg[dd1,dd2]
            }
        }
        mg <- mean.gg
        sdg <- Sigma.gg
    }
    #---- output
    res <- list( Mu=mg, Sigma=sdg)
    return(res)
}
