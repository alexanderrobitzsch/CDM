## File Name: gdm_inits_b.R
## File Version: 0.02


gdm_inits_b <- function( dat0, dat.resp0, I , K )
{
    b <- matrix( 0 , nrow=I , ncol=K )
    for (ii in 1:K){    # ii <- 1
        cm1 <- colMeans( ( dat0 >= ii )*dat.resp0 )
        b[,ii] <-  stats::qlogis( ( cm1 + .01 ) / 1.02 )
    }
    #--- output
    return(b)
}
