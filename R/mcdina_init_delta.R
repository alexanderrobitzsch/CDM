## File Name: mcdina_init_delta.R
## File Version: 0.01



###################################################
# initial estimate of item parameters delta
mcdina_init_delta <- function( lc, lr )
{
    I <- max( lc$item )
    lc$cats <- lc$cats
    CC <- max(lc$cats)
    delta_ideal <- delta <- array( 0, dim=c(I, CC, CC ) )
    delta_ideal[ as.matrix( lc[, c("item", "cats", "lr_index" ) ] ) ] <- 1
    eps <- 1E-10
    # define initial delta estimate
    for (ii in 1:I){
        dii <- delta_ideal[ii,,]
        lcii <- lc[ lc$item==ii, ]
        Ncii <- nrow(lcii)
        for (cc in 1:CC){
            dii.cc <- dii[,cc]
            delta[ii,,cc] <- dii.cc * ( 0.8 / sum( dii.cc + eps) ) +
                        (1-dii.cc) * ( .2 / sum( ( 1-dii.cc) + eps ) )
            if (Ncii < CC ){
                delta[ii,seq(Ncii+1,CC),cc] <- 0
                delta[ii,,cc] <- delta[ii,,cc] / sum( delta[ii,,cc] )
            }
            if ( sum( dii.cc )==0 ){ delta[ii,,cc] <- 0 }
        }
    }
    res <- list( "delta"=delta, "delta_ideal"=delta_ideal )
    return(res)
}
############################################################
