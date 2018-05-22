## File Name: gdina_attr_rpf_hogdina.R
## File Version: 0.19


####################################
# function for calculating attribute response function
gdina_attr_rpf_hogdina <- function( attr.patt, attr.prob, theta.k, wgt.theta, HOGDINA,
        tetrachoric=NULL )
{
    #- use weights for calculation of tetrachoric correlation
    wc <- cdm_tetrachoric( dat=attr.patt, weights=attr.prob, rho_init=tetrachoric$rho,
                    maxit=200 )
    b <-  wc$tau
    NB <- length(b)
    TP <- length(theta.k)
    NAP <- nrow(attr.patt)
    if (HOGDINA>0){
        upper_bound <- .99
        L <- cdm_fa1( Sigma=wc$rho, method=1 )$L
        L <- as.vector( L )
        L <- ifelse( L > upper_bound, upper_bound, L )
        L <- L / ( max(1,max(L)) + .0025 )
        L1 <- L / sqrt(  1 - L^2  )

    } else {
        L1 <- L <- rep(0,NB)
    }
    b1 <- b / sqrt( 1-L^2 )
    # calculate probabilities using the factor model
    probs <- stats::pnorm( L1 * matrix( theta.k, nrow=NB, ncol=TP, byrow=TRUE) - b1 )
    probsL <- array( 0, dim=c( NB, 2, TP ) )
    probsL[,2,] <- probs
    probsL[,1,] <- 1 - probs
    # probsL
    probsAP <- array( 1, dim=c( NAP, TP) )
    for (kk in 1:NB){
        probsAP <- probsAP * probsL[ kk, attr.patt[,kk] + 1, ]
    }
    # expected attribute probabilities
    attr.prob.exp <- rowSums( probsAP * matrix( wgt.theta, nrow=NAP, ncol=TP, byrow=TRUE ) )
    res <- list( a.attr=L1, b.attr=b1, attr.prob.exp=attr.prob.exp,
                    tetrachoric=wc)
    return(res)
}
#####################################################################

.attr.rpf <- gdina_attr_rpf_hogdina
