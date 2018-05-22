## File Name: WaldTest.R
## File Version: 0.13
#################################################
# helper function Wald test
WaldTest <- function( delta, vcov, R, nobs, cvec=NULL, eps=1E-10 )
{
    NR <- nrow(R)
    if ( is.null(cvec) ){
        cvec <- rep( 0, NR )
    }
    Rdii <- R %*% delta - cvec
    v1 <- R %*% vcov %*% t(R)
    diag(v1) <- diag(v1) * ( 1 + eps )
    stat <- ( t( Rdii ) %*% solve( v1 ) %*% Rdii )[1,1]
    stats <- list()
    stats["X2"] <- stat
    stats["df"] <- NR
    stats["p"] <- stats::pchisq( stat, df=NR, lower.tail=FALSE)
    l1 <- stats$X2 / stats$df - 1
    l1 <- if ( l1 < 0 ){ 0 } else { sqrt(l1 / ( nobs - 1 ))    }
    stats["RMSEA"] <- l1
    return(stats)
}
##############################################
