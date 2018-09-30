## File Name: sim.din.R
## File Version: 1.13
################################################################################
# utility function for data simulation from a CDM model                        #
################################################################################

sim.din <- function( N=0, q.matrix, guess=rep(.2, nrow(q.matrix) ), slip=guess,
    mean=rep(0, ncol(q.matrix) ), Sigma=diag( ncol(q.matrix) ), rule="DINA",
    alpha=NULL)
{
    # simulated normal variates
    if (N>0){
        normsim <- CDM_rmvnorm( N, mean, Sigma)
        # dichotomous variates
        dichsim <- 1 * ( normsim > 0 )
    }
    if ( ! is.null( alpha) ){
        dichsim <- alpha
        N <- nrow(alpha)
    }

    # number of possessed attributes, of those which are necessary for this item
    poss.attr <- dichsim %*% t( q.matrix )

    # calculate for each item how many attributes are necessary for solving the items
    # according to the specified DINA or DINO rule
    ness.attr <- ( rowSums(q.matrix)  )*( rule=="DINA") + 1* ( rule=="DINO" )
    # latent response
    eta.pp <- poss.attr >=outer( rep(1,N), ness.attr )
    # simulating responses according DINA rule
    R <- matrix( eta.pp * stats::rbinom( N*nrow(q.matrix), size=1, prob=1 - outer( rep(1,N), slip ) ) +
                ( 1 - eta.pp) * stats::rbinom( N*nrow(q.matrix), size=1,
                prob=outer( rep(1,N), guess ) ), ncol=nrow(q.matrix) )
    colnames(R) <- paste( "I", substring( 1000 + 1:( nrow(q.matrix) ), 2), sep="")
    res <- list( "dat"=R, "alpha"=dichsim )
    return(res)
}

