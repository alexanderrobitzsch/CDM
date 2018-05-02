## File Name: gdm_est_b.R
## File Version: 0.39

###########################################################################
# estimation of b parameters
gdm_est_b <- function(probs, n.ik, N.ik, I, K, G, b, b.constraint,
    max.increment, a, thetaDes, Qmatrix, TP, TD, msteps, convM,
    centerintercepts, decrease.increments=TRUE )
{

    max.increment0 <- max.increment
    iter <- 1
    parchange <- 1
    eps <- 1E-10
    b00 <- b
    while( ( iter <= msteps ) & ( parchange > convM)  ){
        b0 <- b
        probs <- gdm_calc_prob( a=a, b=b, thetaDes=thetaDes, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD )
        d2.b <- d1.b <- matrix( 0 , nrow=I,ncol=K)
        for (kk in 2:(K+1)){
            probs_kk <- probs[,kk,]
            for (gg in 1:G){
                t_Nik_gg <- t(N.ik[,,gg])
                d1.b[,kk-1] <- d1.b[,kk-1] - rowSums( t(n.ik[,,kk,gg]) - t_Nik_gg * probs_kk )
                d2.b[,kk-1] <- d2.b[,kk-1]  + rowSums( t_Nik_gg * ( 1 - probs_kk ) * probs_kk )
            }
        }
        #--- calc increments
        res <- cdm_calc_increment( d1=-d1.b, d2=d2.b, max.increment=max.increment )
        increment <- res$increment
        max.increment <- res$max.increment
        b <- b + increment
        se.b <- sqrt( 1 / abs( d2.b + eps ) )

        #-- parameter fixings
        res <- cdm_include_fixed_parameters( parm=b, se_parm=se.b, parm_fixed=b.constraint )
        b <- res$parm
        se.b <- res$se_parm

        #-- centerintercepts
        b <- gdm_est_b_centerintercepts( b=b, centerintercepts=centerintercepts, TD=TD, Qmatrix=Qmatrix )

        iter <- iter + 1
        parchange <- max( abs(b0-b))
    }
    max.increment.b <- max( abs( b - b00 ))

    #-- final trimming of the increment
    res <- cdm_increment_trimming_after_mstep( parm=b, parm0=b00, max.increment0=max.increment0, type=2 )
    b <- res$parm
    max.increment.b <- res$max.increment0

    #--- decrease increments
    if (decrease.increments){
        max.increment.b <- max.increment.b/1.01
    }
    #--- OUTPUT
    res <- list( b=b, se.b=se.b , max.increment.b=max.increment.b)
    return(res)
}


.gdm.est.b <- gdm_est_b
