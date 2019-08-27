## File Name: gdina_calc_prob.R
## File Version: 0.18

gdina_calc_prob <- function( progress, iter, disp, J, L, aggr.attr.patt, Mj, 
        delta, linkfct)
{
    if ( progress ){
        cat(disp)
        cat("Iteration", iter, "   ", paste( Sys.time() ), "\n" )
    }
    pj1 <- matrix( 0, nrow=J, ncol=L )
    #---- calculate P(X_j | alpha_l )
    for (jj in 1:J){
        ajj <- aggr.attr.patt[[jj]]
        mjjj <- Mj[[jj]][[1]]
        djj <- cdm_matrix2( delta[[jj]], nrow=nrow(mjjj) )
        pj11 <- rowSums( mjjj * djj )
        if (linkfct=="logit"){
            pj11 <- stats::plogis( pj11 )
        }
        if (linkfct=="log"){
            pj11 <- exp(pj11)
        }
        pj1[jj,] <- pj11[ ajj ]
    }
    #-- restrict probabilities in calculations
    eps <- 1E-10
    pj1[ pj1 < 0 ] <- eps
    pj1[ pj1 > 1 ] <- 1 - eps
    #-- create array
    pjM <- array( NA, dim=c(J,2,L) )
    pjM[,1,] <- 1 - pj1
    pjM[,2,] <- pj1
    #--- OUTPUT
    return(pjM)
}


# cat( "\n Step 1a (calculate P(X_j|alpha_l)\n" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1
