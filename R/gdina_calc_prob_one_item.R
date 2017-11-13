## File Name: gdina_calc_prob_one_item.R
## File Version: 0.01

gdina_calc_prob_one_item <- function( progress, iter, disp , J , jj, L , aggr.attr.patt, Mj, delta,
		linkfct)
{
	if ( progress ){
		cat(disp)	
		cat("Iteration" , iter , "   " , paste( Sys.time() ) , "\n" )	   
	}
	pj1 <- matrix( 0 , nrow = 1 , ncol = L )
	#---- calculate P(X_j | alpha_l )
	ajj <- ( aggr.attr.patt[[jj]] )
	mjjj <- Mj[[jj]][[1]][ ajj , ]
	djj <- matrix( delta[[jj]] , nrow=L , ncol=length(delta[[jj]]) , byrow=TRUE )
	pj1[1,] <- rowSums( mjjj * djj )
	if (linkfct == "logit"){
		pj1[1,] <- stats::plogis( pj1[1,] )
	}
	if (linkfct == "log"){
		pj1[1,] <- exp( pj1[1,] )
	}
	#-- restrict probabilities in calculations
	eps <- 1E-10
	pj1[ pj1 < 0 ] <- eps
	pj1[ pj1 > 1 ] <- 1 - eps
	#-- create array
	pjM <- array( NA , dim=c(1,2,L) )
	pjM[,1,] <- 1 - pj1
	pjM[,2,] <- pj1		
	pjM <- pjM[1,,]
	#--- OUTPUT
	return(pjM)			
}		


# cat( "\n Step 1a (calculate P(X_j|alpha_l)\n" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1
