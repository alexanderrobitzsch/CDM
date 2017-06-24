
gdina_calc_prob <- function( progress, iter, disp , J , L , aggr.attr.patt, Mj, delta,
		linkfct)
{
	if ( progress ){
		cat(disp)	
		cat("Iteration" , iter , "   " , paste( Sys.time() ) , "\n" )	   
	}
	pj1 <- matrix( 0 , nrow = J , ncol = L )
	#---- calculate P(X_j | alpha_l )
	for (jj in 1:J){
		ajj <- ( aggr.attr.patt[[jj]] )
		mjjj <- Mj[[jj]][[1]][ ajj , ]
		djj <- matrix( delta[[jj]] , L , length(delta[[jj]]) , byrow=TRUE )
		pj1[jj,] <- rowSums( mjjj * djj )
		if (linkfct == "logit"){
			pj1[jj,] <- stats::plogis( pj1[jj,] )
		}
		if (linkfct == "log"){
			pj1[jj,] <- exp( pj1[jj,] )
		}									
	}									
	#-- restrict probabilities in calculations									
	eps <- 1E-10
	pj1[ pj1 < 0 ] <- eps
	pj1[ pj1 > 1 ] <- 1 - eps		
# cat( "\n Step 1a (calculate P(X_j|alpha_l)\n" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1
	#-- create array								
	pjM <- array( NA , dim=c(J,2,L) )
	pjM[,1,] <- 1 - pj1
	pjM[,2,] <- pj1		
	#--- OUTPUT
	return(pjM)			
}		