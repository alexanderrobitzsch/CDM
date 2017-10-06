## File Name: gdm_est_b.R
## File Version: 0.06
## File Last Change: 2017-10-06 10:32:50

###########################################################################
# estimation of b parameters
gdm_est_b <- function(probs, n.ik, N.ik, I, K, G, b, b.constraint,
	max.increment, a, thetaDes, Qmatrix, TP, TD, msteps, convM,
	centerintercepts, decrease.increments=TRUE )
{		
 	max.increment <- 1
	iter <- 1
	parchange <- 1
	eps <- 1E-10
	b00 <- b	
	while( ( iter <= msteps ) & ( parchange > convM)  ){
		b0 <- b
		probs <-  gdm_calc_prob( a=a, b=b, thetaDes=thetaDes, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD ) 
		d2.b <- d1.b <- matrix( 0 , nrow=I,ncol=K)				
		for (kk in 2:(K+1)){
			for (gg in 1:G){
				d1.b[,kk-1] <- d1.b[,kk-1] - rowSums( t(n.ik[,,kk,gg]) - t(N.ik[,,gg]) * probs[,kk,] )
				d2.b[,kk-1] <- d2.b[,kk-1]  + rowSums( t(N.ik[,,gg]) * ( 1 - probs[,kk,] ) * probs[,kk,] )
			}
		}		
		increment <-  - d1.b / ( abs( d2.b + eps ) )
		increment[ is.na(increment) ] <- 0		
		increment <- ifelse(abs(increment)> max.increment, 
					sign(increment)*max.increment , increment )						
		max.increment <- max(abs(increment)) / .98
		b <- b + increment
		se.b <- sqrt( 1 / abs( d2.b + eps ) )
		if ( ! is.null( b.constraint) ){
			b[ b.constraint[,1:2,drop=FALSE] ] <- b.constraint[,3,drop=FALSE]
			se.b[ b.constraint[,1:2,drop=FALSE] ] <- 0		
		}
		# centerintercepts
		if ( centerintercepts ) {
			if (TD==1){
				b <- b - mean(b)		
			}
			if (TD > 1){		
				for (dd in 1:TD){
					ind.dd <- which( Qmatrix[,dd,1] > 0 )
					m1 <- sum( b[ind.dd,] ) / ( ncol(b) * length(ind.dd) )	
					b[ind.dd,] <- b[ind.dd,] - 	m1
				}
			 }
		}				
		iter <- iter + 1
		parchange <- max( abs(b0-b))
	}
	max.increment.b <- max( abs( b - b00 ))
	#--- decrease increments
	if (decrease.increments){ 
		max.increment.b <- max.increment.b/1.01	
	}	
	#--- OUTPUT
	res <- list( b=b, se.b=se.b , max.increment.b=max.increment.b)
	return(res)
}


.gdm.est.b <- gdm_est_b
