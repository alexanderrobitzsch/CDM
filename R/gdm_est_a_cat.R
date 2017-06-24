
###########################################
# estimation of a
gdm_est_a_cat <- function(probs, n.ik, N.ik, I, K, G,a,a.constraint,TD,
				Qmatrix,thetaDes,TP, max.increment ,
				b , msteps , convM, decrease.increments=TRUE  ){
	iter <- 1
	parchange <- 1
	a00 <- a
	eps <- 1E-10
	
	while( ( iter <= msteps ) & ( parchange > convM )  ){
		a0 <- a
		probs <- gdm_calc_prob( a=a, b=b, thetaDes=thetaDes, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD ) 
		# 1st derivative
		d2.b <- d1.b <- array( 0 , dim=c(I , TD , K ) )
		for (td in 1:TD){
			for (kk in 2:(K+1)){	
				for (gg in 1:G){
					QM <- matrix( Qmatrix[,td,kk-1] , nrow=TP , ncol=I, byrow=TRUE)
					v1 <- colSums( n.ik[,,kk,gg] * QM * thetaDes[ , td ] )
					v2 <- N.ik[,,gg] * QM * thetaDes[,td] *  t( probs[,kk,] )
					v2 <- colSums(v2)
					d1.b[  , td , kk-1] <- d1.b[  , td , kk-1] + v1 - v2
				}
			}	
		}	
		# 2nd derivative
		for (td in 1:TD){
			for (ii in 1:I){
				v1 <- l0 <- 0
				for (kk in 2:(K+1) ){		# kk <- 2
					v1 <- l0 <- 0
					for (gg in 1:G){			
						v1 <- N.ik[,ii,gg] * as.vector( ( Qmatrix[ii,td,kk-1] * 
									thetaDes[ , td ] )^2 * t( probs[ii,kk,] ) )
						l0 <- as.vector ( Qmatrix[ii,td,kk-1] * thetaDes[ , td ]  * t( probs[ii,kk,] ) )
						d2.b[ii,td,kk-1] <- d2.b[ii,td,kk-1] + sum(v1) - sum( l0^2 * N.ik[,ii,gg] )				
					}
				}				
			}
		}				
		increment <-  d1.b / abs( d2.b + eps )
		increment[ is.na(increment) ] <- 0		
		increment <- ifelse(abs(increment)> max.increment, 
						    sign(increment)*max.increment , increment )	
		a <- a + increment
		se.a <- sqrt( 1 / abs( d2.b + eps ) )
		if ( ! is.null( a.constraint) ){
			a[ a.constraint[,1:3,drop=FALSE] ] <- a.constraint[,4,drop=FALSE]
			se.a[ a.constraint[,1:3,drop=FALSE] ] <- 0		
			increment[ a.constraint[,1:3,drop=FALSE] ] <- 0						
		}
		iter <- iter + 1
		parchange <- max( abs( a - a0 ))
	} # iter
	#-------------
	max.increment.a <- max(abs(a-a00)) / .95				
	if (decrease.increments){ 	
		max.increment.a <- max.increment.a / 1.01				
	}	
	#---- OUTPUT
	res <- list( a = a , se.a = se.a , max.increment.a = max.increment.a)
	return(res)
}	


.gdm.est.a.cat <- gdm_est_a_cat