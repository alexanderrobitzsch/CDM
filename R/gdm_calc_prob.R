## File Name: gdm_calc_prob.R
## File Version: 0.01
## File Last Change: 2017-06-12 13:10:13

#######################################
# calculate probability in the GDM		
gdm_calc_prob <- function( a, b, thetaDes, Qmatrix, I, K, TP, TD)
{
	probs <- array( 0 , dim=c(I,K+1,TP) )	# categories 0 , ... , K
	for (kk in 1:K){
		l0 <- matrix( b[,kk] , nrow=I,ncol=TP)
		for (td in 1:TD){ 	# kk <- 1	# category 1
			# td <- 1
			l0 <- l0 + a[ , td , kk ] * Qmatrix[ , td, kk] * matrix( thetaDes[ , td ] , nrow=I,ncol=TP , byrow=T)
		}
		probs[,kk+1,] <- l0
	}
	probs <- exp( probs )
	probs1 <- probs[,1,]
	for (kk in 2:(K+1)){ 
		probs1 <- probs1 + probs[,kk,] 
	}
	for (kk in 1:(K+1)){ 
		probs[,kk,] <- probs[,kk,] / probs1 
	}
	return(probs)
}

.gdm.calc.prob <- gdm_calc_prob
