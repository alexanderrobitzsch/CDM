
#####################################################
# estimation of skill space
gdm_est_skillspace_traits <- function( n.ik , a , b , theta.k , Qmatrix , I , K , TP,
		TD , numdiff.parm , max.increment , msteps , convM ){
	# sum over groups
	n.ik0 <- apply( n.ik , c(1,2,3) , sum )
	h <- numdiff.parm
	parchange <- 1000
	iter <- 1
	se.theta.k <- 0 * theta.k
	Q1 <- matrix( 0 , nrow=TP , ncol=TD)
	while( ( iter <= msteps ) & (parchange > convM ) ){
		theta.k0 <- theta.k
		for ( dd in 1:TD){	
			Q0 <- Q1
			Q0[,dd] <- 1
			# calculate log-likelihood
			pjk <- gdm_calc_prob( a=a, b=b, thetaDes=theta.k, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD ) 
			theta.k1 <- theta.k + h*Q0
			pjk1 <- gdm_calc_prob( a=a, b=b, thetaDes=theta.k1, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD ) 
			theta.k2 <- theta.k - h*Q0
			pjk2 <- gdm_calc_prob( a=a, b=b, thetaDes=theta.k2, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD ) 
			res <- gdm_numdiff_index( pjk=pjk, pjk1=pjk1, pjk2=pjk2, n.ik=n.ik0, max.increment=max.increment, 
						numdiff.parm=numdiff.parm, eps=1E-80 ) 
			theta.k[,dd] <- theta.k[,dd] + res$increment		
			se.theta.k[,dd] <- 1 / sqrt( abs(res$d2) )
		}
		iter <- iter + 1 
		parchange <- max( abs( theta.k - theta.k0 ))
	}	
	
	#--- OUTPUT
	res <- list( theta.k = theta.k , se.theta.k = se.theta.k )
	return(res)
	}
##########################################################

.gdm.est.skillspace.traits <- gdm_est_skillspace_traits