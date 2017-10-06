## File Name: slca_inits_skill_distribution.R
## File Version: 0.02
## File Last Change: 2017-10-06 11:02:19

slca_inits_skill_distribution <- function( delta.designmatrix, delta.init, 
		delta.linkfct, G, K, I )
{
	TP <- nrow(delta.designmatrix)

	if (  ! is.null(delta.init) ){	
		delta <- delta.init
		if ( delta.linkfct == "log"){
			pik <- exp( delta.designmatrix %*% delta.init[,1] )
		} else {
			pik <- stats::plogis( delta.designmatrix %*% delta.init[,1] )
		}
	} else {
		pik <- cdm_sumnorm( rep( 1 /TP , TP ) + stats::runif(TP , 0 , .5 ) )
		if ( delta.linkfct == "logit"){
			g1 <- solve( crossprod( delta.designmatrix )) %*% t( delta.designmatrix) %*% pik				
			delta <- matrix( g1[,1] , nrow=ncol(delta.designmatrix) , ncol=G)
		} else {
			delta <- matrix( 0 , ncol(delta.designmatrix) , G )		
			delta[1,] <- 1		
		}
	}

	pi.k <- matrix( 0 , TP , G )
	for (gg in 1:G){ 
		pi.k[,gg] <- pik 
	}
	n.ik <- array( 0 , dim=c(TP,I,K+1,G) )	
	#--------- output
	res <- list(TP=TP, n.ik=n.ik, pi.k=pi.k, delta=delta)
	return(res)
}
