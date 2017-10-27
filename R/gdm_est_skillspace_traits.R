## File Name: gdm_est_skillspace_traits.R
## File Version: 0.09

#####################################################
# estimation of skill space
gdm_est_skillspace_traits <- function( n.ik , a , b , theta.k , Qmatrix , I , K , TP,
		TD , numdiff.parm , max.increment , msteps , convM )
{
	n.ik0 <- apply( n.ik , c(1,2,3) , sum )
	h <- numdiff.parm
	parchange <- 1000
	iter <- 1
	se.theta.k <- 0 * theta.k	
	Q1 <- matrix( 0 , nrow=TP , ncol=TD)
	
	#-- define likelihood function and list of arguments
	prob_fct <- gdm_calc_prob
	prob_args <- list( a=a, b=b, thetaDes=theta.k, Qmatrix=Qmatrix, I=I, K=K, TP=TP, TD=TD ) 
	parm_args_varname <- "thetaDes"
	
	#--------- begin M-steps
	while( ( iter <= msteps ) & (parchange > convM ) ){
		theta.k0 <- theta.k
		for ( dd in 1:TD){	
			Q0 <- Q1
			Q0[,dd] <- 1
			# calculate log-likelihood
			prob_args[[ parm_args_varname ]] <- theta.k0
			pjk <- do.call( what=prob_fct, args=prob_args)

			prob_args[[ parm_args_varname ]] <- theta.k0 + h*Q0
			pjk1 <- do.call( what=prob_fct, args=prob_args)

			prob_args[[ parm_args_varname ]] <- theta.k0 - h*Q0
			pjk2 <- do.call( what=prob_fct, args=prob_args)

			#-- compute increments
			res <- gdm_numdiff_index( pjk=pjk, pjk1=pjk1, pjk2=pjk2, n.ik=n.ik0, max.increment=max.increment, 
						numdiff.parm=numdiff.parm ) 
			increment <- res$increment						
			d2 <- res$d2
			theta.k[,dd] <- theta.k[,dd] + increment		
			se.theta.k[,dd] <- 1 / sqrt( abs(d2) )
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
