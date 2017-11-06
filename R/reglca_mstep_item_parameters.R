## File Name: reglca_mstep_item_parameters.R
## File Version: 0.26


reglca_mstep_item_parameters <- function(I, n.ik, N.ik, h, mstep_iter, conv, regular_lam,
		regular_type, cd_steps, item_probs, max_increment, iter, fac=1.02 )
{
	penalty <- rep(0,I)
	n_par <- rep(0,I)
	n_reg <- 0
	n_reg_item <- rep(0,I)
	nclasses <- ncol(item_probs)
	item_probs0 <- item_probs	
	for (ii in 1:I){	
		freq <- n.ik[,ii,2] / N.ik[,ii]
		res <- reglca_fit_probabilities( freq=freq, h=h, maxit=mstep_iter, conv=conv, verbose=FALSE,
							parm_init = NULL, lambda = regular_lam, regular_type=regular_type,
							cd_steps=cd_steps, max_increment=max_increment)						
		incr <- res$probs - item_probs[ii,]
		incr <- cdm_trim_increment( increment=incr, max.increment=max_increment, type=1 )
		item_probs[ii,] <- item_probs[ii,] + incr
		penalty[ii] <- sum(N.ik[,ii]) * res$pen
		n_par[ii] <- res$n_par
		n_reg_item[ii] <- nclasses - res$n_par
		n_reg <- n_reg + nclasses - res$n_par
	}

	penalty <- sum(penalty)
	n_par <- sum(n_par)
	max_increment <- min( max_increment, max( abs( item_probs - item_probs0 ) )	 ) / fac
	# max_increment <- max( abs( item_probs - item_probs0 ) )
	# max_increment <- max_increment / fac
	# max_increment <- .1
	#--- output
	res <- list( item_probs=item_probs, penalty=penalty, n_par=n_par, n_reg=n_reg, max_increment=max_increment,
						n_reg_item=n_reg_item)
	return(res)	
}
		
