## File Name: reglca_dpm_smoothing.R
## File Version: 0.02

reglca_dpm_smoothing <- function( p.aj.xi, weights, nclasses, alpha, dpm_maxit=10, dpm_conv=1E-4 )
{
	freq_classes <- colSums( p.aj.xi * weights )		
	# update vh
	iterate <- TRUE
	eps <- 1E-20
	iter <- 0
	vh <- rep(1,nclasses)
	while( iterate ){
		alpha0 <- alpha	
		vh0 <- vh		
		for (tt in 1:nclasses){
			vh[tt] <- freq_classes[tt] / ( sum( freq_classes[ seq(tt,nclasses) ] ) + alpha - 1 )
		}
		ind <- which( vh > 1 - eps )
		if ( length(ind)>0){
			vh[ seq( ind, nclasses) ] <- 1 - eps
		}
		# update alpha
		alpha <- ( 1 - nclasses ) / sum( log( 1 - vh[ seq(1, nclasses - 1 ) ] ) )
		iter <- iter + 1
		parm_change <- max( c( abs( alpha - alpha0 ) , abs( vh - vh0 ) ))
		if ( iter == dpm_maxit ){ iterate <- FALSE }
		if ( parm_change < dpm_conv ){ iterate <- FALSE }
	}		
	class_probs <- dpm_calc_probs( vh=vh )
	#--- output
	res <- list( alpha=alpha, class_probs=class_probs)
	return(res)
}
	
