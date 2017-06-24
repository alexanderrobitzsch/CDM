


###########################################################################
# reduced skillspace estimation
gdm_est_skillspace <- function(Ngroup, pi.k , Z, G , delta , eps=1E-10 ){		
		# gg <- 1
	covdelta <- as.list(1:G)
	for (gg in 1:G){
		ntheta <- Ngroup[gg] * pi.k[,gg]
		ntheta <- ntheta / sum(ntheta )		
		lntheta <- log(ntheta+eps)
		mod <- stats::lm( lntheta ~ 0 + Z , weights = ntheta )
		covbeta <- vcov(mod)		
		beta <- coef(mod)		
		pi.k[,gg] <- exp( Z %*% beta ) / Ngroup[gg]
		pi.k[,gg] <- pi.k[,gg] / sum( pi.k[,gg] )
		delta[,gg] <- beta
		covdelta[[gg]] <- covbeta
	}	
	#--- OUTPUT
	res <- list( pi.k=pi.k , delta=delta , covdelta = covdelta )			
	return(res)
}
			

.gdm.est.skillspace <- gdm_est_skillspace