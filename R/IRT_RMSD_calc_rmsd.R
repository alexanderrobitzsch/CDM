## File Name: IRT_RMSD_calc_rmsd.R
## File Version: 3.15

	
##########################################
# auxiliary function RMSD calculation
IRT_RMSD_calc_rmsd <- function( n.ik , pi.k , probs , eps=1E-30 ){	
	#*** compute distributions based on expected counts
	res <- IRT_RMSD_calc_distributions( n.ik = n.ik, pi.k=pi.k , eps = eps  )
	pi.k_tot <- res$pi.k_tot
	p.ik_observed <- res$p.ik_observed
	maxK <- res$maxK
	K <- res$K	
	N.ik <- res$N.ik
	eps1 <- 1
    #------------------------------------------------	
	#*** RMSD fit statistic
	dist.item <- pi.k_tot * ( p.ik_observed - probs )^2	
	h1 <- IRT_RMSD_proc_dist_item(dist.item = dist.item)
	RMSD <- sqrt( colSums( h1 ) / maxK )			
	#*** bias corrected RMSD statistic
	N.it <- array(0 , dim = dim(N.ik) )
	K <- dim(N.ik)[3]
	for (kk in 1:K){
		N.it[ ,, 1] <- N.it[,,1] + N.ik[,,kk]
	}
	for (kk in 2:K){
		N.it[ ,, kk] <- N.it[,,1]
	}
	N.it <- N.it #  + 1
	p.ik_obs_var <- probs * ( 1 - probs ) / N.it
	p.ik_samp_var <- pi.k_tot^2 * p.ik_obs_var	
	h2 <- IRT_RMSD_proc_dist_item(dist.item = p.ik_samp_var)
	h2 <- colSums( h2 ) / maxK 
	RMSD_bc <- sqrt( ifelse( RMSD^2 - h2 < 0 , 0 , RMSD^2 - h2 ) )
	
    #------------------------------------------------	
	#*** MD fit statistic
	dist.item <- pi.k_tot * ( p.ik_observed - probs )
	h1 <- IRT_RMSD_proc_dist_item(dist.item = dist.item)
	MD <- colSums( h1 ) / ( maxK - 1)
    #------------------------------------------------	
	#*** MAD fit statistic
	dist.item <- pi.k_tot * abs( p.ik_observed - probs )
	h1 <- IRT_RMSD_proc_dist_item(dist.item = dist.item)
	MAD <- colSums( h1 ) / maxK

	#--- output
	res <- list( RMSD = RMSD , RMSD_bc = RMSD_bc,  MD = MD, MAD = MAD )	
	return(res)
}

rmsea_aux <- function( n.ik , pi.k , probs , eps = 1E-30 )
{
	res <- IRT_RMSD_calc_rmsd( n.ik = n.ik , pi.k = pi.k , probs = probs , eps = eps )
	return( res$RMSD )
}

.rmsea.aux <- rmsea_aux
