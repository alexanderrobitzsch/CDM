## File Name: reglca_calc_counts.R
## File Version: 0.01


#----- calculation of expected counts
reglca_calc_counts <- function(weights, dat, dat.resp, p.aj.xi, K, n.ik, TP, I, dat.ind2 )
{
	N.ik <- matrix( 0 , nrow=TP, ncol=I)
	n.ik <- array( 0 , dim=c(TP,I,K+1 ) ) 
	for (kk in 1:(K+1)){
        dkk2 <- dat.ind2[[kk]]
		n.ik[,,kk] <- crossprod( p.aj.xi , dkk2 )
		N.ik <- N.ik + n.ik[,,kk]
	}
	#----- output
	res <- list(n.ik = n.ik , N.ik = N.ik)
	return(res)
}

	
