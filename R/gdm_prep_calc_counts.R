## File Name: gdm_prep_calc_counts.R
## File Version: 0.02

gdm_prep_calc_counts <- function(K, G, group, weights, dat.resp, dat.ind, use.freqpatt)
{
	dat.ind2 <- as.list( 1:(K+1) )
	ind.group <- as.list( 1:G )
	for (kk in 1:(K+1)){ 
		l1 <- as.list(1:G)
		for (gg in 1:G){
			if ( ! use.freqpatt ){
				ind.gg <- which( group == gg )
				ind.group[[gg]] <- ind.gg
				dkk <- (dat.ind[[kk]])[ ind.gg , ]
				l1[[gg]] <- dkk * dat.resp[ind.gg,] * weights[ind.gg] 	
			}
			if ( use.freqpatt ){
				dkk <- dat.ind[[kk]]
				if (G>1){
					wgg <- weights[,gg]	 
				}
				if (G==1){ 
					wgg <- weights 
					ind.group[[gg]] <- which( group==gg)
				}
				l1[[gg]] <- dkk * dat.resp * wgg 
			}
		}   # end gg
		dat.ind2[[kk]] <- l1
	}
	#-------- output
	res <- list(ind.group=ind.group, dat.ind2=dat.ind2)
	return(res)
}
