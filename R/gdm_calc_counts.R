## File Name: gdm_calc_counts.R
## File Version: 0.01
## File Last Change: 2017-06-12 13:18:50



################################################	
# calculation of expected counts
gdm_calc_counts <- function(G, weights, dat.ind, dat, dat.resp,
			p.aj.xi, K, n.ik, TP,I,group , dat.ind2 , ind.group ,
			use.freqpatt )
{
	# n.ik [ 1:TP , 1:I , 1:(K+1) , 1:G ]
	# N.ik [ 1:TP , 1:I ,  1:G ]
	N.ik <- array( 0 , dim=c(TP,I,G) )
    if (G==1){
		gg <- 1
		for (kk in 1:(K+1)){   #		kk <- 1	# category 0 ( -> 1 )
            dkk2 <- dat.ind2[[kk]][[gg]]
			n.ik[,,kk,gg] <- crossprod( p.aj.xi , dkk2 )
			N.ik[,,gg] <- N.ik[,,gg] + n.ik[,,kk,gg]
		}	
	}
	if (G>1){
		for (gg in 1:G){	# gg <- 1
			if ( ! use.freqpatt ){	
				ind.gg <- ind.group[[gg]]
				t.p.aj.xi.gg <- t( p.aj.xi[ind.gg,] )
			} 
			if ( use.freqpatt ){	
				t.p.aj.xi.gg <- t( p.aj.xi[[gg]] )
			} 					
			for (kk in 1:(K+1)){   #		kk <- 1	# category 0 ( -> 1 )
				dkk2 <- dat.ind2[[kk]][[gg]]
				if ( use.freqpatt ){
					if (G>1){ 
						dkk2 <- dkk2[ which(weights[,gg] > 0) , ] 
					}
				}
				n.ik[,,kk,gg] <- t.p.aj.xi.gg %*% dkk2
				N.ik[,,gg] <- N.ik[,,gg] + n.ik[,,kk,gg]
			}						
		}
	}
	#--- OUTPUT
	res <- list("n.ik" = n.ik , "N.ik" = N.ik )					
	return(res)
}

.gdm.calc.counts <-	gdm_calc_counts
