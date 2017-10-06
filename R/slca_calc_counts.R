## File Name: slca_calc_counts.R
## File Version: 0.01
## File Last Change: 2017-10-03 18:56:57


################################################	
# calculation of expected counts
slca_calc_counts <- function(G, weights, dat.ind, dat, dat.resp,
			p.aj.xi, K, n.ik, TP,I,group , dat.ind2 , ind.group , use.freqpatt )
{
	N.ik <- array( 0 , dim=c(TP,I,G) )
	N.ik1 <- array( 0 , dim=c(TP,I) )
	n.ik1 <- array( 0 , dim=c(TP,I,K+1 ) ) 
	#-------------------------------------
	#--- single group
    if (G==1){
		gg <- 1
		for (kk in 1:(K+1)){   #		kk <- 1	# category 0 ( -> 1 )
            dkk2 <- dat.ind2[[kk]][[gg]]
			n.ik[,,kk,gg] <- crossprod( p.aj.xi , dkk2 )
			n.ik1[,,kk] <- n.ik[,,kk,gg]
			N.ik[,,gg] <- N.ik[,,gg] + n.ik[,,kk,gg]
		}
        N.ik1 <- N.ik1 + N.ik[,,gg]						
	}
	#-------------------------------------
	#--- multiple groups	
	if (G>1){
		for (gg in 1:G){	# gg <- 1
			if ( ! use.freqpatt ){	
				ind.gg <- ind.group[[gg]]
				t.p.aj.xi.gg <- t( p.aj.xi[ind.gg,] )
			} 
			if (  use.freqpatt ){	
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
				n.ik1[,,kk] <- n.ik1[,,kk] + n.ik[,,kk,gg]
				N.ik[,,gg] <- N.ik[,,gg] + n.ik[,,kk,gg]
			}						
			N.ik1 <- N.ik1 + N.ik[,,gg]
		}  # end gg
	}  # end multiple group
	#----- output
	res <- list(n.ik = n.ik , N.ik = N.ik , n.ik1 = n.ik1, N.ik1 = N.ik1)
	return(res)
}

	

.slca.calc.counts <- slca_calc_counts
