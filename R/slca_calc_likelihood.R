## File Name: slca_calc_likelihood.R
## File Version: 0.02

slca_calc_likelihood <- function(G, use.freqpatt, ind.group, p.xi.aj, pi.k, weights )
{
	ll <- 0
	for (gg in 1:G){ 
		#-- do not use frequency pattern
		if ( ! use.freqpatt ){
			ind.gg <- ind.group[[gg]]
			ll <- ll + sum( weights[ind.gg] * log( rowSums( p.xi.aj[ind.gg,] * 
							    matrix( pi.k[,gg] , nrow=length(ind.gg) , ncol=nrow(pi.k) , byrow=TRUE ) ) ) )
		}
		#-- use frequency pattern
		if ( use.freqpatt ){
			if (G>1){  
				wgg <- weights[,gg]  
			}
			if (G==1){ 
				wgg <- weights 
			}
			ll <- ll + sum( wgg * log( rowSums( p.xi.aj * matrix( pi.k[,gg] , nrow= nrow(p.xi.aj) , 
											ncol=nrow(pi.k) , byrow=TRUE ) ) ) )
		}							
	}
	return(ll)
}
