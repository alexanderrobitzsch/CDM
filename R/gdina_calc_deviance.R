## File Name: gdina_calc_deviance.R
## File Version: 0.01
## File Last Change: 2017-06-04 19:15:46

gdina_calc_deviance <- function( p.xi.aj , attr.prob, item.patt.freq, loglike, G, IP)
{
	eps <- 1E-30
	# calculate the updated likelihood    
	p.xi.aj[ p.xi.aj > 1 ] <- 1 - eps
	p.xi.aj[ p.xi.aj < 0 ] <- eps	
	if (G==1){ 	
		l1 <- rowSums( p.xi.aj * outer( rep(1,IP), attr.prob )  ) + eps
		l1[ l1 < 0 ] <- eps
	}
	if (G>1){
		l1 <- matrix( 0 , IP , G )
		for (gg in 1:G){ 
			l1[,gg] <- rowSums( p.xi.aj * outer( rep(1,IP), attr.prob[,gg] )  ) + eps
			l1[ l1[,gg] < 0 ,gg] <- eps
		}
	}
	like.new <- sum( log( l1 ) * item.patt.freq ) 
    likediff <- abs( loglike - like.new )
	#--- OUTPUT
	res <- list( like.new=like.new, likediff=likediff)
	return(res)
}
