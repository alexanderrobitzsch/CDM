## File Name: gdina_itemfit.R
## File Version: 0.01
## File Last Change: 2017-06-05 15:17:44

gdina_itemfit <- function( L, J, R.lj, I.lj, item.patt.freq , G , attr.prob, data, pjM )
{
	# # n.ik [ 1:TP , 1:I , 1:(K+1) , 1:G ]
	n.ik <- array( 0 , dim=c(L , J , 2 , 1 ) )
	n.ik[  , , 2 , 1 ] <- t(R.lj)
	n.ik[  , , 1 , 1 ] <- t(I.lj-R.lj)
	pi.k <- array( 0 , dim=c(L,1) )
	
	if (G>1){	# item fit only in multiple group case
		g1 <- colSums( item.patt.freq )
		g1 <- g1 / sum(g1) 
		for (gg in 1:G){		
			pi.k[,1] <- pi.k[,1] + attr.prob[,gg] * g1[gg]
		}
	} 
		
	if (G==1){	# item fit only in one group case
		pi.k[,1] <- attr.prob$class.prob
	} 
	probs <- aperm( pjM , c(3,1,2) )
	itemfit.rmsea <- itemfit.rmsea( n.ik , pi.k , probs )$rmsea	
	names(itemfit.rmsea) <- colnames(data)
	#--- OUTPUT
	res <- list( itemfit.rmsea=itemfit.rmsea, pi.k=pi.k, n.ik=n.ik, pi.k=pi.k)
	return(res)
}
