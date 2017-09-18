## File Name: IRT_RMSD_calc_distributions.R
## File Version: 0.02
## File Last Change: 2017-02-26 18:35:25

IRT_RMSD_calc_distributions <- function( n.ik , pi.k , eps = 1E-30 )
{
	# probs ... [ classes , items , categories ]
	# n.ik ... [ classes , items , categories , groups ]	
	# N.ik ... [ classes , items , categories]	
	N.ik <- n.ik[,,,1]
	G <- dim(n.ik)[4]
	pitot <- pi.k[,1]
	eps <- 1E-10
	if (G>1){ 
		for (gg in 2:G ){
			N.ik <- N.ik + n.ik[,,,gg]
			pitot <- pitot + pi.k[,gg]
		}
	}
	#*** extract maximum number of categories
	maxK <- apply( N.ik , c(2,3) , sum , na.rm=TRUE )
	maxK <- rowSums( maxK > eps )
	# calculate summed counts
	N.ik_tot <- array( 0 , dim=dim(N.ik) )
	N.ik_tot[,,1] <- N.ik[,,1,drop=FALSE]
	K <- dim(N.ik)[3]			
	for (kk in 2:K){
		N.ik_tot[,,1] <- N.ik_tot[,,1,drop=FALSE] + N.ik[,,kk,drop=FALSE] 
	}
	for (kk in 2:K){	
		N.ik_tot[,,kk] <- N.ik_tot[,,1] 
	}
		
	# calculate itemwise statistics
	p.ik_observed <- N.ik / ( N.ik_tot + eps )
	p.ik_observed <- replace_NA( p.ik_observed , value=0 )
	# define class weights 
	pi.k_tot <- array( 0 , dim= dim(p.ik_observed) )
	for (kk in 1:K){
		pi.k_tot[,,kk] <- matrix( pitot , nrow= dim(pi.k_tot)[1] , 
				ncol= dim(pi.k_tot)[2] , byrow=FALSE )
	}	
	#--- output
	res <- list( N.ik = N.ik , N.ik_tot = N.ik_tot , p.ik_observed = p.ik_observed , 
					pi.k_tot = pi.k_tot , maxK = maxK , K = K )
	return(res)	
}
		
