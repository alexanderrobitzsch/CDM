## File Name: reglca_proc_data.R
## File Version: 0.09

reglca_proc_data <- function( dat, weights, group )
{
	dat0 <- dat
	N <- nrow(dat)
	I <- ncol(dat)
	dat_resp <- 1 - is.na(dat)
	dat[ ! dat_resp ] <- 0
	dat <- as.matrix(dat)
	resp.ind.list <- list( 1:I )
	for (ii in 1:I){ 
		resp.ind.list[[ii]] <- which( dat_resp[,ii] == 1)  
	}
	if ( is.null(weights) ){
		weights <- rep(1,N)
	}		
	W <- sum(weights)
		
	#--- groups
	if (is.null(group)){
		G <- 1
		groups_unique <- NULL
		ind_groups <- NULL
		N_groups <- NULL
	} else {	
		groups_unique <- sort( unique(group))
		group <- match( group, groups_unique) 	
		G <- length(groups_unique)
		N_groups <- rep(NA, G)
		ind_groups <- list()
		W <- rep(0,G)
		for (gg in 1:G){
			ind_gg <- which( group == groups_unique[gg] )
			ind_groups[[gg]] <- ind_gg
			N_groups[gg] <- length(ind_gg)
			W[gg] <- sum( weights[ind_gg] )
		}
	}
	
	#--- indicator datasets
	K <- max(dat)
	dat.ind2 <- list()	
	for (kk in 1:(K+1)){
		r1 <- ( dat == kk-1 ) * ( dat_resp ) * weights
		dat.ind2[[kk]] <- r1
	}		
	
	#---- output
	res <- list( dat=dat, dat_resp=dat_resp, resp.ind.list=resp.ind.list, dat0=dat0, I=I,
					weights=weights, N=N, W=W, K=K, dat.ind2=dat.ind2, G=G, group=group,
					groups_unique = groups_unique, ind_groups=ind_groups, N_groups=N_groups )
	return(res)
}
