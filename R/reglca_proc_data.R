## File Name: reglca_proc_data.R
## File Version: 0.03

reglca_proc_data <- function( dat, weights )
{
	dat0 <- dat
	N <- nrow(dat)
	I <- ncol(dat)
	dat_resp <- 1 - is.na(dat)
	dat[ ! dat_resp ] <- 0
    resp.ind.list <- list( 1:I )
	for (ii in 1:I){ 
		resp.ind.list[[ii]] <- which( dat_resp[,ii] == 1)  
	}
	if ( is.null(weights) ){
		weights <- rep(1,N)
	}		
	W <- sum(weights)
	
	#--- indicator datasets
	K <- max(dat)
	dat.ind2 <- list()	
	for (kk in 1:(K+1)){
		r1 <- ( dat == kk-1 ) * ( dat_resp ) * weights
		dat.ind2[[kk]] <- r1
	}	
	#---- output
	res <- list( dat=dat, dat_resp=dat_resp, resp.ind.list=resp.ind.list, dat0=dat0, I=I,
					weights=weights, N=N, W=W, K=K, dat.ind2=dat.ind2)
	return(res)
}
