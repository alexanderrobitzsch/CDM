## File Name: slca_proc_data.R
## File Version: 0.02

slca_proc_data <- function(data)
{
	data0 <- data <- as.matrix(data)
	dat.resp0 <- dat.resp <- 1 - is.na(data)
	dat <- data
	dat[ is.na(data) ] <- 0
	# maximal categories
	K <- max(dat)
	maxK <- K+1
	# list of indicator data frames
	dat.ind <- as.list( 1:(K+1) )
	for (ii in 0:K){
		dat.ind[[ii+1]] <- 1 * ( dat==ii )*dat.resp
	}
	I <- ncol(dat)	# number of items
	n <- nrow(dat)
	#--- response indicators	
	resp.ind.list <- gdm_proc_response_indicators(dat.resp=dat.resp)		
	#----- output
	res <- list(dat=dat, dat.ind=dat.ind, I=I, n=n, dat.resp=dat.resp, K=K, maxK=maxK,
					data=data, data0=data0, resp.ind.list=resp.ind.list)
	return(res)
}
