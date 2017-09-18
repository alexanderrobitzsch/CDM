## File Name: univar_table_statistics.R
## File Version: 0.01
## File Last Change: 2017-06-20 15:22:26

univar_table_statistics <- function(freq, values=NULL)
{
	K <- length(freq)
	if (is.null(values)){
		values <- seq(0, K-1)	
	}
	N <- sum(freq)
	probs <- freq / N	
	M <- sum( probs * values)
	SD <- sqrt( sum( probs * values^2) - M^2 )
	#--- output
	res <- list(N=N, M=M, SD=SD)
	return(res)
}
