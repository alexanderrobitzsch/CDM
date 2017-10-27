## File Name: bivariate_table_statistics.R
## File Version: 0.05

bivariate_table_statistics <- function( freqtable, values=NULL)
{
	freqtable <- expand_matrix(x=freqtable)
	K <- nrow(freqtable)
	if (is.null(values)){
		values <- seq(0,K-1)
	}	
	#--- first item
	freq1 <- rowSums(freqtable)
	res <- univar_table_statistics(freq=freq1)
	M1 <- res$M
	SD1 <- res$SD
	K1 <- sum( freq1 > 0 )
	#--- second item
	freq2 <- colSums(freqtable)
	res <- univar_table_statistics(freq=freq2)
	M2 <- res$M
	SD2 <- res$SD
	K2 <- sum( freq2 > 0 )
	#--- bivariate statistics
	valuesM <- outer( values, values )
	probs <- freqtable / sum( freqtable )
	cov1 <- sum( probs * valuesM ) - M1 * M2
	cor1 <- cov1 / SD1 / SD2
	#--- output
	res <- list( cov=cov1, cor=cor1, M1=M1, SD1=SD1, K1=K1, M2=M2, SD2=SD2, K2=K2)
	return(res)
}
