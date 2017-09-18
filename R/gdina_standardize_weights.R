## File Name: gdina_standardize_weights.R
## File Version: 0.01
## File Last Change: 2017-06-05 13:13:00


gdina_standardize_weights <- function( weights )
{
	N <- length(weights)
	weights <- N*weights / sum(weights)
	return(weights)
}
