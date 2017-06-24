

gdina_standardize_weights <- function( weights )
{
	N <- length(weights)
	weights <- N*weights / sum(weights)
	return(weights)
}