## File Name: slca_calc_class_probabilities.R
## File Version: 0.04

slca_calc_class_probabilities <- function( delta, delta.designmatrix )
{
	G <- ncol(delta)
	pi.k <- matrix(0, nrow=nrow(delta.designmatrix), ncol=G)
	for (gg in 1:G){
		pi.k[,gg] <- cdm_sumnorm( exp( delta.designmatrix %*% delta[,gg] ) )
	}
	return(pi.k)
}
