## File Name: reglca_calc_probs.R
## File Version: 0.05

reglca_calc_probs <- function(parm)
{
	eps <- 1E-10
	probs <- cumsum(parm)	
	M1 <- max(probs)
	M0 <- min(probs)
	if ( M1 > 1 ){
		probs <- probs / ( M1 + eps )
	}
	if ( M0 < 0 ){
		probs <- ( probs - M0 ) / ( 1 - M0 + eps )
	}
	return(probs)
}
