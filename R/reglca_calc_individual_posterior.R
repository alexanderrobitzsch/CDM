## File Name: reglca_calc_individual_posterior.R
## File Version: 0.04

reglca_calc_individual_posterior <- function(class_probs, p.xi.aj, N, nclasses, weights, W)
{
	p.aj.xi <- cdm_matrix2( class_probs, nrow=N ) * p.xi.aj 
	p.aj.xi <- p.aj.xi / rowSums( p.aj.xi )
	class_probs <- colSums( p.aj.xi * weights / W )	
	#---- OUTPUT
	res <- list( p.aj.xi=p.aj.xi, class_probs=class_probs)
	return(res)
}
