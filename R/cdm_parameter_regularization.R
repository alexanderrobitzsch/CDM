## File Name: cdm_parameter_regularization.R
## File Version: 0.05


cdm_parameter_regularization <- function(x, regular_type, regular_lam )
{
	y <- x
	if ( regular_type == "scad"){
		y <- cdm_scad_threshold( beta=x, lambda=regular_lam)
	}
	if ( regular_type == "lasso"){
		y <- cdm_soft_threshold( val=x, eta=regular_lam )
	}
	return(y)
}
