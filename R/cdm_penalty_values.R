## File Name: cdm_penalty_values.R
## File Version: 0.01

cdm_penalty_values <- function(x, regular_type, regular_lam, regular_tau=NULL)
{
	penalty <- 0
	if (regular_type=="scad"){
		penalty <- cdm_penalty_scad( x=x , lambda=regular_lam )
	}
	if (regular_type=="lasso"){
		penalty <- cdm_penalty_lasso( x=x , lambda=regular_lam )
	}
	if (regular_type=="ridge"){
		penalty <- cdm_penalty_ridge( x=x , lambda=regular_lam )
	}
	return(penalty)
}
