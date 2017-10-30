## File Name: gdina_mstep_item_ml_update_parameter_regularization.R
## File Version: 0.04


gdina_mstep_item_ml_update_parameter_regularization <- function(x, regular_type, regular_lam )
{
	y <- cdm_parameter_regularization(x=x, regular_type=regular_type, regular_lam=regular_lam )	
	return(y)
}
