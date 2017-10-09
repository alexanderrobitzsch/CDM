## File Name: slca_est_Xlambda_calc_increment.R
## File Version: 0.01
## File Last Change: 2017-10-07 19:53:32

slca_est_Xlambda_calc_increment <- function(d1, d2, x0, regularization, regular_lam_used, max.increment )
{	
	if (regularization){
		res <- cdm_calc_increment_regularization( d1=d1, d2=d2, x0=x0, regular_lam_used=regular_lam_used, max.increment=max.increment ) 
	}
	if (!regularization){	
		res <- cdm_calc_increment( d1=d1, d2=d2, max.increment=max.increment ) 
	}
	return(res)
}
