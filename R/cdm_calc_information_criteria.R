## File Name: cdm_calc_information_criteria.R
## File Version: 0.01
## File Last Change: 2017-10-04 16:59:16

cdm_calc_information_criteria <- function(ic)
{
	dev <- ic$deviance
	# AIC
    ic$AIC <- dev + 2*ic$np
    # BIC
    ic$BIC <- dev + ( log(ic$n) )*ic$np
    # CAIC (conistent AIC)
    ic$CAIC <- dev + ( log(ic$n) + 1 )*ic$np
	# corrected AIC
    ic$AICc <- ic$AIC + 2*ic$np * ( ic$np + 1 ) / ( ic$n - ic$np - 1 )		
	#--- output
	return(ic)
}
