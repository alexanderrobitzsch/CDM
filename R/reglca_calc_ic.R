## File Name: reglca_calc_ic.R
## File Version: 0.03

reglca_calc_ic <- function( loglike, nclasses, I, N, n_reg, G)
{
	Nskillpar <- G*(nclasses - 1)
	Nipar <- I*nclasses - n_reg
	Npars <- Nipar + Nskillpar
	aic <- -2*loglike + 2 * Npars  
	bic <- -2*loglike + Npars*log(N)
	caic <- -2*loglike + ( log(N) + 1 ) * Npars
	deviance <- -2*loglike
	#---- OUTPUT
	res <- list(Npars=Npars, AIC=aic, BIC=bic, CAIC=caic, Nskillpar=Nskillpar, Nipar=Nipar,
					deviance=deviance, n_reg=n_reg )
	return(res)
}
