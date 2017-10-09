## File Name: cdm_ll_numerical_differentiation.R
## File Version: 0.01
## File Last Change: 2017-10-08 15:05:26

cdm_ll_numerical_differentiation <- function(ll0, ll1, ll2, h)
{
    d1 <- ( ll1 - ll2  ) / ( 2 * h )    # negative sign?
    # second order derivative
    # f(x+h)+f(x-h) = 2*f(x) + f''(x)*h^2
    d2 <- ( ll1 + ll2 - 2*ll0 ) / h^2
	#--- output
	res <- list( d1=d1, d2=d2)
	return(res)

}
