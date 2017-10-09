## File Name: cdm_soft_threshold.R
## File Version: 0.01
## File Last Change: 2017-10-07 19:26:15

cdm_soft_threshold <- function( val, eta )
{
	res <- val
	res <- ifelse( abs(val) < eta , 0 , res )
	res <- ifelse( val > eta , val - eta , res )
	res <- ifelse( val < - eta , val + eta , res )
	return(res)
}
