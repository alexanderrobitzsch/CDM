## File Name: IRT.data.R
## File Version: 0.09


###########################################################
# extracts used dataset
IRT.data <- function(object, ...)
{
	UseMethod("IRT.data")
}
###########################################################
IRT.data.din <- function( object , ... ){
	dat <- object$dat
	attr(dat,"weights") <- object$control$weights
	attr(dat,"group") <- object$control$group
	return(dat)
}
############################################################			
IRT.data.gdina <- IRT.data.din
IRT.data.gdm <- IRT.data.din
IRT.data.mcdina <- IRT.data.din
IRT.data.slca <- IRT.data.din
#############################################################

IRT.data.reglca <- function( object , ... )
{
	dat <- object$dat0
	attr(dat,"weights") <- object$weights
	attr(dat,"group") <- NULL
	return(dat)
}
