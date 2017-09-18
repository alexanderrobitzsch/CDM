## File Name: IRT.parameterTable.R
## File Version: 0.03
## File Last Change: 2017-01-31 14:07:28


###########################################################
# extracts used dataset
IRT.parameterTable <- function(object, ...) {
    UseMethod("IRT.parameterTable")
}
###########################################################
# IRT.data.din <- function( object , ... ){
#	dat <- object$dat
#	attr(dat,"weights") <- object$control$weights
#	attr(dat,"group") <- object$control$group
#   return(dat)
#			}
############################################################			
# IRT.data.gdina <- IRT.data.din
# IRT.data.gdm <- IRT.data.din
# IRT.data.mcdina <- IRT.data.din
# IRT.data.slca <- IRT.data.din
#############################################################
