## File Name: discrim.index.R
## File Version: 0.05


discrim.index <- function(object, ...)
{
	UseMethod("discrim.index")
}


discrim_index_cdm <- function( object, ...)
{
	attr_patt <- object$attribute.patt.splitted
	probs <- IRT.irfprob(object)
	skill_names <- colnames(object$q.matrix)
	res <- discrim_index_computation( attr_patt=attr_patt, 
				probs=probs, dicho=TRUE, skill_names=skill_names)	
	return(res)
}


discrim.index.gdina <- discrim_index_cdm
discrim.index.din <- discrim_index_cdm
