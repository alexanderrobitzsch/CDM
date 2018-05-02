## File Name: discrim.index.R
## File Version: 0.11


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
                probs=probs, skill_names=skill_names, item_names=dimnames(probs)[[1]] )
    return(res)
}



discrim.index.din <- discrim_index_cdm
discrim.index.gdina <- discrim_index_cdm
discrim.index.mcdina <- discrim_index_cdm
