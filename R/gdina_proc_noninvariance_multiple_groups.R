## File Name: gdina_proc_noninvariance_multiple_groups.R
## File Version: 0.06
## File Last Change: 2017-09-18 15:51:18
#####################################################################
# handle non-invariance of multiple group parameters
gdina_proc_noninvariance_multiple_groups <- function( data , q.matrix , invariance ,
	group )
{	
	create_pseudo_items <- TRUE
	invariant <- invariance
	invariance_TRUE <- mean( invariance == TRUE ) == 1
	invariance_FALSE <- mean( invariance == FALSE ) == 1
	if (invariance_TRUE ){
		create_pseudo_items <- FALSE
		invariant <- NULL
	}
	if (invariance_FALSE){
		invariant <- NULL
	}	
	rownames(q.matrix) <- colnames(data)
	if ( ( ! is.null(group) ) & create_pseudo_items ){
		I <- ncol(data)
		data <- item_by_group(dat = data, group=group, invariant=invariant)
		G <- length( unique(group) )
		ind <- c( attr(data, "invariant_index") , attr(data, "noninvariant_index_extended") )
		q.matrix <- q.matrix[ ind , ]
		rownames(q.matrix) <- colnames(data)
	}
	res <- list( data = data, q.matrix = q.matrix )
	return(res)
}
#####################################################################
