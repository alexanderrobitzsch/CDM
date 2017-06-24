#####################################################################
# handle non-invariance of multiple group parameters
gdina_proc_noninvariance_multiple_groups <- function( data , q.matrix , invariance ,
	group )
{	
	if ( ( ! is.null(group) ) & ( ! invariance ) ){
		I <- ncol(data)
		data <- item_by_group(dat = data, group=group)
		G <- ncol(data) / I
		q.matrix <- q.matrix[ rep(1:I , each = G ) , ]
	}
	res <- list( data = data, q.matrix = q.matrix )
	return(res)
}
#####################################################################
