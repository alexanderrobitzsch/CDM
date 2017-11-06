## File Name: cdm_print_summary_data_frame.R
## File Version: 0.04


cdm_print_summary_data_frame <- function(obji, from=NULL, to=NULL, digits=3, rownames_null=FALSE)
{
	if (is.vector(obji)){
		obji <- round(obji, digits)
	} else {
		if (is.null(from)){
			from <- 1
		}
		if (is.null(to)){
			to <- ncol(obji)
		}		
		ind <- seq( from, to )
		for (vv in ind){
			obji_vv <- obji[,vv] 
			if ( is.numeric(obji_vv) ){
				obji[ , vv ] <- round( obji_vv, digits )
			}
		}
	}
	if (rownames_null){
		rownames(obji) <- NULL
	}
	print(obji)
}
