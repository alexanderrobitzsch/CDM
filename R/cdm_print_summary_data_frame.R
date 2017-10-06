## File Name: cdm_print_summary_data_frame.R
## File Version: 0.03
## File Last Change: 2017-10-06 10:16:59


cdm_print_summary_data_frame <- function(obji, from=NULL, to=NULL, digits=3)
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
	print(obji)
}
