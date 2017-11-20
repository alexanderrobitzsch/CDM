## File Name: dataframe_summary.R
## File Version: 0.05

dataframe_summary <- function( dfr , exclude_index , labels , na.rm=TRUE )
{
	if ( ! is.null(exclude_index) ){
		dfr1 <- dfr[ , - exclude_index, drop=FALSE]
	} else {
		dfr1 <- dfr
	}
	dfr_summary <- data.frame( "Parm" = labels ,
						"M"= apply( dfr1 , 2, mean , na.rm=na.rm) , 
						"SD"= apply( dfr1 , 2 , stats::sd , na.rm=na.rm) ,
						"Min"= apply( dfr1 , 2 , min , na.rm=na.rm),
						"Max"= apply( dfr1 , 2 , max , na.rm=na.rm )
					)
	rownames(dfr_summary) <- NULL
	return(dfr_summary)
}
