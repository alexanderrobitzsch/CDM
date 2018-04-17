## File Name: summary.discrim.index.R
## File Version: 0.01


summary.discrim.index <- function( object , file = NULL , digits = 3 , ... )
{
	osink( file = file , suffix = paste0( "__SUMMARY.Rout") )

	cat("-----------------------------------------------------------------------------\n")
	d1 <- utils::packageDescription("CDM")
	cat( paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="") , "\n\n" )

	cat("-----------------------------------------------------------------------------\n")
	cat("Test-level discrimination index \n\n")
	
	obji <- object$discrim_test
	cdm_print_summary_data_frame(obji, from=1, digits=digits)

	cat("-----------------------------------------------------------------------------\n")
	cat("Item-level discrimination index \n\n")
	
	obji <- object$discrim_item
	cdm_print_summary_data_frame(obji, from=2, digits=digits)
	
	csink( file = file )
}
#*******************************************************
