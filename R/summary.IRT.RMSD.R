## File Name: summary.IRT.RMSD.R
## File Version: 0.09


#*******************************************************
# Summary
summary.IRT.RMSD <- function( object , file = NULL , digits = 3 , ... ){
    	
 	osink( file = file , suffix = paste0( "__SUMMARY.Rout") )

    cat("-----------------------------------------------------------------------------\n")
    d1 <- utils::packageDescription("CDM")
	cat( paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="") , "\n\n" )
    
	G <- object$G
	
	cat("Call:\n")
	print(object$CALL)
	cat("\n")
	

	cat("-----------------------------------------------------------------------------\n")
	cat("Root Mean Square Deviation (RMSD) \n\n")
	
	res0 <- summary.IRT.RMSD_print_statistics( stat_summary = object$RMSD_summary , 
				stat = object$RMSD , digits=digits)	

	cat("-----------------------------------------------------------------------------\n")
	cat("Bias Corrected Root Mean Square Deviation (RMSD) \n\n")
	
	res0 <- summary.IRT.RMSD_print_statistics( stat_summary = object$RMSD_bc_summary , 
				stat = object$RMSD_bc , digits=digits)					
				
	cat("-----------------------------------------------------------------------------\n")
	cat("Mean Absolute Deviation (MAD) \n\n")
	
	res0 <- summary.IRT.RMSD_print_statistics( stat_summary = object$MAD_summary , 
				stat = object$MAD , digits=digits)	
	
	cat("-----------------------------------------------------------------------------\n")
	cat("Mean Deviation (MD) \n\n")
	
	res0 <- summary.IRT.RMSD_print_statistics( stat_summary = object$MD_summary , 
				stat = object$MD , digits=digits)		
	
    csink( file = file )

}
#*******************************************************
