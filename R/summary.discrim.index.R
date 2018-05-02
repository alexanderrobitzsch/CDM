## File Name: summary.discrim.index.R
## File Version: 0.04


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
    cat("Item discrimination index (IDI) \n\n")
    obji <- object$idi
    cdm_print_summary_data_frame(obji, digits=digits)
    
    
    cat("-----------------------------------------------------------------------------\n")
    cat("Item-attribute discrimination index \n\n")
    obji <- object$discrim_item_attribute
    cdm_print_summary_data_frame(obji, from=2, digits=digits)

    csink( file = file )
}
#*******************************************************
