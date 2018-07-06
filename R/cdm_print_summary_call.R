## File Name: cdm_print_summary_call.R
## File Version: 0.05


cdm_print_summary_call <- function(object, call_name="call")
{
    CALL <- object[[ call_name ]]
    s3 <- paste0(CALL, collapse=" ")
    if ( nchar(s3) < 3000 ){
        cat("Call:\n")
        print( CALL )
        cat("\n")
    }
}
