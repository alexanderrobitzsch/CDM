## File Name: cdm_print_summary_computation_time.R
## File Version: 0.02


cdm_print_summary_computation_time <- function(object)
{
    cat( "Date of Analysis:", paste( object$time$s2 ), "\n" )
    cat("Computation Time:", print(object$time$s2 - object$time$s1), "\n\n")
}
