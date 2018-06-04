## File Name: cdm_print_summary_call.R
## File Version: 0.02


cdm_print_summary_call <- function(object, call_name="call")
{
    cat("Call:\n")
    print(object[[ call_name ]] )
    cat("\n")
}
