## File Name: cdm_print_summary_call.R
## File Version: 0.01
## File Last Change: 2017-10-04 17:04:55


cdm_print_summary_call <- function(object)
{
	cat("Call:\n")
	print(object$call)
	cat("\n")
}
