## File Name: slca_print_progress_end.R
## File Version: 0.01
## File Last Change: 2017-10-06 09:55:30

slca_print_progress_end <- function(s1, s2, progress)
{
	if (progress){
        cat("----------------------------------- \n")
        cat("Start:" , paste(s1) , "\n")
        cat("End:" , paste(s2) , "\n")
        cat("Difference:" , print(s2 -s1), "\n")
        cat("----------------------------------- \n")
    }
}
