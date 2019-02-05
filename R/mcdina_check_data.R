## File Name: mcdina_check_data.R
## File Version: 0.05

mcdina_check_data <- function(dat, q.matrix)
{
    I <- ncol(dat)
    print_warning <- FALSE
    for (ii in 1L:I){
        values_ii <- sort(unique( stats::na.omit(dat[,ii]) ))
        q_ii <- q.matrix[ q.matrix$item == ii, "categ"]
        non_def <- setdiff(values_ii, q_ii)
        if (length(non_def)>0){
            v1 <- paste0("Non-defined category for item ", colnames(dat)[ii], ": ")
            v2 <- paste0(non_def, collapse=" ")
            cat(paste0(v1,v2,"\n"))
            print_warning <- TRUE
        }
    }
    if (print_warning){
        stop("Modify data or Q-matrix input!\n")
    }
}
