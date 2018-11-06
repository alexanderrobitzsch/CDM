## File Name: cat_paste.R
## File Version: 0.03


cat_paste <- function(...)
{
    args <- list(...)
    N1 <- length(args)
    res <- args[[1]]
    if (N1 > 1){
        for (nn in 2:N1){
            res <- paste0( res, args[[nn]] )
        }
    }
    cat(res)
}

