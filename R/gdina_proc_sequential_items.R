## File Name: gdina_proc_sequential_items.R
## File Version: 0.04

gdina_proc_sequential_items <- function( data , q.matrix )
{
    maxK <- max( data , na.rm=TRUE )
    sequential <- FALSE
    if ( maxK > 1){
        res0 <- sequential.items( data = data )
        data <- res0$dat.expand
        sequential <- TRUE
        q.matrix <- q.matrix[,-c(1:2)]
    }
    res <- list( data = data , sequential = sequential ,
                q.matrix = q.matrix )
    return(res)
}
