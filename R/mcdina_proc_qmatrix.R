## File Name: mcdina_proc_qmatrix.R
## File Version: 0.01



mcdina_proc_qmatrix <- function( dat, q.matrix )
{
    if ( min( dat, na.rm=TRUE )==0 ){
        dat <- dat + 1
        I <- ncol(dat)
        if ( nrow(q.matrix)==I ){
            q1 <- data.frame( "item"=1:I, "categ"=2, q.matrix )
            q0 <- data.frame( "item"=1:I, "categ"=1, 0+0*q.matrix )
            q.matrix <- rbind( q0, q1 )
            q.matrix <- q.matrix[ order( 100 * q.matrix$item + q.matrix$categ ), ]
        }
    }
    res <- list("dat"=dat, "q.matrix"=q.matrix )
    return(res)
}
