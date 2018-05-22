## File Name: osink.R
## File Version: 1.09


osink <- function( file, suffix, append=FALSE)
{
    if ( ! is.null( file ) ){
        sink( paste0( file, suffix), split=TRUE, append=append )
    }
}


