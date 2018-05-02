## File Name: csink.R
## File Version: 1.02


csink <- function( file)
{
    if ( ! is.null( file ) ){
        sink()
    }
}
