## File Name: summary_sink.R
## File Version: 0.06

summary_sink <- function( object, file, append=FALSE, ...)
{
    osink( file=file, suffix="__SUMMARY.Rout", append=append )
    print( summary(object=object, ... ) )
    csink(file=file)
}
