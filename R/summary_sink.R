## File Name: summary_sink.R
## File Version: 0.08

summary_sink <- function( object, file, append=FALSE, ...)
{
    osink( file=file, suffix=".Rout", append=append )
    summary(object=object, ... )
    csink(file=file)
}
