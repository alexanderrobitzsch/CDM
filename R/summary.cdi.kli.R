## File Name: summary.cdi.kli.R
## File Version: 0.05

#################################################################################
# summary S3 method
summary.cdi.kli <- function( object, digits=2, ...)
{
    obji <- object$summary
    V <- ncol(obji)
    for (vv in 2:V){
        obji[,vv] <- round( obji[,vv], digits)
    }
    rownames(obji) <- NULL
    print(obji)
}
#####################################################################################
