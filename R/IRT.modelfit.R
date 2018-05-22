## File Name: IRT.modelfit.R
## File Version: 0.21

###########################################################
IRT.modelfit <- function (object, ...)
{
    UseMethod("IRT.modelfit")
}
###########################################################
# general model fit function for CDM objects
IRT.modelfit.CDM <- function( object, mod )
{
    res <- modelfit.cor.din( dinobj=object)
    res$IRT.IC <- IRT.IC(object)
    res$objname <- mod
    class(res) <- paste0("IRT.modelfit.", class(object) )
    return(res)
}
###########################################################


###########################################################
# IRT.modelfit for objects of class din, gdina
IRT.modelfit.din <- function( object, ... )
{
    cl <- paste(match.call())[2]
    res <- IRT.modelfit.CDM( object, mod=cl )
    return(res)
}
#####################################################
# IRT.modelfit for gdina objects
IRT.modelfit.gdina <- function( object, ... )
{
    cl <- paste(match.call())[2]
    res <- IRT.modelfit.CDM( object, mod=cl )
    return(res)
}
#############################################################

#####################################################
# IRT.modelfit for gdm objects
IRT.modelfit.gdm <- function( object, ... )
{
    cl <- paste(match.call())[2]
    res <- IRT.modelfit.CDM( object, mod=cl )
    return(res)
}
#############################################################

############################################################
# summary
summary.IRT.modelfit.helper <- function( object, ... )
{
    cat("Test of Global Model Fit\n")
    obji <- object$modelfit.test
    cdm_print_summary_data_frame(obji=obji, from=2, digits=3, rownames_null=FALSE)

    #------
    cat("\nFit Statistics\n")
    obji <- object$modelfit.stat
    cdm_print_summary_data_frame(obji=obji, from=1, digits=3, rownames_null=FALSE)
}
#################################################################


# summary.modelfit.cor.din
summary.IRT.modelfit.din <- summary.IRT.modelfit.helper
summary.IRT.modelfit.gdina <- summary.IRT.modelfit.helper
summary.IRT.modelfit.gdm <- summary.IRT.modelfit.helper
# summary.IRT.modelfit.gdm <- summary.modelfit.cor.gdm
