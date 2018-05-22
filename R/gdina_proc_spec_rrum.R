## File Name: gdina_proc_spec_rrum.R
## File Version: 0.09

gdina_proc_spec_rrum <- function(rule, method, linkfct)
{
    # estimation of a reduced RUM model
    rrum.params <- FALSE
    rrum.model <- FALSE

    if ( any( rule=="RRUM" ) ){
        rule <- "ACDM"
        linkfct <- "log"
        if ( is.null(method) ){
            method <- "ML"
        }
        rrum.model <- TRUE
    } else {
        if ( is.null(method) ){
            method <- "WLS"
        }
    }
    #---- OUTPUT
    res <- list( rrum.params=rrum.params, rrum.model=rrum.model, method=method,
                    linkfct=linkfct, rule=rule)
    return(res)
}
