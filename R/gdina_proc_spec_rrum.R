## File Name: gdina_proc_spec_rrum.R
## File Version: 0.12

gdina_proc_spec_rrum <- function(rule, method, linkfct, optimizer="CDM")
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
        optimizer <- "optim"
    } else {
        if ( is.null(method) ){
            method <- "WLS"
        }
    }
    #---- OUTPUT
    res <- list( rrum.params=rrum.params, rrum.model=rrum.model, method=method,
                    linkfct=linkfct, rule=rule, optimizer=optimizer)
    return(res)
}
