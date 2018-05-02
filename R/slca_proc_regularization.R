## File Name: slca_proc_regularization.R
## File Version: 0.08

slca_proc_regularization <- function(regular_lam, regular_w, Nlam, Xlambda.fixed, regular_n, regular_type )
{
    regularization <- FALSE

    regular_avai <- c("lasso","mcp","scad")
    if ( ! ( regular_type %in% regular_avai ) ){
        stop("Only regularization methods 'lasso', 'scad' or 'mcp' can be chosen.\n")
    }
    if ( regular_lam > 0 ){
        regularization <- TRUE
    }
    if ( is.null(regular_w) ){
        regular_w <- rep(1,Nlam)
    }
    if ( ! is.null(Xlambda.fixed) ){
        regular_w[ Xlambda.fixed[,1] ] <- 0
    }
    # regular_lam_used <- regular_lam * regular_w * regular_n
    regular_lam_used <- regular_lam * regular_w
    regular_indicator_parameters <- regular_lam_used > 0
    #---- output
    res <- list( regular_lam=regular_lam, regular_w=regular_w, regular_lam_used=regular_lam_used,
                    regular_indicator_parameters=regular_indicator_parameters, regularization=regularization )
    return(res)
}
