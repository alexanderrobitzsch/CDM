## File Name: gdina_proc_prior_distribution.R
## File Version: 0.09


gdina_proc_prior_distribution <- function( prior_intercepts, prior_slopes, method, linkfct, PEM )
{
    prior_intercepts_null <- is.null(prior_intercepts)
    prior_slopes_null <- is.null(prior_slopes)
    use_prior <- FALSE
    if ( ( ! prior_intercepts_null ) | ( ! prior_slopes_null ) ){
        method <- "ML"
        linkfct <- "logit"
        use_prior <- TRUE
        PEM <- FALSE
        if (prior_intercepts_null){
            prior_intercepts <- c(0,1,Inf)
        }
        if (prior_slopes_null){
            prior_slopes <- c(0,1,Inf)
        }
        prior_intercepts <- gdina_proc_prior_distribution_extend_normal(prior=prior_intercepts)
        prior_slopes <- gdina_proc_prior_distribution_extend_normal(prior=prior_slopes)
    }
    #---- output
    res <- list(prior_intercepts=prior_intercepts, prior_slopes=prior_slopes, linkfct=linkfct,
                    method=method, use_prior=use_prior, PEM=PEM)
    return(res)
}
