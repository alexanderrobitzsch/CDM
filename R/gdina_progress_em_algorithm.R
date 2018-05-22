## File Name: gdina_progress_em_algorithm.R
## File Version: 0.31

gdina_progress_em_algorithm <- function( delta, data, like.new, loglikeold,
        max.par.change, iter, progress, progress.item, regularization, penalty, opt_fct,
        opt_fct_change, ll_value, regular_type, logprior_value, use_prior)
{
    digits_par_change <- 6
    digits_opt_fct <- 5
    digits_opt_fct_change <- 7

    if (progress){
        if (progress.item){
            g1 <- unlist( lapply( delta, FUN=function(ll){ paste( round(ll,4), collapse=" " ) } ))
            g1 <- matrix( paste( colnames(data), g1 ), ncol=1)
            print(g1)
        }
        cat( "Deviance=", round( -2*like.new, digits_opt_fct ) )
        devchange <- 2*(like.new-loglikeold)
        if (iter >1){
            cat(" | Deviance change=", round( 2*(like.new-loglikeold), digits_opt_fct_change) )
        }
        cat("\n" )
        if (regularization | use_prior){
            if ( regularization){
                cat( "Penalty=", round(  penalty, digits_opt_fct ), "\n")
            }
            if ( use_prior){
                cat( "Log prior=", round(  logprior_value, digits_opt_fct ), "\n")
            }
            cat( "Optimization function=", round(  opt_fct, digits_opt_fct ) )
            if (iter>1){
                cat(" | Function change=", round( opt_fct_change, digits_opt_fct_change) )
            }
            cat("\n")
        }
        if ( ( ! regularization ) & ( ! use_prior ) ){
            if ( devchange < 0 & iter>1){
                cat( "**** Deviances decreases! Check for nonconvergence.   ****\n")
            }
        }
        cat("Maximum parameter change:", round( max.par.change, digits_par_change), "\n")
    }
}
