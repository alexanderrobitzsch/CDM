## File Name: reglca_progress_em_algorithm.R
## File Version: 0.15

reglca_progress_em_algorithm <- function( like.new, loglikeold,
        max.par.change, iter, progress, penalty, opt_fct,    opt_fct_change, n_reg,
        control_random_starts )
{
    digits_par_change <- 6
    digits_opt_fct <- 5
    digits_opt_fct_change <- 7

    #---- no random starts
    if (progress & ( ! control_random_starts$use_random_starts) ) {
        print_fct_change <- FALSE
        cat("---------------------------------------------------------------------------------\n")
        cat("Iteration", iter, "   ", paste( Sys.time() ), "\n" )
        cat( "Deviance=", round( -2*like.new, digits_opt_fct ) )
        devchange <- 2*(like.new-loglikeold)
        if ((iter >1) & print_fct_change ){
            cat(" | Deviance change=", round( 2*(like.new-loglikeold), digits_opt_fct_change) )
        }
        cat("\n" )
        cat( "Penalty=", round(  - penalty, digits_opt_fct ),
                " | number of regularized parameters=", n_reg, "\n")
        cat( "Optimization function=", round(  opt_fct, digits_opt_fct ) )
        if ((iter>1) & print_fct_change){
            cat(" | Function change=", round( opt_fct_change, digits_opt_fct_change) )
        }
        cat("\n")
        cat("Maximum parameter change:", round( max.par.change, digits_par_change), "\n")
    }

    #--- random starts
    if (progress & ( control_random_starts$use_random_starts) ){
        cat("Random start", control_random_starts$random_start_temp,
            " Iteration", iter,
            " | Deviance=", round( -2*like.new, digits_opt_fct ),
            " | Penalty=", round(  - penalty, digits_opt_fct ) )
        cat("\n")
    }


}
