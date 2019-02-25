## File Name: slca_print_progress_em_algorithm.R
## File Version: 0.182

slca_print_progress_em_algorithm <- function(progress, disp, iter, dev, dev0, deltadiff, Xlambda_change,
        regularization, regular_penalty, numb_regularized, digits_dev=4, digits_parm=6 )
{
    if (progress){
        cat(disp)
        cat("Iteration", iter, "   ", paste( Sys.time() ), "\n" )
        cat( paste( "   Deviance ", "=", " ", round( dev, digits_dev ),
                    if (iter > 1 ){ paste(" | Deviance change", "=", "") } else {""},
                            if( iter>1){round( - dev + dev0, digits_parm )} else { ""}    ,sep=""))
        if ( (dev > dev0) & (iter>1 ) & ( ! regularization) ){
            cat( "  Deviance increases!")
        }
        cat("\n")
        if (regularization){
            cat( paste( "   Penalty", "=", round( regular_penalty, digits_dev ), " | " ) )
            cat( paste( "Number of regularized parameters",    "=", numb_regularized, "\n" ) )
        }
        cat( paste( "    Maximum Xlambda parameter change", "=",
                                round( max( Xlambda_change ), digits_parm ),  " \n"   )  )
        cat( paste( "    Maximum distribution parameter change", "=",
                                round( max( deltadiff ), digits_parm ),  " \n"   )  )
        utils::flush.console()
    }
}
