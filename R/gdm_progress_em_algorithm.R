## File Name: gdm_progress_em_algorithm.R
## File Version: 0.08

gdm_progress_em_algorithm <- function( progress, disp, iter, dev, dev0, b_change,
        a_change, deltadiff, dev_digits=4, parm_digits=6 )
{
    if (progress){
        cat(disp)
        cat("Iteration", iter, "   ", paste( Sys.time() ), "\n" )
        cat( paste( "   Deviance=", round( dev, dev_digits ),
                if (iter > 1 ){ " | Deviance change=" } else {""},
                        if( iter>1){round( - dev + dev0, parm_digits )} else { ""}    ,sep="") )
        if ( dev > dev0 & (iter>1 ) ){
            cat( "  Deviance increases!")
        }
        cat("\n")
        cat( paste( "    Maximum item intercept parameter change=",
                                round( b_change, parm_digits ),  " \n"   )  )
        cat( paste( "    Maximum item slope parameter change=",
                                round( a_change, parm_digits ),  " \n"   )  )
        cat( paste( "    Maximum distribution parameter change=",
                                round( deltadiff, parm_digits ),  " \n"   )  )
        utils::flush.console()
    }
}
