## File Name: summary.reglca.R
## File Version: 0.22


summary.reglca <- function( object, digits=4, file=NULL, ... )
{
    rdigits <- digits

    osink( file=file, suffix=paste0( "__SUMMARY.Rout") )

    # Parameter summary
    cat("---------------------------------------------------------------------------------------------------------- \n")

    #-- print package
    cdm_print_summary_package(pack="CDM")
    cat("\n")

    #-- summary call
    cdm_print_summary_call(object=object)

    #-- print computation time
    cdm_print_summary_computation_time(object=object)

    cat("Regularized Latent Class Model \n")

    cat( "\nNumber of iterations=", object$iter, "\n")
    if ( ! object$converged ){
        cat("\nMaximum number of iterations was reached.\n")
    }

    regtype <- object$regular_type
    if (object$regular_lam==0 ){
        regtype <- "none"
    }
    cat( paste0("Regularization type: ", regtype, "\n" ) )
    cat( paste0("Regularization parameter lambda: ",  object$regular_lam, "\n" ) )
    cat( paste0("Number of regularized item parameters: ",  object$n_reg, "\n" ) )
    cat("\n")

    cat( "Deviance=", round( object$deviance, 2 ) )
    cat( "  | Log likelihood=", round( - object$deviance / 2, 2 ),    "\n" )
    cat( "Penalty value=", round( object$penalty, 2 ) )
    cat( " | Optimization function=", round( object$opt_fct, 2 ), "\n" )
    cat("\n")

    cat( "Number of persons=", object$N, "\n" )
    cat( "Number of groups=", object$G, "\n" )
    cat( "Number of items=", object$I, "\n" )
    cat( "Number of estimated parameters=", object$Npars, "\n" )
    cat( "Number of estimated item parameters=", object$Nipar,
            "(out of", object$I * object$nclasses, "estimable parameters)",    "\n" )
    cat( "Number of estimated class parameters=", object$Nskillpar )
    grlab <- if (object$G==1){ "group" } else { "groups" }
    cat( " (", object$nclasses, "latent classes,", object$G, grlab, ")\n")
    cat( "\n")

    cat( "AIC=", round( object$AIC, 2 ), " | penalty=", round( object$AIC - object$deviance,2 ), "\n" )
    cat( "BIC=", round( object$BIC, 2 ), " | penalty=", round( object$BIC - object$deviance,2 ), "\n" )
    cat( "CAIC=", round( object$CAIC, 2 )," | penalty=", round( object$CAIC - object$deviance,2 ), "\n\n" )

    cat("----------------------------------------------------------------------------\n")
    cat("Model Implied Conditional Item Probabilities \n\n")
    res <- cdm_print_summary_data_frame(obji=object$item, from=2, digits=rdigits, rownames_null=TRUE)

    cat("----------------------------------------------------------------------------\n")
    cat("Latent Class Probabilities \n\n")
    print(round(object$class_probs,rdigits) )

    csink( file=file )
}
##########################################################################
