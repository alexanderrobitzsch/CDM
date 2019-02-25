## File Name: summary.gdina.R
## File Version: 1.792


# Summary of the GDINA model
summary.gdina <- function( object, digits=4, file=NULL, ... )
{
    osink( file=file, suffix=paste0( "__SUMMARY.Rout") )

    rdigits <- digits

    # Parameter summary
    display <- cdm_summary_display()
    cat(display)

    #-- print package
    cdm_print_summary_package(pack="CDM")
    cat("\n")

    #-- summary call
    cdm_print_summary_call(object=object)

    #-- print computation time
    cdm_print_summary_computation_time(object=object)

    if (object$HOGDINA==-1){
        cat("Generalized DINA Model \n") } else {
        cat("Higher Order Generalized DINA Model \n") }
    if ( object$G > 1 ){
        cat("  Multiple Group Estmation with",object$G, "Groups \n")
        # group statistics
        cat("\nGroup statistics\n")
        print( object$group.stat )
        cat("\n")
    }

    cat( "\nNumber of iterations","=", object$iter  )
    if ( ! object$converged ){ cat("\nMaximum number of iterations was reached.") }
    cat( "\nIteration with minimal deviance","=", object$iter.min, "\n\n" )

    #-- information about algorithm
    cat( paste0("Estimation method: ",  object$method, "\n") )
    cat( paste0("Optimizer: ",  object$optimizer, "\n") )
    cat( paste0("Monotonicity constraints: ",  object$mono.constr, "\n") )
    cat( paste0("Number of items at boundary monotonicity constraint: ",  object$numb_bound_mono, "\n") )
    if ( ! is.na(object$numb_bound_mono) > 0 ){
        v1 <- paste0( paste0("Items at boundary constraint: "), paste0( object$item_bound_mono, collapse=" " ) )
        cat(v1,"\n")
    }
    cat("\n")

    cat( paste0("Parameter regularization: ",  object$regularization, "\n") )
    if (object$regularization){
        cat( paste0("Regularization type: ",  object$regular_type, "\n" ) )
        cat( paste0("Regularization parameter lambda: ",  object$regular_lam, "\n" ) )
        cat( paste0("Regularization parameter alpha: ",  object$regular_alpha, " (SCAD-L2, elastic net)\n" ) )
        cat( paste0("Regularization parameter tau: ",  object$regular_tau, " (truncated L1 penalty)\n" ) )
        cat( paste0("Number of regularized item parameters: ",  object$numb_regular_pars, "\n" ) )
    }
    cat("\n")

    cat( "Deviance","=", round( object$deviance, 2 ) )
    cat( "  | Log likelihood","=", round( - object$deviance / 2, 2 ),    "\n" )
    if ( object$regularization | object$use_prior ){
        if ( object$regularization ){
            cat( "Penalty value","=", round( object$penalty, 2 ) )
        }
        if ( object$use_prior ){
            cat( "Log prior value","=", round( object$logprior_value, 2 ) )
        }
        cat( " | Optimization function","=", round( object$opt_fct, 2 ), "\n" )
    }
    cat("\n")

    cat( "Number of persons","=", object$N, "\n" )
    cat( "Number of groups","=", object$G, "\n" )
    cat( "Number of items","=", ncol(object$data), "\n" )
    cat( "Number of estimated parameters","=", object$Npars, "\n" )
    cat( "Number of estimated item parameters","=", object$Nipar, "\n" )
    cat( "Number of estimated skill class parameters","=", object$Nskillpar )
    cat( " (", object$Nskillclasses, "latent skill classes)\n\n")

    #*** information criteria
    cdm_print_summary_information_criteria(object=object)

    cat(display)
    cat("Used Q-matrix \n\n")
    print(object$q.matrix)
    cat("\n")

    cat(display)
    cat("\nItem Parameter Estimates \n\n")

    ds <- object$coef
    selvars <- intersect( c("est", "se" ), colnames(ds) )
    ind <- which( colnames(ds) %in% selvars )
    for (ii in ind){
        ds[,ii] <- round( ds[,ii], rdigits )
    }

    r1 <- options()
    options(scipen=999)
    print(ds)
    options(scipen=r1$scipen)
        if ( ! is.null( object$delta.designmatrix ) ){
            cat("\nNote: Standard errors are not (yet) correctly implemented!\n")
    }

    cat("\nRMSD (RMSEA) Item Fit Statistics\n")
    print( round( object$itemfit.rmsea,3) )

    cat("\nMean of RMSEA item fit:",
    round( object$mean.rmsea,3 ), "\n")

    # RRUM model
    if (object$rrum.model){
        cat("\n****\nRRUM Parametrization\n")
        print( round( object$rrum.params,3), na="")
        cat("\n")
    }

    cat(display)
    cat("Model Implied Conditional Item Probabilities \n\n")
    obji <- object$probitem
    obji[,"prob"] <- round( obji$prob, rdigits )
    print(obji)

    cat(display)
    cat("\nSkill Probabilities \n\n")
    print(round(object$skill.patt,rdigits) )

    #**** output tetrachoric or polychoric correlations
    cat(display)
    cat("\nPolychoric Correlations \n")
    G <- object$G
    for (gg in 1:G){
        cat( paste0( "\nGroup ", gg, "\n") )
        obji <- object$polychor[[gg]]
        print( round( obji, 3 ))
    }

    cat("\n", display)
    cat("\nSkill Pattern Probabilities \n\n")
    if ( object$G==1 ){
        xt <- round( object$attribute.patt[,1], rdigits )
        names(xt) <- rownames( object$attribute.patt )
    } else {
        xt <- round( object$attribute.patt, rdigits )
        rownames(xt) <- rownames( object$attribute.patt )
    }
    print(xt)

    if (object$HOGDINA>=0){
        cat("\n", display)
        cat("Higher Order GDINA Model ")
        cat("\n  Attribute Response Function Parameters \n\n")
        print( round( object$attr.rf,3) )
    }

    csink( file=file )
}

