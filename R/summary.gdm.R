## File Name: summary.gdm.R
## File Version: 1.35
#*******************************************************
# Summary for gdm object
summary.gdm <- function( object, file=NULL, ... )
{

    osink( file=file, suffix=paste0( "__SUMMARY.Rout") )

    cat("-----------------------------------------------------------------------------\n")
    d1 <- utils::packageDescription("CDM")
    cat( paste( d1$Package, " ", d1$Version, " (", d1$Date, ")", sep=""), "\n\n" )
    cat( "Date of Analysis:", paste( object$time$s2 ), "\n" )
    cat("Computation Time:", print(object$time$s2 - object$time$s1), "\n\n")
    cat("General Diagnostic Model \n\n")
    modeltype <- object$irtmodel

    cat( "   ", object$N, "Cases, ", object$I, "Items, ", object$G, "Group(s)", ",",
                object$D, "Dimension(s)\n")


    if (object$skillspace=="normal" ){
        cat("    Normal distribution assumption\n" )
    }
    if (object$skillspace=="loglinear" ){
        cat("    Log-linear Smoothing (up to 3 Moments)\n" )
    }
    if (object$skillspace=="full" ){
        cat("    Saturated skill space\n" )
    }
    if (object$skillspace=="est" ){
        cat("    Saturated skill space with estimated trait grid\n" )
    }

    if (object$G > 1 ){
        cat("\nGroup statistics\n")
        print( object$group.stat )
    }
    cat("\n-----------------------------------------------------------------------------\n")
    cat( "Number of iterations=", object$iter   )
    if ( ! object$converged ){ cat("\nMaximum number of iterations was reached.") }

    cat_paste( "\n\nDeviance", xx(), round( object$deviance, 2 ), " | " )
    cat_paste( "Log Likelihood", xx(), round( -object$deviance/2, 2 ), "\n" )
    cat_paste( "Number of persons", xx(), object$ic$n, "\n" )

    cat_paste( "Number of estimated parameters", xx(), object$ic$np, "\n" )
    cat_paste( "  Number of estimated item parameters", xx(), object$ic$itempars, "\n" )
    cat_paste( "      ", object$ic$itempars.b, " Intercepts and ", object$ic$itempars.a, " Slopes \n")
    cat_paste( "      ", object$ic$centeredintercepts, " centered intercepts and ",
            object$ic$centeredslopes, " centered slopes \n")
    cat_paste( "  Number of estimated distribution parameters", xx(),
                        object$ic$traitpars, "\n\n" )

    #** print information criteria
    cdm_print_summary_information_criteria(object=object)

    cat("-----------------------------------------------------------------------------\n")
    cat("Trait Distribution\n")
    obji <- object$pi.k
    cat( "\nM Trait:\n" )
    print( round( t(object$mean.trait ), 3 ) )
    cat( "\nSD Trait:\n" )
    print( round( t(object$sd.trait ), 3 ) )
    cat( "\nSkewness Trait:\n" )
    print( round( t(object$skewness.trait ), 3 ) )
    cat( "\nCorrelations Trait: \n" )
    for (gg in 1:object$G){
        cat("Group", gg, "\n")
        print( round( object$correlation.trait[[gg]], 3 ) )
    }
    if ( object$skillspace=="est" ){
        cat("\n\nEstimated Skill Distribution\n")
        dfr <- data.frame( "theta.k"=object$theta.k, "pi.k"=object$pi.k )
        NV <- ncol(dfr)
        for (vv in 1:(NV-1) ){ dfr[,vv] <- round( dfr[,vv], 3 ) }
        vv <- NV ; dfr[,vv] <- round( dfr[,vv], 5 )
        print(dfr)
        cat("")
    }
    cat( "\nEAP Reliability:\n" )
    print( round( t(object$EAP.rel ), 3 ) )
    cat("-----------------------------------------------------------------------------\n")
    cat("Item Parameters \n")
    obji <- object$item
    obji[,-1] <- round( obji[,-1], 3)
    print( obji )
    cat("\nMean of RMSEA item fit:",
    round( object$mean.rmsea,3 ), "\n")

    csink( file=file )
}
#*******************************************************
