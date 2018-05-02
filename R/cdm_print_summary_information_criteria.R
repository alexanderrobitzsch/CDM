## File Name: cdm_print_summary_information_criteria.R
## File Version: 0.06


cdm_print_summary_information_criteria <- function(object, digits_crit=0, digits_penalty=2)
{
    #-- AIC
    cat( "AIC  = " , round( object$ic$AIC , digits_crit ) , " | penalty =" ,
                    round( object$ic$AIC - object$ic$deviance ,digits_penalty ) ,
            "   | AIC = -2*LL + 2*p  \n" )
    #-- AICc
    if ( "AICc" %in% names(object$ic) ){
        cat( "AICc = " , round( object$ic$AICc , digits_crit ) ," | penalty =",
                round( object$ic$AICc - object$ic$deviance ,digits_penalty ) )
        cat("    | AICc = -2*LL + 2*p + 2*p*(p+1)/(n-p-1)  (bias corrected AIC)\n" )
    }
    #-- BIC
    cat( "BIC  = " , round( object$ic$BIC , digits_crit ) , " | penalty =" ,
                    round( object$ic$BIC - object$ic$deviance ,digits_penalty ) ,
            "   | BIC = -2*LL + log(n)*p  \n" )
    #-- CAIC
    cat( "CAIC = " , round( object$ic$CAIC , digits_crit ) ," | penalty =" ,
                        round( object$ic$CAIC - object$ic$deviance ,digits_penalty ) )
        cat("   | CAIC = -2*LL + [log(n)+1]*p  (consistent AIC)\n\n" )
}
