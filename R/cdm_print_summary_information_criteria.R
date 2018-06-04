## File Name: cdm_print_summary_information_criteria.R
## File Version: 0.20


cdm_print_summary_information_criteria <- function(object, digits_crit=0, digits_penalty=2)
{
    deviance <- object$ic$deviance
    #*** labels information criteria
    labels <- list( AIC=paste0("AIC ","=", " -2*LL + 2*p" ) )
    labels$AICc <- paste0("AICc ","=", " -2*LL + 2*p + 2*p*(p+1)/(n-p-1)  (bias corrected AIC)" )
    labels$BIC <- paste0("BIC ","=", " -2*LL + log(n)*p " )
    labels$CAIC <- paste0("CAIC ","=", " -2*LL + [log(n)+1]*p  (consistent AIC) " )

    #*** display criteria
    crits <- intersect(names(labels), names(object$ic))
    for (crit_name in crits){
        res <- cdm_print_summary_information_criteria_one_criterium( object=object,
                crit_name=crit_name, labels=labels, digits_crit=digits_crit,
                digits_penalty=digits_penalty )
    }
    cat("\n")
}
