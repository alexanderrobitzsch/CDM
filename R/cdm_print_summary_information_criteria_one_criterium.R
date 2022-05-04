## File Name: cdm_print_summary_information_criteria_one_criterium.R
## File Version: 0.042


cdm_print_summary_information_criteria_one_criterium <- function(object, crit_name,
                    labels, digits_crit, digits_penalty)
{
    deviance <- object$ic$deviance
    crit <- object$ic[[ crit_name ]]
    cat( crit_name, "=", round( crit, digits_crit ), " | penalty", "=",
                    round( crit - deviance, digits_penalty ),
            "   |", labels[[ crit_name ]], " \n" )
}
