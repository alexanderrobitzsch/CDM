## File Name: reglca_bound_classprobs.R
## File Version: 0.04


#-- bound skill class probabilities
reglca_bound_classprobs <- function(class_probs, min_class_probs=1e-4)
{
    if (any(is.na(class_probs))){
        TP <- length(class_probs)
        class_probs <- rep(1/TP, TP)
    }
    ind <- class_probs < min_class_probs
    if (sum(ind)>0){
        class_probs[ind] <- min_class_probs
        class_probs <- cdm_sumnorm(class_probs)
    }
    return(class_probs)
}
