## File Name: cdm_est_calc_accuracy_version2_classify_simulated_data.R
## File Version: 0.04


cdm_est_calc_accuracy_version2_classify_simulated_data <- function( data, irfprob, prior)
{
    like <- eval_likelihood(data=data, irfprob=irfprob, prior=NULL, normalization=FALSE)
    res_like <- cdm_rcpp_irt_classify_individuals(like=like)
    post <- eval_likelihood(data=data, irfprob=irfprob, prior=prior, normalization=TRUE)
    res_post <- cdm_rcpp_irt_classify_individuals(like=post)
    #--- output
    res <- list( like=like, MLE=res_like$class_index, post=post, MAP=res_post$class_index)
    return(res)
}


