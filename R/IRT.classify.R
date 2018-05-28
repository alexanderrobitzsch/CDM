## File Name: IRT.classify.R
## File Version: 0.05

IRT.classify <- function(object, type="MLE")
{
    if (type=="MLE"){
        like <- IRT.likelihood(object)
    } else {  # type=="MAP"
        like <- IRT.posterior(object)
    }
    theta <- attr(like, "theta")
    #-- individual classification
    res <- cdm_rcpp_irt_classify_individuals( like=like )
    class_index <- res$class_index
    class_maxval <- res$class_maxval
    class_theta <- theta[ class_index, ]
    #--- output
    res <- list( class_theta=class_theta, class_index=class_index,
                        class_maxval=class_maxval, type=type )
    return(res)
}
