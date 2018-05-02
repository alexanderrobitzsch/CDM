## File Name: IRT.frequencies.R
## File Version: 0.05


###########################################################
# extracts used dataset
IRT.frequencies <- function(object, ...)
{
    UseMethod("IRT.frequencies")
}

IRT_frequencies_wrapper <- function(object, ...)
{
    data <- IRT.data(object=object)
    weights <- attr(data, "weights")
    post <- IRT.posterior(object=object)
    probs <- IRT.irfprob(object=object)
    res <- IRT_frequencies_default(data=data, post=post, probs=probs, weights=weights)
    return(res)
}

IRT.frequencies.din <- IRT_frequencies_wrapper
IRT.frequencies.gdina <- IRT_frequencies_wrapper
IRT.frequencies.gdm <- IRT_frequencies_wrapper
IRT.frequencies.mcdina <- IRT_frequencies_wrapper
IRT.frequencies.slca <- IRT_frequencies_wrapper
#############################################################
