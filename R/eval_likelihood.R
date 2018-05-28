## File Name: eval_likelihood.R
## File Version: 0.08

eval_likelihood <- function( data, irfprob, prior=NULL, normalization=FALSE, N=NULL )
{
    long_format <- inherits(x=data, what="data_long_format")
    if ( is.null(N) ){
        N <- nrow(data)
        if (long_format){
            N <- data[ N, 1 ] + 1
        }
    }
    TP <- dim(irfprob)[3]
    #-- set prior if no prior is provided
    if (is.null(prior)){
        prior <- matrix(1, nrow=N, ncol=TP)
    }
    if (is.vector(prior)){
        prior <- matrix( prior, nrow=N, ncol=TP, byrow=TRUE)
    }
    #-- evaluate likelihood in Rcpp
    res <- cdm_rcpp_eval_likelihood(data=data, irfprob=as.vector(irfprob),
                dim_irfprob=dim(irfprob), prior=prior, normalization=normalization,
                long_format=long_format, N=N )
    return(res)
}
