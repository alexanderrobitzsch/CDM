## File Name: sim_model.R
## File Version: 0.10

sim_model <- function(object=NULL, irfprob=NULL, theta_index=NULL,
    prob.theta=NULL, data=NULL, N_sim=NULL )
{
    theta <- NULL
    if ( ! is.null(object)){
        #--- extract parameter
        irfprob <- IRT.irfprob(object)
        theta <- attr(irfprob, "theta")
        prob.theta <- attr(irfprob, "prob.theta")
        data <- IRT.data(object)
        if (is.null(N_sim)){
            N_sim <- nrow(data)
        }
    }
    if ( is.null(N_sim) & ( ! is.null(theta_index) ) ){
        N_sim <- length(theta_index)
    }
    #--- sample theta
    TP <- length(prob.theta)
    if ( ( ! is.null(N_sim) ) & ( is.null(theta_index) ) ){
        theta_index <- sample( 1:TP, size=N_sim, prob=prob.theta, replace=TRUE )
        if (! is.null(theta)){
            theta <- theta[ theta_index, ]
        }
    }
    dim_irfprob <- dim(irfprob)
    #** apply sampling function
    dat <- cdm_rcpp_sim_model_item_responses( theta_index=theta_index-1,
                irfprob=as.vector(irfprob), dim_irfprob=dim_irfprob)
    N_sim <- nrow(dat)
    #*** include missings
    if (! is.null(data) ){
        ind_miss <- sample(1:nrow(data), size=N_sim, replace=TRUE)
        dat[ is.na( data[ind_miss,] ) ] <- NA
    }
    #--- output
    res <- list( dat=dat, theta=theta, theta_index=theta_index)
    return(res)
}
