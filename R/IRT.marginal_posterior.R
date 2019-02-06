## File Name: IRT.marginal_posterior.R
## File Version: 0.13



IRT.marginal_posterior <- function(object, dim, remove_zeroprobs=TRUE, ...)
{
    UseMethod("IRT.marginal_posterior")
}

IRT_marginal_posterior_compute <- function(object, dim,
    remove_zeroprobs=TRUE, ... )
{
    post <- IRT.posterior(object=object)
    theta <- attr(post, "theta")
    skill_names <- colnames(theta)
    if ( ! is.numeric(dim) ){
        dim <- match(dim, skill_names)
    }
    ND <- length(dim)
    if (remove_zeroprobs){
        ind <- which( colSums(post) > 0 )
        post <- post[, ind, drop=FALSE]
        theta <- theta[ ind,, drop=FALSE]
    }
    N <- nrow(post)
    LD <- length(dim)
    sp <- ""
    for (ll in 1:LD){
        sp <- paste0(sp,theta[,ll])
    }
    spu <- sort(unique(sp))
    NS <- length(spu)
    index_sp <- match(sp, spu)
    marg_post <- matrix(NA, nrow=N, ncol=NS)
    colnames(marg_post) <- spu
    for (ss in 1:NS){
        marg_post[,ss] <- rowSums( post[, index_sp==ss] )
    }
    # MAP estimate
    map <- spu[ max.col(marg_post) ]
    # skill design
    theta <- strsplit(spu, split="")
    theta <- matrix( as.numeric(unlist(theta)), ncol=ND, byrow=TRUE)
    colnames(theta) <- skill_names[dim]
    rownames(theta) <- spu

    #-- output
    res <- list(marg_post=marg_post, map=map, theta=theta)
    return(res)
}

IRT.marginal_posterior.gdina <- IRT_marginal_posterior_compute
IRT.marginal_posterior.din <- IRT_marginal_posterior_compute
IRT.marginal_posterior.mcdina <- IRT_marginal_posterior_compute
