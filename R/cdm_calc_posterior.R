## File Name: cdm_calc_posterior.R
## File Version: 1.175


#--- compute posterior distribution
cdm_calc_posterior <- function(rprobs, gwt, resp, nitems,
    resp.ind.list, normalization=TRUE,
    thetasamp.density=NULL, snodes=0 )
{
    if ( snodes==0 ){
        fx <- gwt
    } else {
        # calculate individual 'sampling weight'
        swt <- fx <- gwt / outer( rep(1, nrow(gwt)), thetasamp.density )
    }
    nstud <- nrow(fx)

    # using C Code
    storage.mode(resp) <- "integer"
    fx <- .Call( '_CDM_calcfx', PACKAGE='CDM', fx, rprobs, resp.ind.list, resp)
    # numerical integration
    if ( snodes==0 ){
        rfx <- rowSums(fx)
        if (normalization ){
            hwt <- fx / rfx
        } else {
            hwt <- fx
        }
    }
    # Monte Carlo integration
    if ( snodes > 0 ){
        rfx <- rowMeans(fx)
        if (normalization ){
            hwt <- fx / rfx
        } else {
            hwt <- fx
        }
    }
    res <-  list("hwt"=hwt, "rfx"=rfx )
    if ( snodes > 0 ){
        res[["swt" ]] <- swt
    }
    return(res)
}
#..........................................................

calc_posterior.v2 <- cdm_calc_posterior
