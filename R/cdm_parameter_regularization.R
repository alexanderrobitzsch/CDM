## File Name: cdm_parameter_regularization.R
## File Version: 0.20


cdm_parameter_regularization <- function(x, regular_type, regular_lam,
        regular_alpha=NULL, regular_tau=NULL)
{
    y <- x
    #--- scad
    if ( regular_type=="scad"){
        y <- cdm_penalty_threshold_scad( beta=x, lambda=regular_lam)
    }
    #--- lasso
    if ( regular_type=="lasso"){
        y <- cdm_penalty_threshold_lasso( val=x, eta=regular_lam )
    }
    #--- ridge
    if ( regular_type=="ridge"){
        y <- cdm_penalty_threshold_ridge( beta=x, lambda=regular_lam )
    }
    #--- elastic net
    if ( regular_type=="elnet"){
        y <- cdm_penalty_threshold_elnet( beta=x, lambda=regular_lam, alpha=regular_alpha )
    }
    #--- scadL2
    if ( regular_type=="scadL2"){
        y <- cdm_penalty_threshold_scadL2( beta=x, lambda=regular_lam, alpha=regular_alpha)
    }
    #--- tlp
    if ( regular_type=="tlp"){
        y <- cdm_penalty_threshold_tlp( beta=x, lambda=regular_lam, tau=regular_tau)
    }
    #--- mcp
    if ( regular_type=="mcp"){
        y <- cdm_penalty_threshold_mcp( beta=x, lambda=regular_lam)
    }
    return(y)
}
