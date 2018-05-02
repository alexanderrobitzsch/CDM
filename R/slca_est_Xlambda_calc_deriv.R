## File Name: slca_est_Xlambda_calc_deriv.R
## File Version: 0.03

slca_est_Xlambda_calc_deriv <- function(XdesM, dimXdes, Xlambda, probs, n.ik, N.ik)
{
    res <- cdm_rcpp_slca_calc_deriv( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda ,
                    probs=as.vector(probs), nik=as.vector(n.ik) , Nik=as.vector(N.ik) )
    return(res)
}
