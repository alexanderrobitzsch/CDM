## File Name: slca_proc_design_matrix_xlambda.R
## File Version: 0.03

slca_proc_design_matrix_xlambda <- function(Xdes)
{
    dimXdes <- dim(Xdes)
    res <- cdm_rcpp_slca_calc_Xdes( XDES=as.vector(Xdes), dimXdes=dimXdes )
    # XdesM     [ii,kk,tt,ll, value ]
    NX <- res$NXdesM
    XdesM <- res$XdesM[1:NX,]
    XdesM <- XdesM[ order( XdesM[,1]*NX + XdesM[,3] ) , ]
    #--- output
    res <- list(XdesM=XdesM, NX=NX, dimXdes=dimXdes )
    return(res)
}
