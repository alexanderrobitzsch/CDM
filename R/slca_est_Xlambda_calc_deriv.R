## File Name: slca_est_Xlambda_calc_deriv.R
## File Version: 0.01

slca_est_Xlambda_calc_deriv <- function(XdesM, dimXdes, Xlambda, probs, n.ik, N.ik)
{
	res <- calc_slca_deriv( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda , probs=as.vector(probs) ,
					nik=as.vector(n.ik) , Nik=as.vector(N.ik) )   
	return(res)
}
