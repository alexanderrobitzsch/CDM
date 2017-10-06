## File Name: slca_calc_prob.R
## File Version: 0.02
## File Last Change: 2017-10-03 18:53:44


#############################################################
# Rcpp function for calculating probabilities
slca_calc_prob <- function( XdesM , dimXdes , Xlambda )
{
   res <- calc_slca_probs( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda )
   I <- dimXdes[1]
   maxK <- dimXdes[2]
   TP <- dimXdes[3]
   probs <- array( res , dim=c( I , maxK , TP ))   
   return(probs)
}

.slca.calc.prob <- slca_calc_prob
