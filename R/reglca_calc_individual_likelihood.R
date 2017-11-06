## File Name: reglca_calc_individual_likelihood.R
## File Version: 0.01


reglca_calc_individual_likelihood <- function(N, nclasses, pjM, dat, I, resp.ind.list)
{
	h1 <- matrix( 1 , nrow=N , ncol=nclasses )	
	p.xi.aj <- cdm_calc_posterior( rprobs=pjM, gwt=h1, resp=dat, nitems=I, 
					resp.ind.list=resp.ind.list, normalization=FALSE, thetasamp.density=NULL, 
					snodes=0 )$hwt	
	#--- OUTPUT
	return(p.xi.aj)
}
