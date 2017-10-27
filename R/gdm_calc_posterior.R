## File Name: gdm_calc_posterior.R
## File Version: 0.01

gdm_calc_posterior <- function(probs, gwt0, dat, I, resp.ind.list)
{
	res <- cdm_calc_posterior( rprobs=probs, gwt=gwt0, resp=dat, nitems=I, resp.ind.list=resp.ind.list, 
						normalization=FALSE, thetasamp.density=NULL, snodes=0 ) 
	return(res)
}
