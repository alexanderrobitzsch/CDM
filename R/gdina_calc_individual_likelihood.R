
gdina_calc_individual_likelihood <- function(IP, L, pjM, item.patt.split, J,
		resp.ind.list, zeroprob.skillclasses)
{
	h1 <- matrix( 1 , nrow=IP , ncol=L )	
	p.xi.aj <- cdm_calc_posterior( rprobs=pjM, gwt=h1, resp=item.patt.split, nitems=J, 
					resp.ind.list=resp.ind.list, normalization=FALSE, thetasamp.density=NULL, 
					snodes=0 )$hwt	
	if ( ! is.null(zeroprob.skillclasses) ){
		p.xi.aj[ , zeroprob.skillclasses ] <- 0
	}	
	#--- OUTPUT
	return(p.xi.aj)
}