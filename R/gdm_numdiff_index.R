## File Name: gdm_numdiff_index.R
## File Version: 0.05
## File Last Change: 2017-10-08 15:25:04

		
####################################################################
# general function for numerical differentiation
# diffindex aggregates across super items
gdm_numdiff_index <- function( pjk , pjk1 , pjk2 , n.ik , 
		max.increment , numdiff.parm , eps=1E-10 )
{					
	h <- numdiff.parm
    an.ik <- aperm( n.ik , c(2,3,1) )
	#--- log-likelihood
	ll0 <- cdm_calc_ll_with_counts( an.ik=an.ik, pjk=pjk )
    ll1 <- cdm_calc_ll_with_counts( an.ik=an.ik, pjk=pjk1 )
    ll2 <- cdm_calc_ll_with_counts( an.ik=an.ik, pjk=pjk2 )
	
	#--- derivatives
    res <- cdm_ll_numerical_differentiation( ll0=ll0, ll1=ll1, ll2=ll2, h=h ) 
	d1 <- res$d1
	d2 <- res$d2
	#--- calculate increment
    res <- cdm_calc_increment( d1=d1, d2=d2, max.increment=max.increment, type=2 )
	increment <- res$increment
						 
	#--- output
	res <- list(increment=increment , d1=d1, d2=d2 , ll0=ll0)
	return(res)
}


.gdm.numdiff.index <- gdm_numdiff_index
