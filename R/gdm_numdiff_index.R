## File Name: gdm_numdiff_index.R
## File Version: 0.02
## File Last Change: 2017-10-06 10:38:30

		
####################################################################
# general function for numerical differentiation
# diffindex aggregates across super items
gdm_numdiff_index <- function( pjk , pjk1 , pjk2 , n.ik , 
		max.increment , numdiff.parm , eps=1E-80 )
{					
	h <- numdiff.parm
	eps <- 1E-10
    an.ik <- aperm( n.ik , c(2,3,1) )
    ll0 <- colSums( colSums( an.ik * log(pjk+eps) ))
    ll1 <- colSums( colSums( an.ik * log(pjk1+eps) ))
    ll2 <- colSums( colSums( an.ik * log(pjk2+eps) ))
    d1 <- ( ll1 - ll2  ) / ( 2 * h )    # negative sign?
    # second order derivative
    # f(x+h)+f(x-h) = 2*f(x) + f''(x)*h^2
    d2 <- ( ll1 + ll2 - 2*ll0 ) / h^2
    # change in item difficulty
    d2[ abs(d2) < eps ] <- eps
    increment <- - d1 / d2
	ci <- ceiling( abs(increment) / ( abs( max.increment) + eps ) )
    increment <- ifelse( abs( increment) > abs(max.increment)  , 
                                 increment/(2*ci) , increment )	
	#--- output
	res <- list(increment=increment , d2=d2 , ll0=ll0)
	return(res)
}


.gdm.numdiff.index <- gdm_numdiff_index
