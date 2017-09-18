## File Name: rrumpars2logpars.R
## File Version: 0.02
## File Last Change: 2017-01-31 14:07:29


#######################################################	
#   pi * r1^(1-a1) * r2^(1-a2)				
# = pi * r1 * r2 * r1^(-a1) * r2^(-a2)
# = pi * r1 * r2 * (1/r1)^a1 * (1/r2)^a2
rrumpars2logpars <- function(v1){
	l1 <- rep(0,length(v1))
	l1[-1] <- log( 1 / v1[-1] )
	l1[1] <- log( prod(v1) )
	return(l1)	
		}
#######################################################	
