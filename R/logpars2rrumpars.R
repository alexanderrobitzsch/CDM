## File Name: logpars2rrumpars.R
## File Version: 0.02
## File Last Change: 2017-01-31 14:07:28


###############################################
# log parametrization to rum parametrization
#   pi * r1^(1-a1) * r2^(1-a2)
# = pi * r1 * r2 * (1/r1)^a1 * (1/r2)^a2
logpars2rrumpars <- function(delta_jj){
	v1 <- delta_jj
	N <- length(v1)
	v1 <- 1 / exp( delta_jj )
	v1[1] <- exp(sum( delta_jj))
	return(v1)
		}
###############################################	
