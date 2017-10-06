## File Name: cdm_pem_inits_assign_parmlist.R
## File Version: 0.01
## File Last Change: 2017-10-05 17:54:13


cdm_pem_inits_assign_parmlist <- function(pem_pars, envir)
{
	parmlist <- list()
	NP <- length(pem_pars)	
	for (pp in 1:NP){
		p1 <- get( pem_pars[pp], pos=envir )
		parmlist[[ pp ]] <- p1
	}
	names(parmlist) <- pem_pars
	return(parmlist)
}
