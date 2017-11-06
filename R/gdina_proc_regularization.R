## File Name: gdina_proc_regularization.R
## File Version: 0.12

gdina_proc_regularization <- function( regular_type, cd, mono.constr, linkfct,
	method, PEM, regular_alpha, regular_tau  )
{
	save.devmin <- TRUE
	regularization <- FALSE
	cd_algorithm <- FALSE
	regularization_types <- c("lasso","scad", "elnet","ridge", "scadL2", "tlp", "mcp")
	if (regular_type %in% regularization_types ){
		regularization <- TRUE
		cd_algorithm <- TRUE
		method <- "ML"
	}
	if ( cd ){ 
		cd_algorithm <- TRUE 
	}	
	if ( mono.constr ){
		linkfct <- "logit"
		method <- "ML"
	}
	if (regularization){
		save.devmin <- FALSE
		linkfct <- "logit"
		PEM <- FALSE
	}
	if ( ! ( regular_type %in% c("elnet", "scadL2") ) ){
		regular_alpha <- NA
	}
	if ( ! ( regular_type %in% c("tlp") ) ){
		regular_tau <- NA
	}
	
	#---- output
	res <- list( linkfct=linkfct, save.devmin=save.devmin, method=method,
				regularization=regularization, cd_algorithm=cd_algorithm,
				PEM=PEM, regular_alpha=regular_alpha, regular_tau=regular_tau,
				regularization_types=regularization_types )
	return(res)
}