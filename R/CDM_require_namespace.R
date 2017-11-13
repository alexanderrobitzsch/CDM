## File Name: CDM_require_namespace.R
## File Version: 0.04

CDM_require_namespace <- function(pkg)
{
	if ( ! requireNamespace( pkg , quietly = TRUE) ){
		stop( paste0("Package '" , pkg , "' is needed for applying this 
			function. Please install it." ), call. = FALSE)
	}
}
