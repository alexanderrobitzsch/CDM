
CDM_require_namespace <- function(pkg)
{
    if ( ! requireNamespace( pkg , quietly = TRUE) ){
        stop( paste0("Package '" , pkg , "' needed fo this function 
             to work. Please install it." ), call. = FALSE)
    }
}