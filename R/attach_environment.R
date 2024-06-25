## File Name: attach_environment.R
## File Version: 0.05


#######################################
# attach all elements in an environment
attach_environment <- function( res, envir )
{
    CC <- length(res)
    for (cc in 1L:CC){
        assign( names(res)[cc], res[[cc]], envir=envir )
    }
}

.attach.environment <- attach_environment

