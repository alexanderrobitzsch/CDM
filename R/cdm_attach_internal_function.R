## File Name: cdm_attach_internal_function.R
## File Version: 0.03

cdm_attach_internal_function <- function(pack, fun)
{
    CDM_require_namespace(pkg=pack)
    fn <- paste0(pack, paste0(rep(":",3), collapse=""), fun)
    fn <- eval(parse(text=fn))
    return(fn)
}
