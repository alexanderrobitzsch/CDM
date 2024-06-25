## File Name: cdm_attach_exported_function.R
## File Version: 0.02


cdm_attach_exported_function <- function(pack, fun)
{
    CDM_require_namespace(pkg=pack)
    fn <- paste0(pack, paste0(rep(':',2), collapse=''), fun)
    fn <- eval(parse(text=fn))
    return(fn)
}
