## File Name: gdina_mstep_item_ml_mono_optim_function.R
## File Version: 0.06


gdina_mstep_item_ml_mono_optim_function <- function()
{
    CDM_require_namespace("ROI")
    fn <- paste0("ROI", paste0(rep(":",3), collapse=""),"nlminb2")
    fn <- eval(parse(text=fn))
    return(fn)
}
