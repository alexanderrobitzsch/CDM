## File Name: gdina_mstep_item_ml_mono_optim_function.R
## File Version: 0.133


gdina_mstep_item_ml_mono_optim_function <- function()
{
    CDM_require_namespace(pkg="ROI")
    fun <- cdm_attach_internal_function(pack="ROI", fun="nlminb2")
    # fun <- ROI::nlminb2
    return(fun)
}
