#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP calcfx(SEXP, SEXP, SEXP, SEXP);
extern SEXP CDM_calc_scoredistribution_cdm(SEXP, SEXP);
extern SEXP CDM_calc_slca_deriv(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CDM_calc_slca_probs(SEXP, SEXP, SEXP);
extern SEXP CDM_calc_Xdes(SEXP, SEXP);
extern SEXP CDM_calccounts_pcm_groups_C(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CDM_cdm_kli_id_C(SEXP, SEXP);
extern SEXP CDM_din_deterministic_devcrit_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CDM_din_jml_devcrit_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CDM_generalized_distance_method__C(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CDM_ideal_resp_pattern__C(SEXP, SEXP);
extern SEXP CDM_IRT_predict(SEXP, SEXP, SEXP, SEXP);
extern SEXP CDM_modelfit_cor2_Cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CDM_probs_pcm_groups_C(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"calcfx",                             (DL_FUNC) &calcfx,                             4},
    {"CDM_calc_scoredistribution_cdm",     (DL_FUNC) &CDM_calc_scoredistribution_cdm,     2},
    {"CDM_calc_slca_deriv",                (DL_FUNC) &CDM_calc_slca_deriv,                6},
    {"CDM_calc_slca_probs",                (DL_FUNC) &CDM_calc_slca_probs,                3},
    {"CDM_calc_Xdes",                      (DL_FUNC) &CDM_calc_Xdes,                      2},
    {"CDM_calccounts_pcm_groups_C",        (DL_FUNC) &CDM_calccounts_pcm_groups_C,        7},
    {"CDM_cdm_kli_id_C",                   (DL_FUNC) &CDM_cdm_kli_id_C,                   2},
    {"CDM_din_deterministic_devcrit_C",    (DL_FUNC) &CDM_din_deterministic_devcrit_C,    5},
    {"CDM_din_jml_devcrit_C",              (DL_FUNC) &CDM_din_jml_devcrit_C,              5},
    {"CDM_generalized_distance_method__C", (DL_FUNC) &CDM_generalized_distance_method__C, 6},
    {"CDM_ideal_resp_pattern__C",          (DL_FUNC) &CDM_ideal_resp_pattern__C,          2},
    {"CDM_IRT_predict",                    (DL_FUNC) &CDM_IRT_predict,                    4},
    {"CDM_modelfit_cor2_Cpp",              (DL_FUNC) &CDM_modelfit_cor2_Cpp,              7},
    {"CDM_probs_pcm_groups_C",             (DL_FUNC) &CDM_probs_pcm_groups_C,             6},
    {NULL, NULL, 0}
};

void R_init_CDM(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
