//// File Name: init.c
//// File Version: 0.08
//// File Last Change: 2017-10-05 18:41:35
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _CDM_calc_scoredistribution_cdm(SEXP, SEXP);
extern SEXP _CDM_calc_slca_deriv(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_calc_slca_probs(SEXP, SEXP, SEXP);
extern SEXP _CDM_calc_Xdes(SEXP, SEXP);
extern SEXP _CDM_calccounts_pcm_groups_C(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_kli_id_C(SEXP, SEXP);
extern SEXP _CDM_din_deterministic_devcrit_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_din_jml_devcrit_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_generalized_distance_method__C(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_ideal_resp_pattern__C(SEXP, SEXP);
extern SEXP _CDM_IRT_predict(SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_modelfit_cor2_Cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_probs_pcm_groups_C(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP calcfx(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_CDM_calc_scoredistribution_cdm",     (DL_FUNC) &_CDM_calc_scoredistribution_cdm,     2},
    {"_CDM_calc_slca_deriv",                (DL_FUNC) &_CDM_calc_slca_deriv,                6},
    {"_CDM_calc_slca_probs",                (DL_FUNC) &_CDM_calc_slca_probs,                3},
    {"_CDM_calc_Xdes",                      (DL_FUNC) &_CDM_calc_Xdes,                      2},
    {"_CDM_calccounts_pcm_groups_C",        (DL_FUNC) &_CDM_calccounts_pcm_groups_C,        7},
    {"_CDM_cdm_kli_id_C",                   (DL_FUNC) &_CDM_cdm_kli_id_C,                   2},
    {"_CDM_din_deterministic_devcrit_C",    (DL_FUNC) &_CDM_din_deterministic_devcrit_C,    5},
    {"_CDM_din_jml_devcrit_C",              (DL_FUNC) &_CDM_din_jml_devcrit_C,              5},
    {"_CDM_generalized_distance_method__C", (DL_FUNC) &_CDM_generalized_distance_method__C, 6},
    {"_CDM_ideal_resp_pattern__C",          (DL_FUNC) &_CDM_ideal_resp_pattern__C,          2},
    {"_CDM_IRT_predict",                    (DL_FUNC) &_CDM_IRT_predict,                    4},
    {"_CDM_modelfit_cor2_Cpp",              (DL_FUNC) &_CDM_modelfit_cor2_Cpp,              7},
    {"_CDM_probs_pcm_groups_C",             (DL_FUNC) &_CDM_probs_pcm_groups_C,             6},
    {"calcfx",                              (DL_FUNC) &calcfx,                              4},
    {NULL, NULL, 0}
};

void R_init_CDM(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
