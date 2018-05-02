//// File Name: init.c
//// File Version: 6.002091
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP _CDM_calcfx(SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_din_deterministic_devcrit(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_din_jml_devcrit(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_din_calc_prob(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_din_calc_counts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_din_validate_aggregate_max(SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_din_validate_update_qmatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_discrimination_index_attribute_patterns(SEXP);
extern SEXP _CDM_cdm_rcpp_discrimination_index_calc(SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_discrimination_index_test_level(SEXP);
extern SEXP _CDM_cdm_rcpp_discrimination_index_idi(SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_generalized_distance_method(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_ideal_resp_pattern(SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_irt_predict(SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_itemfit_sx2_calc_scoredistribution(SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_kli_id(SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_mcdina_probs_pcm_groups(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_mcdina_calccounts_pcm_groups(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_modelfit_cor2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_modelfit_cor_counts(SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_slca_calc_probs(SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_slca_calc_deriv(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _CDM_cdm_rcpp_slca_calc_Xdes(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_CDM_calcfx", (DL_FUNC) &_CDM_calcfx, 4},
    {"_CDM_cdm_rcpp_din_deterministic_devcrit", (DL_FUNC) &_CDM_cdm_rcpp_din_deterministic_devcrit, 5},
    {"_CDM_cdm_rcpp_din_jml_devcrit", (DL_FUNC) &_CDM_cdm_rcpp_din_jml_devcrit, 5},
    {"_CDM_cdm_rcpp_din_calc_prob", (DL_FUNC) &_CDM_cdm_rcpp_din_calc_prob, 5},
    {"_CDM_cdm_rcpp_din_calc_counts", (DL_FUNC) &_CDM_cdm_rcpp_din_calc_counts, 6},
    {"_CDM_cdm_rcpp_din_validate_aggregate_max", (DL_FUNC) &_CDM_cdm_rcpp_din_validate_aggregate_max, 3},
    {"_CDM_cdm_rcpp_din_validate_update_qmatrix", (DL_FUNC) &_CDM_cdm_rcpp_din_validate_update_qmatrix, 8},
    {"_CDM_cdm_rcpp_discrimination_index_attribute_patterns", (DL_FUNC) &_CDM_cdm_rcpp_discrimination_index_attribute_patterns, 1},
    {"_CDM_cdm_rcpp_discrimination_index_calc", (DL_FUNC) &_CDM_cdm_rcpp_discrimination_index_calc, 4},
    {"_CDM_cdm_rcpp_discrimination_index_test_level", (DL_FUNC) &_CDM_cdm_rcpp_discrimination_index_test_level, 1},
    {"_CDM_cdm_rcpp_discrimination_index_idi", (DL_FUNC) &_CDM_cdm_rcpp_discrimination_index_idi, 3},
    {"_CDM_cdm_rcpp_generalized_distance_method", (DL_FUNC) &_CDM_cdm_rcpp_generalized_distance_method, 6},
    {"_CDM_cdm_rcpp_ideal_resp_pattern", (DL_FUNC) &_CDM_cdm_rcpp_ideal_resp_pattern, 2},
    {"_CDM_cdm_rcpp_irt_predict", (DL_FUNC) &_CDM_cdm_rcpp_irt_predict, 4},
    {"_CDM_cdm_rcpp_itemfit_sx2_calc_scoredistribution", (DL_FUNC) &_CDM_cdm_rcpp_itemfit_sx2_calc_scoredistribution, 2},
    {"_CDM_cdm_rcpp_kli_id", (DL_FUNC) &_CDM_cdm_rcpp_kli_id, 2},
    {"_CDM_cdm_rcpp_mcdina_probs_pcm_groups", (DL_FUNC) &_CDM_cdm_rcpp_mcdina_probs_pcm_groups, 6},
    {"_CDM_cdm_rcpp_mcdina_calccounts_pcm_groups", (DL_FUNC) &_CDM_cdm_rcpp_mcdina_calccounts_pcm_groups, 7},
    {"_CDM_cdm_rcpp_modelfit_cor2", (DL_FUNC) &_CDM_cdm_rcpp_modelfit_cor2, 7},
    {"_CDM_cdm_rcpp_modelfit_cor_counts", (DL_FUNC) &_CDM_cdm_rcpp_modelfit_cor_counts, 2},
    {"_CDM_cdm_rcpp_slca_calc_probs", (DL_FUNC) &_CDM_cdm_rcpp_slca_calc_probs, 3},
    {"_CDM_cdm_rcpp_slca_calc_deriv", (DL_FUNC) &_CDM_cdm_rcpp_slca_calc_deriv, 6},
    {"_CDM_cdm_rcpp_slca_calc_Xdes", (DL_FUNC) &_CDM_cdm_rcpp_slca_calc_Xdes, 2},
    {NULL, NULL, 0}
};

void R_init_CDM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
