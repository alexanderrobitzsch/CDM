## File Name: RcppExports.R
## File Version: 8.003007
# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

cdm_rcpp_din_deterministic_devcrit <- function(DAT, DATRESP, LATRESP, GUESS, SLIP) {
    .Call('_CDM_cdm_rcpp_din_deterministic_devcrit', PACKAGE='CDM', DAT, DATRESP, LATRESP, GUESS, SLIP)
}

cdm_rcpp_din_jml_devcrit <- function(DAT, DATRESP, LATRESP, GUESS, SLIP) {
    .Call('_CDM_cdm_rcpp_din_jml_devcrit', PACKAGE='CDM', DAT, DATRESP, LATRESP, GUESS, SLIP)
}

cdm_rcpp_din_calc_prob <- function(latresp1, guess, slip, J, L) {
    .Call('_CDM_cdm_rcpp_din_calc_prob', PACKAGE='CDM', latresp1, guess, slip, J, L)
}

cdm_rcpp_din_calc_counts <- function(p_aj_xi, item_patt_freq, item_patt_split1, resp_patt_bool, J, L) {
    .Call('_CDM_cdm_rcpp_din_calc_counts', PACKAGE='CDM', p_aj_xi, item_patt_freq, item_patt_split1, resp_patt_bool, J, L)
}

cdm_rcpp_din_validate_aggregate_max <- function(IDI, itemindex, I) {
    .Call('_CDM_cdm_rcpp_din_validate_aggregate_max', PACKAGE='CDM', IDI, itemindex, I)
}

cdm_rcpp_din_validate_update_qmatrix <- function(qmatrix_poss, attr_patt, Ilj, Rlj, I, L, K, rule) {
    .Call('_CDM_cdm_rcpp_din_validate_update_qmatrix', PACKAGE='CDM', qmatrix_poss, attr_patt, Ilj, Rlj, I, L, K, rule)
}

cdm_rcpp_discrimination_index_attribute_patterns <- function(attr_patt) {
    .Call('_CDM_cdm_rcpp_discrimination_index_attribute_patterns', PACKAGE='CDM', attr_patt)
}

cdm_rcpp_discrimination_index_calc <- function(comp_matrix, probs, dim_probs, K) {
    .Call('_CDM_cdm_rcpp_discrimination_index_calc', PACKAGE='CDM', comp_matrix, probs, dim_probs, K)
}

cdm_rcpp_discrimination_index_test_level <- function(discrim_item_attribute) {
    .Call('_CDM_cdm_rcpp_discrimination_index_test_level', PACKAGE='CDM', discrim_item_attribute)
}

cdm_rcpp_discrimination_index_idi <- function(probs, dim_probs, K) {
    .Call('_CDM_cdm_rcpp_discrimination_index_idi', PACKAGE='CDM', probs, dim_probs, K)
}

cdm_rcpp_est_calc_accuracy_version2_consistency_helper <- function(post, est, max_est_index, N, prob_theta, eps) {
    .Call('_CDM_cdm_rcpp_est_calc_accuracy_version2_consistency_helper', PACKAGE='CDM', post, est, max_est_index, N, prob_theta, eps)
}

cdm_rcpp_data_prep_long_format <- function(data) {
    .Call('_CDM_cdm_rcpp_data_prep_long_format', PACKAGE='CDM', data)
}

cdm_rcpp_normalize_matrix_row <- function(x) {
    .Call('_CDM_cdm_rcpp_normalize_matrix_row', PACKAGE='CDM', x)
}

cdm_rcpp_eval_likelihood_calc_wide_format <- function(data, irfprob, dim_irfprob, like0) {
    .Call('_CDM_cdm_rcpp_eval_likelihood_calc_wide_format', PACKAGE='CDM', data, irfprob, dim_irfprob, like0)
}

cdm_rcpp_eval_likelihood_calc_long_format <- function(data_long, irfprob, dim_irfprob, like0) {
    .Call('_CDM_cdm_rcpp_eval_likelihood_calc_long_format', PACKAGE='CDM', data_long, irfprob, dim_irfprob, like0)
}

cdm_rcpp_eval_likelihood <- function(data, irfprob, dim_irfprob, prior, normalization, long_format, N) {
    .Call('_CDM_cdm_rcpp_eval_likelihood', PACKAGE='CDM', data, irfprob, dim_irfprob, prior, normalization, long_format, N)
}

cdm_rcpp_generalized_distance_method <- function(data, dataresp, idealresp, theta, a, b) {
    .Call('_CDM_cdm_rcpp_generalized_distance_method', PACKAGE='CDM', data, dataresp, idealresp, theta, a, b)
}

cdm_rcpp_ideal_resp_pattern <- function(qmatrix, skillspace) {
    .Call('_CDM_cdm_rcpp_ideal_resp_pattern', PACKAGE='CDM', qmatrix, skillspace)
}

cdm_rcpp_irt_classify_individuals <- function(like) {
    .Call('_CDM_cdm_rcpp_irt_classify_individuals', PACKAGE='CDM', like)
}

cdm_rcpp_irt_predict <- function(resp, irf1, K, TP) {
    .Call('_CDM_cdm_rcpp_irt_predict', PACKAGE='CDM', resp, irf1, K, TP)
}

cdm_rcpp_itemfit_sx2_calc_scoredistribution <- function(P1, Q1) {
    .Call('_CDM_cdm_rcpp_itemfit_sx2_calc_scoredistribution', PACKAGE='CDM', P1, Q1)
}

cdm_rcpp_kli_id <- function(pjk, sc) {
    .Call('_CDM_cdm_rcpp_kli_id', PACKAGE='CDM', pjk, sc)
}

cdm_rcpp_mcdina_probs_pcm_groups <- function(dat, dat_resp_bool, group, probs, CC, TP) {
    .Call('_CDM_cdm_rcpp_mcdina_probs_pcm_groups', PACKAGE='CDM', dat, dat_resp_bool, group, probs, CC, TP)
}

cdm_rcpp_mcdina_calccounts_pcm_groups <- function(dat, dat_resp_bool, group, fyiqk, pik, CC, weights) {
    .Call('_CDM_cdm_rcpp_mcdina_calccounts_pcm_groups', PACKAGE='CDM', dat, dat_resp_bool, group, fyiqk, pik, CC, weights)
}

cdm_rcpp_modelfit_cor2 <- function(posterior, data, data_resp_bool, probs1, probs0, ip, expiijj) {
    .Call('_CDM_cdm_rcpp_modelfit_cor2', PACKAGE='CDM', posterior, data, data_resp_bool, probs1, probs0, ip, expiijj)
}

cdm_rcpp_modelfit_cor_counts <- function(data, data_resp_bool) {
    .Call('_CDM_cdm_rcpp_modelfit_cor_counts', PACKAGE='CDM', data, data_resp_bool)
}

cdm_rcpp_sim_model_item_responses <- function(theta_index, irfprob, dim_irfprob) {
    .Call('_CDM_cdm_rcpp_sim_model_item_responses', PACKAGE='CDM', theta_index, irfprob, dim_irfprob)
}

cdm_rcpp_slca_calc_probs <- function(XdesM, dimXdes, Xlambda) {
    .Call('_CDM_cdm_rcpp_slca_calc_probs', PACKAGE='CDM', XdesM, dimXdes, Xlambda)
}

cdm_rcpp_slca_calc_deriv <- function(XdesM, dimXdes, Xlambda, probs, nik, Nik) {
    .Call('_CDM_cdm_rcpp_slca_calc_deriv', PACKAGE='CDM', XdesM, dimXdes, Xlambda, probs, nik, Nik)
}

cdm_rcpp_slca_calc_Xdes <- function(XDES, dimXdes) {
    .Call('_CDM_cdm_rcpp_slca_calc_Xdes', PACKAGE='CDM', XDES, dimXdes)
}

