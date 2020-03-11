//// File Name: RcppExports.cpp
//// File Version: 7.006003
// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>

using namespace Rcpp; using namespace arma;

// cdm_rcpp_din_deterministic_devcrit
Rcpp::List cdm_rcpp_din_deterministic_devcrit(Rcpp::NumericMatrix DAT, Rcpp::NumericMatrix DATRESP, Rcpp::NumericMatrix LATRESP, Rcpp::NumericVector GUESS, Rcpp::NumericVector SLIP);
RcppExport SEXP _CDM_cdm_rcpp_din_deterministic_devcrit(SEXP DATSEXP, SEXP DATRESPSEXP, SEXP LATRESPSEXP, SEXP GUESSSEXP, SEXP SLIPSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type DAT(DATSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type DATRESP(DATRESPSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type LATRESP(LATRESPSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type GUESS(GUESSSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type SLIP(SLIPSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_din_deterministic_devcrit(DAT, DATRESP, LATRESP, GUESS, SLIP));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_din_jml_devcrit
Rcpp::List cdm_rcpp_din_jml_devcrit(Rcpp::NumericMatrix DAT, Rcpp::NumericMatrix DATRESP, Rcpp::NumericMatrix LATRESP, Rcpp::NumericVector GUESS, Rcpp::NumericVector SLIP);
RcppExport SEXP _CDM_cdm_rcpp_din_jml_devcrit(SEXP DATSEXP, SEXP DATRESPSEXP, SEXP LATRESPSEXP, SEXP GUESSSEXP, SEXP SLIPSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type DAT(DATSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type DATRESP(DATRESPSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type LATRESP(LATRESPSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type GUESS(GUESSSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type SLIP(SLIPSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_din_jml_devcrit(DAT, DATRESP, LATRESP, GUESS, SLIP));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_din_calc_prob
Rcpp::NumericVector cdm_rcpp_din_calc_prob(Rcpp::LogicalMatrix latresp1, Rcpp::NumericVector guess, Rcpp::NumericVector slip, int J, int L);
RcppExport SEXP _CDM_cdm_rcpp_din_calc_prob(SEXP latresp1SEXP, SEXP guessSEXP, SEXP slipSEXP, SEXP JSEXP, SEXP LSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type latresp1(latresp1SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type guess(guessSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type slip(slipSEXP);
    Rcpp::traits::input_parameter< int >::type J(JSEXP);
    Rcpp::traits::input_parameter< int >::type L(LSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_din_calc_prob(latresp1, guess, slip, J, L));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_din_calc_counts
Rcpp::NumericMatrix cdm_rcpp_din_calc_counts(Rcpp::NumericMatrix p_aj_xi, Rcpp::NumericVector item_patt_freq, Rcpp::LogicalMatrix item_patt_split1, Rcpp::LogicalMatrix resp_patt_bool, int J, int L);
RcppExport SEXP _CDM_cdm_rcpp_din_calc_counts(SEXP p_aj_xiSEXP, SEXP item_patt_freqSEXP, SEXP item_patt_split1SEXP, SEXP resp_patt_boolSEXP, SEXP JSEXP, SEXP LSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type p_aj_xi(p_aj_xiSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type item_patt_freq(item_patt_freqSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type item_patt_split1(item_patt_split1SEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type resp_patt_bool(resp_patt_boolSEXP);
    Rcpp::traits::input_parameter< int >::type J(JSEXP);
    Rcpp::traits::input_parameter< int >::type L(LSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_din_calc_counts(p_aj_xi, item_patt_freq, item_patt_split1, resp_patt_bool, J, L));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_din_validate_aggregate_max
Rcpp::NumericMatrix cdm_rcpp_din_validate_aggregate_max(Rcpp::NumericVector IDI, Rcpp::IntegerVector itemindex, int I);
RcppExport SEXP _CDM_cdm_rcpp_din_validate_aggregate_max(SEXP IDISEXP, SEXP itemindexSEXP, SEXP ISEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type IDI(IDISEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type itemindex(itemindexSEXP);
    Rcpp::traits::input_parameter< int >::type I(ISEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_din_validate_aggregate_max(IDI, itemindex, I));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_din_validate_update_qmatrix
Rcpp::List cdm_rcpp_din_validate_update_qmatrix(Rcpp::IntegerMatrix qmatrix_poss, Rcpp::IntegerMatrix attr_patt, Rcpp::NumericMatrix Ilj, Rcpp::NumericMatrix Rlj, int I, int L, int K, Rcpp::CharacterVector rule);
RcppExport SEXP _CDM_cdm_rcpp_din_validate_update_qmatrix(SEXP qmatrix_possSEXP, SEXP attr_pattSEXP, SEXP IljSEXP, SEXP RljSEXP, SEXP ISEXP, SEXP LSEXP, SEXP KSEXP, SEXP ruleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type qmatrix_poss(qmatrix_possSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type attr_patt(attr_pattSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Ilj(IljSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Rlj(RljSEXP);
    Rcpp::traits::input_parameter< int >::type I(ISEXP);
    Rcpp::traits::input_parameter< int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type rule(ruleSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_din_validate_update_qmatrix(qmatrix_poss, attr_patt, Ilj, Rlj, I, L, K, rule));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_discrimination_index_attribute_patterns
Rcpp::IntegerMatrix cdm_rcpp_discrimination_index_attribute_patterns(Rcpp::NumericMatrix attr_patt);
RcppExport SEXP _CDM_cdm_rcpp_discrimination_index_attribute_patterns(SEXP attr_pattSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type attr_patt(attr_pattSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_discrimination_index_attribute_patterns(attr_patt));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_discrimination_index_calc
Rcpp::NumericMatrix cdm_rcpp_discrimination_index_calc(Rcpp::IntegerMatrix comp_matrix, Rcpp::NumericVector probs, Rcpp::NumericVector dim_probs, int K);
RcppExport SEXP _CDM_cdm_rcpp_discrimination_index_calc(SEXP comp_matrixSEXP, SEXP probsSEXP, SEXP dim_probsSEXP, SEXP KSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type comp_matrix(comp_matrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dim_probs(dim_probsSEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_discrimination_index_calc(comp_matrix, probs, dim_probs, K));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_discrimination_index_test_level
double cdm_rcpp_discrimination_index_test_level(Rcpp::NumericMatrix discrim_item_attribute);
RcppExport SEXP _CDM_cdm_rcpp_discrimination_index_test_level(SEXP discrim_item_attributeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type discrim_item_attribute(discrim_item_attributeSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_discrimination_index_test_level(discrim_item_attribute));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_discrimination_index_idi
Rcpp::NumericVector cdm_rcpp_discrimination_index_idi(Rcpp::NumericVector probs, Rcpp::NumericVector dim_probs, int K);
RcppExport SEXP _CDM_cdm_rcpp_discrimination_index_idi(SEXP probsSEXP, SEXP dim_probsSEXP, SEXP KSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dim_probs(dim_probsSEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_discrimination_index_idi(probs, dim_probs, K));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_est_calc_accuracy_version2_consistency_helper
double cdm_rcpp_est_calc_accuracy_version2_consistency_helper(Rcpp::NumericMatrix post, Rcpp::IntegerVector est, int max_est_index, double N, Rcpp::NumericVector prob_theta, double eps);
RcppExport SEXP _CDM_cdm_rcpp_est_calc_accuracy_version2_consistency_helper(SEXP postSEXP, SEXP estSEXP, SEXP max_est_indexSEXP, SEXP NSEXP, SEXP prob_thetaSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type post(postSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type est(estSEXP);
    Rcpp::traits::input_parameter< int >::type max_est_index(max_est_indexSEXP);
    Rcpp::traits::input_parameter< double >::type N(NSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type prob_theta(prob_thetaSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_est_calc_accuracy_version2_consistency_helper(post, est, max_est_index, N, prob_theta, eps));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_data_prep_long_format
Rcpp::IntegerMatrix cdm_rcpp_data_prep_long_format(Rcpp::IntegerMatrix data);
RcppExport SEXP _CDM_cdm_rcpp_data_prep_long_format(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_data_prep_long_format(data));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_normalize_matrix_row
Rcpp::NumericMatrix cdm_rcpp_normalize_matrix_row(Rcpp::NumericMatrix x);
RcppExport SEXP _CDM_cdm_rcpp_normalize_matrix_row(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_normalize_matrix_row(x));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_eval_likelihood_calc_wide_format
Rcpp::NumericMatrix cdm_rcpp_eval_likelihood_calc_wide_format(Rcpp::IntegerMatrix data, Rcpp::NumericVector irfprob, Rcpp::IntegerVector dim_irfprob, Rcpp::NumericMatrix like0);
RcppExport SEXP _CDM_cdm_rcpp_eval_likelihood_calc_wide_format(SEXP dataSEXP, SEXP irfprobSEXP, SEXP dim_irfprobSEXP, SEXP like0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type irfprob(irfprobSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim_irfprob(dim_irfprobSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type like0(like0SEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_eval_likelihood_calc_wide_format(data, irfprob, dim_irfprob, like0));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_eval_likelihood_calc_long_format
Rcpp::NumericMatrix cdm_rcpp_eval_likelihood_calc_long_format(Rcpp::IntegerMatrix data_long, Rcpp::NumericVector irfprob, Rcpp::IntegerVector dim_irfprob, Rcpp::NumericMatrix like0);
RcppExport SEXP _CDM_cdm_rcpp_eval_likelihood_calc_long_format(SEXP data_longSEXP, SEXP irfprobSEXP, SEXP dim_irfprobSEXP, SEXP like0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type data_long(data_longSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type irfprob(irfprobSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim_irfprob(dim_irfprobSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type like0(like0SEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_eval_likelihood_calc_long_format(data_long, irfprob, dim_irfprob, like0));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_eval_likelihood
Rcpp::NumericMatrix cdm_rcpp_eval_likelihood(Rcpp::IntegerMatrix data, Rcpp::NumericVector irfprob, Rcpp::IntegerVector dim_irfprob, Rcpp::NumericMatrix prior, bool normalization, bool long_format, int N);
RcppExport SEXP _CDM_cdm_rcpp_eval_likelihood(SEXP dataSEXP, SEXP irfprobSEXP, SEXP dim_irfprobSEXP, SEXP priorSEXP, SEXP normalizationSEXP, SEXP long_formatSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type irfprob(irfprobSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim_irfprob(dim_irfprobSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type prior(priorSEXP);
    Rcpp::traits::input_parameter< bool >::type normalization(normalizationSEXP);
    Rcpp::traits::input_parameter< bool >::type long_format(long_formatSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_eval_likelihood(data, irfprob, dim_irfprob, prior, normalization, long_format, N));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_generalized_distance_method
Rcpp::List cdm_rcpp_generalized_distance_method(Rcpp::NumericMatrix data, Rcpp::NumericMatrix dataresp, Rcpp::NumericMatrix idealresp, Rcpp::NumericVector theta, Rcpp::NumericVector a, Rcpp::NumericVector b);
RcppExport SEXP _CDM_cdm_rcpp_generalized_distance_method(SEXP dataSEXP, SEXP datarespSEXP, SEXP idealrespSEXP, SEXP thetaSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type dataresp(datarespSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type idealresp(idealrespSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_generalized_distance_method(data, dataresp, idealresp, theta, a, b));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_ideal_resp_pattern
Rcpp::NumericMatrix cdm_rcpp_ideal_resp_pattern(Rcpp::NumericMatrix qmatrix, Rcpp::NumericMatrix skillspace);
RcppExport SEXP _CDM_cdm_rcpp_ideal_resp_pattern(SEXP qmatrixSEXP, SEXP skillspaceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type qmatrix(qmatrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type skillspace(skillspaceSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_ideal_resp_pattern(qmatrix, skillspace));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_irt_classify_individuals
Rcpp::List cdm_rcpp_irt_classify_individuals(Rcpp::NumericMatrix like);
RcppExport SEXP _CDM_cdm_rcpp_irt_classify_individuals(SEXP likeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type like(likeSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_irt_classify_individuals(like));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_irt_predict
Rcpp::List cdm_rcpp_irt_predict(Rcpp::NumericMatrix resp, Rcpp::NumericVector irf1, int K, int TP);
RcppExport SEXP _CDM_cdm_rcpp_irt_predict(SEXP respSEXP, SEXP irf1SEXP, SEXP KSEXP, SEXP TPSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type resp(respSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type irf1(irf1SEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    Rcpp::traits::input_parameter< int >::type TP(TPSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_irt_predict(resp, irf1, K, TP));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_itemfit_sx2_calc_scoredistribution
Rcpp::NumericMatrix cdm_rcpp_itemfit_sx2_calc_scoredistribution(Rcpp::NumericMatrix P1, Rcpp::NumericMatrix Q1);
RcppExport SEXP _CDM_cdm_rcpp_itemfit_sx2_calc_scoredistribution(SEXP P1SEXP, SEXP Q1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type P1(P1SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Q1(Q1SEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_itemfit_sx2_calc_scoredistribution(P1, Q1));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_kli_id
Rcpp::List cdm_rcpp_kli_id(Rcpp::NumericMatrix pjk, Rcpp::NumericMatrix sc);
RcppExport SEXP _CDM_cdm_rcpp_kli_id(SEXP pjkSEXP, SEXP scSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type pjk(pjkSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type sc(scSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_kli_id(pjk, sc));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_mcdina_probs_pcm_groups
Rcpp::List cdm_rcpp_mcdina_probs_pcm_groups(Rcpp::NumericMatrix dat, Rcpp::LogicalMatrix dat_resp_bool, Rcpp::NumericVector group, Rcpp::NumericMatrix probs, int CC, int TP);
RcppExport SEXP _CDM_cdm_rcpp_mcdina_probs_pcm_groups(SEXP datSEXP, SEXP dat_resp_boolSEXP, SEXP groupSEXP, SEXP probsSEXP, SEXP CCSEXP, SEXP TPSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type dat(datSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type dat_resp_bool(dat_resp_boolSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type group(groupSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< int >::type CC(CCSEXP);
    Rcpp::traits::input_parameter< int >::type TP(TPSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_mcdina_probs_pcm_groups(dat, dat_resp_bool, group, probs, CC, TP));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_mcdina_calccounts_pcm_groups
Rcpp::List cdm_rcpp_mcdina_calccounts_pcm_groups(Rcpp::NumericMatrix dat, Rcpp::LogicalMatrix dat_resp_bool, Rcpp::NumericVector group, Rcpp::NumericMatrix fyiqk, Rcpp::NumericMatrix pik, int CC, Rcpp::NumericVector weights);
RcppExport SEXP _CDM_cdm_rcpp_mcdina_calccounts_pcm_groups(SEXP datSEXP, SEXP dat_resp_boolSEXP, SEXP groupSEXP, SEXP fyiqkSEXP, SEXP pikSEXP, SEXP CCSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type dat(datSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type dat_resp_bool(dat_resp_boolSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type group(groupSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fyiqk(fyiqkSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type pik(pikSEXP);
    Rcpp::traits::input_parameter< int >::type CC(CCSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_mcdina_calccounts_pcm_groups(dat, dat_resp_bool, group, fyiqk, pik, CC, weights));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_modelfit_cor2
Rcpp::List cdm_rcpp_modelfit_cor2(Rcpp::NumericMatrix posterior, Rcpp::NumericMatrix data, Rcpp::LogicalMatrix data_resp_bool, Rcpp::NumericMatrix probs1, Rcpp::NumericMatrix probs0, Rcpp::NumericMatrix ip, Rcpp::NumericMatrix expiijj);
RcppExport SEXP _CDM_cdm_rcpp_modelfit_cor2(SEXP posteriorSEXP, SEXP dataSEXP, SEXP data_resp_boolSEXP, SEXP probs1SEXP, SEXP probs0SEXP, SEXP ipSEXP, SEXP expiijjSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type posterior(posteriorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type data_resp_bool(data_resp_boolSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type probs1(probs1SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type probs0(probs0SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type ip(ipSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type expiijj(expiijjSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_modelfit_cor2(posterior, data, data_resp_bool, probs1, probs0, ip, expiijj));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_modelfit_cor_counts
Rcpp::List cdm_rcpp_modelfit_cor_counts(Rcpp::IntegerMatrix data, Rcpp::LogicalMatrix data_resp_bool);
RcppExport SEXP _CDM_cdm_rcpp_modelfit_cor_counts(SEXP dataSEXP, SEXP data_resp_boolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type data_resp_bool(data_resp_boolSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_modelfit_cor_counts(data, data_resp_bool));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_sim_model_item_responses
Rcpp::NumericMatrix cdm_rcpp_sim_model_item_responses(Rcpp::IntegerVector theta_index, Rcpp::NumericVector irfprob, Rcpp::IntegerVector dim_irfprob);
RcppExport SEXP _CDM_cdm_rcpp_sim_model_item_responses(SEXP theta_indexSEXP, SEXP irfprobSEXP, SEXP dim_irfprobSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type theta_index(theta_indexSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type irfprob(irfprobSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dim_irfprob(dim_irfprobSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_sim_model_item_responses(theta_index, irfprob, dim_irfprob));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_slca_calc_probs
Rcpp::NumericVector cdm_rcpp_slca_calc_probs(Rcpp::NumericMatrix XdesM, Rcpp::NumericVector dimXdes, Rcpp::NumericVector Xlambda);
RcppExport SEXP _CDM_cdm_rcpp_slca_calc_probs(SEXP XdesMSEXP, SEXP dimXdesSEXP, SEXP XlambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type XdesM(XdesMSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dimXdes(dimXdesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Xlambda(XlambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_slca_calc_probs(XdesM, dimXdes, Xlambda));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_slca_calc_deriv
Rcpp::List cdm_rcpp_slca_calc_deriv(Rcpp::NumericMatrix XdesM, Rcpp::NumericVector dimXdes, Rcpp::NumericVector Xlambda, Rcpp::NumericVector probs, Rcpp::NumericVector nik, Rcpp::NumericVector Nik);
RcppExport SEXP _CDM_cdm_rcpp_slca_calc_deriv(SEXP XdesMSEXP, SEXP dimXdesSEXP, SEXP XlambdaSEXP, SEXP probsSEXP, SEXP nikSEXP, SEXP NikSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type XdesM(XdesMSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dimXdes(dimXdesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Xlambda(XlambdaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type nik(nikSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Nik(NikSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_slca_calc_deriv(XdesM, dimXdes, Xlambda, probs, nik, Nik));
    return rcpp_result_gen;
END_RCPP
}
// cdm_rcpp_slca_calc_Xdes
Rcpp::List cdm_rcpp_slca_calc_Xdes(Rcpp::NumericVector XDES, Rcpp::NumericVector dimXdes);
RcppExport SEXP _CDM_cdm_rcpp_slca_calc_Xdes(SEXP XDESSEXP, SEXP dimXdesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type XDES(XDESSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dimXdes(dimXdesSEXP);
    rcpp_result_gen = Rcpp::wrap(cdm_rcpp_slca_calc_Xdes(XDES, dimXdes));
    return rcpp_result_gen;
END_RCPP
}

