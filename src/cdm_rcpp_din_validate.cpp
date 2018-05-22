//// File Name: cdm_rcpp_din_validate.cpp
//// File Version: 0.52


#include <Rcpp.h>
// #include <RcppArmadillo.h>

using namespace Rcpp;
// using namespace arma;

///********************************************************************
///** cdm_rcpp_din_validate_aggregate_max
// [[Rcpp::export]]
Rcpp::NumericMatrix cdm_rcpp_din_validate_aggregate_max( Rcpp::NumericVector IDI,
    Rcpp::IntegerVector itemindex, int I)
{
    // a1 <- stats::aggregate( coef.modified$IDI, list( coef.modified$itemindex ), max )
    Rcpp::NumericMatrix idi_max(I,2);
    int NR = IDI.size();
    int hh=0;
    int hh1=0;
    int index_nn=0;
    double temp_max=0;
    double temp_val=0;
    for (int nn=0; nn<NR; nn++){
        index_nn = itemindex[nn];
        temp_val = IDI[nn];
        if (index_nn > hh){
            hh = index_nn;
            hh1 = hh-1;
            idi_max(hh1,0) = index_nn;
            temp_max = temp_val;
            idi_max(hh1,1) = temp_val;

        } else {
            if (temp_val > temp_max){
                idi_max(hh1,1) = temp_val;
                temp_max = temp_val;
            }
        }
    }
    //--- output
    return idi_max;
}
///********************************************************************

///********************************************************************
///** cdm_rcpp_din_validate_update_qmatrix_one_pattern
// [[Rcpp__NOexport]]
Rcpp::List cdm_rcpp_din_validate_update_qmatrix_one_pattern(
        Rcpp::IntegerVector qvec, Rcpp::IntegerMatrix attr_patt,
        Rcpp::NumericMatrix Ilj, Rcpp::NumericMatrix Rlj, int I, int L, int K,
        Rcpp::CharacterVector rule )
{
    Rcpp::NumericVector Ij0(I);
    Rcpp::NumericVector Ij1(I);
    Rcpp::NumericVector Rj0(I);
    Rcpp::NumericVector Rj1(I);
    Rcpp::NumericVector guess(I);
    Rcpp::NumericVector slip(I);

    int ness_ii=0;
    int latresp=0;

    for (int ii=0; ii<I; ii++){
        ness_ii=0;
        if ( rule[ii] == "DINA" ){
            for (int kk=0; kk<K; kk++){
                ness_ii += qvec[kk];
            }
        } else {
            ness_ii = 1;
        }
        for (int ll=0; ll<L; ll++){
            latresp=0;
            for (int kk=0; kk<K; kk++){
                latresp += qvec[kk]*attr_patt(ll,kk);
            }
            if (latresp < ness_ii){
                Ij0[ii] += Ilj(ii,ll);
                Rj0[ii] += Rlj(ii,ll);
            } else {
                Ij1[ii] += Ilj(ii,ll);
                Rj1[ii] += Rlj(ii,ll);
            }
        }
        guess[ii] = Rj0[ii] / Ij0[ii];
        slip[ii] = (Ij1[ii] - Rj1[ii] ) / Ij1[ii];
    }

    //    guess <- R.j0 / I.j0
    //    slip <- ( I.j1 - R.j1 ) / I.j1

    //--- output
    return Rcpp::List::create(
                Rcpp::Named("guess") = guess,
                Rcpp::Named("slip") = slip
            );
}
///********************************************************************

///********************************************************************
///** cdm_rcpp_din_validate_update_qmatrix
// [[Rcpp::export]]
Rcpp::List cdm_rcpp_din_validate_update_qmatrix(
        Rcpp::IntegerMatrix qmatrix_poss, Rcpp::IntegerMatrix attr_patt, Rcpp::NumericMatrix Ilj,
        Rcpp::NumericMatrix Rlj, int I, int L, int K, Rcpp::CharacterVector rule)
{
    int QQM=qmatrix_poss.nrow();
    Rcpp::NumericVector guess_M(QQM*I);
    Rcpp::NumericVector slip_M(QQM*I);
    Rcpp::IntegerVector qvec(K);
    Rcpp::List res;
    for (int qq=0; qq<QQM; qq++){
        qvec = qmatrix_poss(qq,_);
        res = cdm_rcpp_din_validate_update_qmatrix_one_pattern( qvec, attr_patt, Ilj, Rlj,
                    I, L, K, rule );
        // ind_qqm <- I*( qqm - 1 ) + 1:I
        guess_M[ Rcpp::Range( I*qq, I*qq+ I-1 ) ] = Rcpp::as< Rcpp::NumericVector >(res["guess"]);
        slip_M[ Rcpp::Range( I*qq, I*qq+ I-1 ) ] = Rcpp::as< Rcpp::NumericVector >(res["slip"]);
    }
    //--- output
    return Rcpp::List::create(
                Rcpp::Named("guess_M") = guess_M,
                Rcpp::Named("slip_M") = slip_M
            );
}
///********************************************************************
