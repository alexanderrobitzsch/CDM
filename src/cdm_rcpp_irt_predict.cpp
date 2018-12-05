//// File Name: cdm_rcpp_irt_predict.cpp
//// File Version: 3.12

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace arma;


///********************************************************************
///**  cdm_rcpp_irt_predict
// [[Rcpp::export]]
Rcpp::List cdm_rcpp_irt_predict( Rcpp::NumericMatrix resp, Rcpp::NumericVector irf1,
        int K, int TP)
{
    int N=resp.nrow();
    int I=resp.ncol();
    Rcpp::NumericVector probs_categ(N*K*TP*I);
    arma::cube pred(N,TP,I);
    arma::cube var1(N,TP,I);
    arma::cube resid1(N,TP,I);
    arma::cube sresid1(N,TP,I);
    double p1=0;
    double v1=0;

    for (int ii=0;ii<I;ii++){
        for (int nn=0;nn<N;nn++){
            if ( ! R_IsNA( resp(nn,ii) ) ){
                for (int tt=0;tt<TP;tt++){ // begin tt
                    v1 = 0;
                    for (int kk=0;kk<K;kk++){ // begin kk
                        p1 = irf1[ ii + kk*I + tt*I*K ];
                        probs_categ[ nn + kk*N + tt*N*K + ii*N*K*TP ] = p1;
                        v1 += kk * p1;
                    } // end kk
                    pred(nn,tt,ii) = v1;
                    v1 = 0;
                    for (int kk=0;kk<K;kk++){ // begin kk
                        p1 = irf1[ ii + kk*I + tt*I*K ];
                        v1 += std::pow( kk - pred(nn,tt,ii), 2.0 ) * p1;
                    } // end kk
                    var1(nn,tt,ii) = v1;
                    // residuals
                    resid1(nn,tt,ii) = ( resp( nn, ii ) - pred(nn,tt,ii) );
                    sresid1(nn,tt,ii) = resid1(nn,tt,ii) / std::sqrt( var1(nn,tt,ii) );
                } // end tt
            }
            if ( R_IsNA( resp(nn,ii) ) ){
                for (int tt=0;tt<TP;tt++){
                    pred(nn,tt,ii) = NA_REAL;
                    resid1(nn,tt,ii) = NA_REAL;
                    sresid1(nn,tt,ii) = NA_REAL;
                }
            }
            } // end nn
    } // end ii

    //*************************************************
    // OUTPUT
    return Rcpp::List::create(
                Rcpp::Named("pred") = pred,
                Rcpp::Named("probs_categ") = probs_categ,
                Rcpp::Named("var1") = var1,
                Rcpp::Named("resid1") = resid1,
                Rcpp::Named("sresid1") = sresid1
        );
}
///********************************************************************

