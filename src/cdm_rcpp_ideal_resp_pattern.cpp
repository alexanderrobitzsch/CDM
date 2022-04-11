//// File Name: cdm_rcpp_ideal_resp_pattern.cpp
//// File Version: 0.221



// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;



///********************************************************************
///**  cdm_rcpp_ideal_resp_pattern
// [[Rcpp::export]]
Rcpp::NumericMatrix cdm_rcpp_ideal_resp_pattern( Rcpp::NumericMatrix qmatrix,
        Rcpp::NumericMatrix skillspace )
{
    int I = qmatrix.nrow();
    int K = skillspace.ncol();
    int L = skillspace.nrow();
    Rcpp::NumericMatrix idealresp(I,L);

    for (int ii=0; ii<I; ii++){
        for (int ll=0; ll<L; ll++){
            idealresp(ii,ll) = 1;
            for (int kk=0;kk<K;kk++){
                if ( ( qmatrix(ii,kk) == 1 ) && ( skillspace(ll,kk) == 0 ) ){
                    idealresp(ii, ll) = 0;
                }
            }
        }
    }
    //---- output
    return idealresp;
}
///********************************************************************

