//// File Name: cdm_rcpp_sim_model.cpp
//// File Version: 0.13

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
//#include <Rcpp.h>

using namespace Rcpp;
using namespace arma;

///********************************************************************
///**  cdm_rcpp_sim_model_item_responses
// [[Rcpp::export]]
Rcpp::NumericMatrix cdm_rcpp_sim_model_item_responses(
        Rcpp::IntegerVector theta_index, Rcpp::NumericVector irfprob,
        Rcpp::IntegerVector dim_irfprob )
{
    int I = dim_irfprob[0];
    int K = dim_irfprob[1];
    int N = theta_index.size();
    Rcpp::NumericMatrix dat(N, I);
    Rcpp::NumericVector ind_K(K);
    for (int kk=0; kk<K; kk++){
        ind_K[kk] = kk;
    }
    Rcpp::NumericVector prob_temp(K);
    for (int nn=0; nn<N; nn++){
        for (int ii=0; ii<I; ii++){
            for (int kk=0; kk<K; kk++){
                prob_temp[kk] = irfprob[ii+kk*I+theta_index[nn]*I*K];
            }
            dat(nn,ii) = Rcpp::as<double>( Rcpp::sample(ind_K, 1, TRUE, prob_temp) );
        }
    }
    //---- OUTPUT:
    return dat;
}
///********************************************************************

