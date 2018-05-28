//// File Name: cdm_rcpp_est_calc_accuracy.cpp
//// File Version: 0.06

// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// using namespace arma;

///********************************************************************
///**  cdm_rcpp_est_calc_accuracy_version2_consistency_helper
// [[Rcpp::export]]
double cdm_rcpp_est_calc_accuracy_version2_consistency_helper( Rcpp::NumericMatrix post,
        Rcpp::IntegerVector est, int max_est_index, double N, Rcpp::NumericVector prob_theta,
        double eps )
{
    double val=0;
    double temp=0;
    // val <- 0
    //    for (ee in 1:TP){
    //        for (tt in 1:TP){
    //            h1 <- sum( post[, tt] * ( est == ee ) ) / N
    //            val <- val + h1 * h1 / ( prob_theta[tt] + eps )
    //        }
    //    }
    int TP = post.ncol();
    for (int ee=0; ee<max_est_index; ee++){
        for (int tt=0; tt<TP; tt++){
            temp=0;
            for (int nn=0; nn<N; nn++){
                if ( est[nn] == ee ){
                    temp += post(nn,tt);
                }
            }
            val += std::pow( temp / N, 2.0 ) / ( prob_theta[tt] + eps);
        }
    }
    //---- OUTPUT:
    return val;
}
///********************************************************************

