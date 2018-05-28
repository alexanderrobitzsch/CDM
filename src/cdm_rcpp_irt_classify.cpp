//// File Name: cdm_rcpp_irt_classify.cpp
//// File Version: 0.07

// [[Rcpp::depends(RcppArmadillo)]]

//#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
//using namespace arma;

///********************************************************************
///**  cdm_rcpp_irt_classify_individuals
// [[Rcpp::export]]
Rcpp::List cdm_rcpp_irt_classify_individuals( Rcpp::NumericMatrix like )
{
    int N = like.nrow();
    int TP = like.ncol();
    Rcpp::IntegerVector class_index(N);
    Rcpp::NumericVector class_maxval(N);
    double val=0;
    int ind=0;
    for (int nn=0; nn<N; nn++){
        val=0;
        ind=0;
        for (int tt=0; tt<TP; tt++){
            if ( like(nn,tt) > val ){
                val = like(nn,tt);
                ind = tt;
            }
        }
        class_index[nn] = ind + 1;
        class_maxval[nn] = val;
    }
    //---- OUTPUT:
    return Rcpp::List::create(
                Rcpp::Named("class_index") = class_index,
                Rcpp::Named("class_maxval") = class_maxval
        );
}
///********************************************************************

