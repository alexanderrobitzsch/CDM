//// File Name: cdm_rcpp_eval_likelihood.cpp
//// File Version: 0.34

// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// using namespace arma;



///********************************************************************
///**  cdm_rcpp_data_prep_long_format
// [[Rcpp::export]]
Rcpp::IntegerMatrix cdm_rcpp_data_prep_long_format( Rcpp::IntegerMatrix data )
{
    int N = data.nrow();
    int I = data.ncol();
    Rcpp::IntegerMatrix data_long(N*I, 3);
    int max_index=0;
    for (int nn=0; nn<N; nn++){
        for (int ii=0; ii<I; ii++){
            if ( ! R_IsNA( data(nn,ii) ) ){
                data_long( max_index, 0 ) = nn;
                data_long( max_index, 1 ) = ii;
                data_long( max_index, 2 ) = data(nn,ii);
                max_index ++;
            }
        }
    }
    data_long = data_long( Rcpp::Range(0, max_index-1), Rcpp::Range(0,2) );
    //---- OUTPUT:
    return data_long;
}
///********************************************************************


///********************************************************************
///**  cdm_rcpp_normalize_matrix_row
// [[Rcpp::export]]
Rcpp::NumericMatrix cdm_rcpp_normalize_matrix_row( Rcpp::NumericMatrix x )
{
    int N = x.nrow();
    int TP = x.ncol();
    Rcpp::NumericMatrix y(N, TP);
    double temp=0;
    for (int nn=0; nn<N; nn++){
        temp=0;
        for (int tt=0; tt<TP; tt++){
            temp += x(nn,tt);
        }
        for (int tt=0; tt<TP; tt++){
            y(nn,tt) = x(nn,tt) / temp;
        }
    }
    //---- OUTPUT:
    return y;
}
///********************************************************************

///********************************************************************
///**  cdm_rcpp_eval_likelihood_calc_wide_format
// [[Rcpp::export]]
Rcpp::NumericMatrix cdm_rcpp_eval_likelihood_calc_wide_format( Rcpp::IntegerMatrix data,
        Rcpp::NumericVector irfprob, Rcpp::IntegerVector dim_irfprob,
        Rcpp::NumericMatrix like0)
{
    int N = data.nrow();
    int I = data.ncol();
    int K = dim_irfprob[1];
    int TP = dim_irfprob[2];
    Rcpp::NumericMatrix like(N,TP);
    for (int tt=0; tt<TP; tt++){
        like(_,tt) = like0(_,tt);
    }
    // compute likelihood
    for (int nn=0; nn<N; nn++){
        for (int ii=0; ii<I; ii++){
            if ( ! R_IsNA( data(nn,ii) ) ){
                for (int tt=0; tt<TP; tt++){
                    like(nn,tt) = like(nn,tt) * irfprob[ ii + data(nn,ii)*I + tt*I*K ];
                }
            }
        }
    }
    //---- OUTPUT:
    return like;
}
///********************************************************************

///********************************************************************
///**  cdm_rcpp_eval_likelihood_calc_long_format
// [[Rcpp::export]]
Rcpp::NumericMatrix cdm_rcpp_eval_likelihood_calc_long_format( Rcpp::IntegerMatrix data_long,
        Rcpp::NumericVector irfprob, Rcpp::IntegerVector dim_irfprob,
        Rcpp::NumericMatrix like0)
{
    int I = dim_irfprob[0];
    int K = dim_irfprob[1];
    int TP = dim_irfprob[2];
    int ND = data_long.nrow();
    int N = like0.nrow();
    Rcpp::NumericMatrix like(N,TP);
    for (int tt=0; tt<TP; tt++){
        like(_,tt) = like0(_,tt);
    }
    // compute likelihood
    int nn=0;
    int ii=0;
    for (int hh=0; hh<ND; hh++){
        nn = data_long(hh,0);
        ii = data_long(hh,1);
        for (int tt=0; tt<TP; tt++){
            like(nn,tt) = like(nn,tt) * irfprob[ ii + data_long(hh,2)*I + tt*I*K ];
        }
    }
    //---- OUTPUT:
    return like;
}
///********************************************************************

///********************************************************************
///**  cdm_rcpp_eval_likelihood
// [[Rcpp::export]]
Rcpp::NumericMatrix cdm_rcpp_eval_likelihood( Rcpp::IntegerMatrix data,
        Rcpp::NumericVector irfprob, Rcpp::IntegerVector dim_irfprob,
        Rcpp::NumericMatrix prior, bool normalization, bool long_format, int N )
{
    int TP = dim_irfprob[2];
    Rcpp::NumericMatrix like(N, TP);
    for (int tt=0; tt<TP; tt++){
        like(_,tt) = prior(_,tt);
    }

    // computation likelihood wide format
    if ( ! long_format){
        like = cdm_rcpp_eval_likelihood_calc_wide_format( data, irfprob, dim_irfprob, like);
    } else {
        like = cdm_rcpp_eval_likelihood_calc_long_format( data, irfprob, dim_irfprob, like);
    }
    // normalization
    if (normalization){
        like = cdm_rcpp_normalize_matrix_row(like);
    }
    //---- OUTPUT:
    return like;
}
///********************************************************************

