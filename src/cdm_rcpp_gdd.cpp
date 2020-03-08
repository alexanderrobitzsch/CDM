//// File Name: cdm_rcpp_gdd.cpp
//// File Version: 3.14



// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///**  cdm_rcpp_generalized_distance_method
// [[Rcpp::export]]
Rcpp::List cdm_rcpp_generalized_distance_method( Rcpp::NumericMatrix data,
        Rcpp::NumericMatrix dataresp, Rcpp::NumericMatrix idealresp,
        Rcpp::NumericVector theta, Rcpp::NumericVector a,
        Rcpp::NumericVector b )
{
    int I = idealresp.nrow();
    int L = idealresp.ncol();
    int N = data.nrow();
    double ind = 0;
    double dmin = 100 * I;
    Rcpp::NumericMatrix dist(N,L);
    Rcpp::NumericVector est_skill(N);

    for (int nn=0;nn<N;nn++){ // begin person nn
        dmin=100*I;
        ind=0;
        for (int ll=0; ll<L; ll++){  // begin skill class ll
            for (int ii=0; ii<I; ii++){   //  begin item ii
                if ( dataresp(nn,ii)==1 ){
                    //*** data=1 and idealresp=0
                    if ( ( data(nn,ii) == 1 ) & ( idealresp(ii,ll) == 0 ) ){
                        //     1 - P0  = P1
                        dist(nn,ll) += std::pow( 1 + std::exp( - ( b[ii] + a[ii] * theta[nn] ) ), -1 );
                    }
                    //*** data=0 and idealresp=1
                    if ( ( data(nn,ii) == 0 ) & ( idealresp(ii,ll) == 1 ) ){
                        //     1 - P1  = P0
                        dist(nn,ll) += std::pow( 1 + std::exp( ( b[ii] + a[ii] * theta[nn] ) ), -1 );
                    }
                }  // end dataresp = 1
            }   // end ii
            if ( dmin > dist(nn,ll) ){
                dmin = dist(nn,ll);
                ind = ll +1;
            }
        }   // end ll
        est_skill[nn] = ind;
    }  // end nn

    /////////////////////////////////////////////
    // OUTPUT:
    return Rcpp::List::create(
                Rcpp::Named("dist") = dist,
                Rcpp::Named("est_skill") = est_skill
            );
}
///********************************************************************


