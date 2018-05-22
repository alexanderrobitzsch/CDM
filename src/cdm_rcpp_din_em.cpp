//// File Name: cdm_rcpp_din_em.cpp
//// File Version: 0.557



// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

///********************************************************************
///**  cdm_rcpp_din_calc_prob
// [[Rcpp::export]]
Rcpp::NumericVector cdm_rcpp_din_calc_prob( Rcpp::LogicalMatrix latresp1,
            Rcpp::NumericVector guess, Rcpp::NumericVector slip, int J, int L)
{
    Rcpp::NumericVector pj(J*L*2);
    //    slipM <- matrix( slip, nrow= nrow(latresp), ncol=ncol(latresp))
    //    guessM <- matrix( guess, nrow= nrow(latresp), ncol=ncol(latresp))
    //    pj <- (1 - slipM )*latresp + guessM * ( 1 - latresp )
    Rcpp::NumericVector slip1(J);
    Rcpp::NumericVector guess1(J);
    for (int jj=0; jj<J; jj++){
        slip1[jj] = 1.0 - slip[jj];
        guess1[jj] = 1.0 - guess[jj];
    }
    int NC = 2;
    int ind_temp_cc0=0;
    int ind_temp_cc1=0;
    for (int jj=0; jj<J; jj++){
        for (int ll=0; ll<L; ll++){
            ind_temp_cc0 = jj + ll*J*NC;
            ind_temp_cc1 = ind_temp_cc0 + 1*J;
            if (latresp1(jj,ll)){
                pj[ ind_temp_cc0 ] = slip[jj];  // P(X=0) = slip
                pj[ ind_temp_cc1 ] = slip1[jj];   // P(X=1) = 1 - slip
            } else {
                pj[ ind_temp_cc0 ] = guess1[jj];    // P(X=0) = 1 - guess
                pj[ ind_temp_cc1 ] = guess[jj];   // P(X=1) = guess
            }
        }
    }
    //---- OUTPUT:
    return pj;
}
///********************************************************************

///********************************************************************
///**  cdm_rcpp_din_calc_counts
// [[Rcpp::export]]
Rcpp::NumericMatrix cdm_rcpp_din_calc_counts( Rcpp::NumericMatrix p_aj_xi, Rcpp::NumericVector item_patt_freq,
            Rcpp::LogicalMatrix item_patt_split1, Rcpp::LogicalMatrix resp_patt_bool,
            int J, int L)
{
    Rcpp::NumericMatrix Rlj(J,L);
    // Rcpp::NumericMatrix Ilj(J,L);
    // R.lj <- crossprod(ipr, p.aj.xi    )
    int N = item_patt_freq.size();
    for (int jj=0; jj<J; jj++){
        for (int nn=0; nn<N; nn++){
            if (resp_patt_bool(nn,jj) & item_patt_split1(nn,jj) ){
                for (int ll=0; ll<L; ll++){
                    Rlj(jj,ll) += p_aj_xi(nn,ll)*item_patt_freq[nn];
                }
            }
        }
    }
    //---- OUTPUT:
    return Rlj;
}
///********************************************************************

// return Rcpp::List::create(  Rcpp::Named("dist") = dist,  Rcpp::Named("est_skill") = est_skill  );
