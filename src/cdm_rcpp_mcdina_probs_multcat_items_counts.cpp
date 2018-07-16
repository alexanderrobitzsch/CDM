//// File Name: cdm_rcpp_mcdina_probs_multcat_items_counts.cpp
//// File Version: 3.14

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///**  cdm_rcpp_mcdina_probs_pcm_groups
// [[Rcpp::export]]
Rcpp::List cdm_rcpp_mcdina_probs_pcm_groups( Rcpp::NumericMatrix dat,
        Rcpp::LogicalMatrix dat_resp_bool, Rcpp::NumericVector group,
        Rcpp::NumericMatrix probs, int CC, int TP )
{
    int I = dat.ncol();  // number of items
    int N = dat.nrow(); // number of subjects
    // create likelihood object
    Rcpp::NumericMatrix fyiqk(N,TP);
    fyiqk.fill(1);

    for (int nn=0;nn<N;nn++){
        for (int ii=0;ii<I;ii++){
            if (dat_resp_bool(nn,ii) ){
                for (int tt=0;tt<TP;tt++){
                    // probs ( ii, cc, tt, gg ) =
                    // probs_C(ii,  cc + tt*CC + gg * CC*TP )
                    fyiqk(nn,tt) = fyiqk(nn,tt) * probs( ii, dat(nn,ii) + tt * CC + group[nn] * CC * TP );
                }  // end tt
            }  // end if dat_resp = 1
        } // end ii
    }   // end nn

    ///////////////////////////////////////
    /// OUTPUT
    return Rcpp::List::create(
                Rcpp::Named("fyiqk") = fyiqk
        );
}
///********************************************************************


///********************************************************************
///**  cdm_rcpp_mcdina_calccounts_pcm_groups
// [[Rcpp::export]]
Rcpp::List cdm_rcpp_mcdina_calccounts_pcm_groups( Rcpp::NumericMatrix dat,
        Rcpp::LogicalMatrix dat_resp_bool, Rcpp::NumericVector group, Rcpp::NumericMatrix fyiqk,
        Rcpp::NumericMatrix pik, int CC, Rcpp::NumericVector weights )
{
    int TP = fyiqk.ncol();
    int G = pik.ncol();
    int N = dat.nrow();
    int I = dat.ncol();
    Rcpp::NumericMatrix fqkyi(N,TP);
    Rcpp::NumericMatrix pik1(TP,G);
    double t1 = 0;
    double eps=1e-30;

    //*** calculate posterior
    for (int nn=0;nn<N;nn++){
        t1 = eps;
        for (int tt=0;tt<TP;tt++){
            fqkyi(nn,tt) = fyiqk(nn,tt) * pik( tt, group[nn] );
            t1 += fqkyi(nn,tt);
        }
        fqkyi.row(nn) = fqkyi.row(nn) / t1;
    }

    //*** calculate expected counts
    // probs ( ii, cc, tt, gg ) =
    // probs_C(ii,  cc + tt*CC + gg * CC*TP )
    Rcpp::NumericMatrix nik(I,CC*TP*G);
    for (int ii=0;ii<I;ii++){
        for (int nn=0;nn<N;nn++){
            if (dat_resp_bool(nn,ii)){
                for(int tt=0;tt<TP;tt++){
                    nik(ii,dat(nn,ii)+tt*CC+group[nn]*CC*TP ) +=  fqkyi(nn,tt) * weights[nn];
                } // end tt
            }  // end if dat_resp == 1
        } // end nn
    }  // end ii

    //*** calculate loglikelihood and updated group distributions
    double LL=0;
    for (int nn=0;nn<N;++nn){
        double total = 0;
        for (int tt=0;tt<TP;++tt){
            total += fyiqk(nn,tt) * pik(tt,group[nn]);
            pik1(tt,group[nn]) += fqkyi(nn,tt)*weights[nn];
        } // end tt
        LL += std::log( total+eps ) * weights[nn];
    }  // end nn

    ///////////////////////////////////////
    /// OUTPUT
    return Rcpp::List::create(
                Rcpp::Named("LL") = LL,
                Rcpp::Named("fqkyi") = fqkyi,
                Rcpp::Named("nik") = nik,
                Rcpp::Named("count_pik") = pik1
        );
}
///********************************************************************



