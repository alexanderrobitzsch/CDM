//// File Name: cdm_rcpp_modelfit_cor.cpp
//// File Version: 3.22

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;



///********************************************************************
///**  cdm_rcpp_modelfit_cor2
// [[Rcpp::export]]
Rcpp::List cdm_rcpp_modelfit_cor2( Rcpp::NumericMatrix posterior,
        Rcpp::NumericMatrix data, Rcpp::LogicalMatrix data_resp_bool,
        Rcpp::NumericMatrix probs1, Rcpp::NumericMatrix probs0, Rcpp::NumericMatrix ip,
        Rcpp::NumericMatrix expiijj )
{
    int NIP = ip.nrow();
    int N = posterior.nrow();
    int TP = posterior.ncol();
    Rcpp::NumericMatrix itempair_stat(NIP,4);
    Rcpp::NumericVector psiijj(TP);
    Rcpp::NumericVector Q3(NIP);
    double t1 = 0;
    double mii = 0;
    double mjj = 0;
    double vii = 0;
    double vjj = 0;
    double ciijj = 0;
    double niijj=0;
    double rii=0;
    double rjj=0;

    for (int zz=0;zz<NIP;zz++){
        int ii = ip(zz,0);
        int jj = ip(zz,1);
        //        ps.iijj <- colSums( posterior[ data.resp[,ii]*data.resp[,jj]>0, ] )
        for (int tt=0;tt<TP;tt++){
            t1 = 0;
            for (int nn=0;nn<N;nn++){  // begin nn
                if ( ( data_resp_bool(nn,ii) ) & ( data_resp_bool(nn,jj) ) ){
                    t1 += posterior(nn,tt);
                }
            }    // end nn
            psiijj[tt] = t1;
        }

        //         itempairs[ii1,"Exp11"] <- sum( probs[ii,2,]*probs[jj,2,] * ps.iijj )
        for (int vv=0;vv<TP;vv++){
            itempair_stat(zz,0) += probs1(ii,vv) * probs1(jj,vv) * psiijj[vv];
            itempair_stat(zz,1) += probs1(ii,vv) * probs0(jj,vv) * psiijj[vv];
            itempair_stat(zz,2) += probs0(ii,vv) * probs1(jj,vv) * psiijj[vv];
            itempair_stat(zz,3) += probs0(ii,vv) * probs0(jj,vv) * psiijj[vv];
        }

        /// calculation of Q3 statistic
        mii = 0;
        mjj = 0;
        vii = 0;
        vjj = 0;
        ciijj = 0;
        niijj=0;

        for (int nn=0;nn<N;nn++){
            if ( ( data_resp_bool(nn,ii) ) & ( data_resp_bool(nn,jj) ) ){
                niijj ++;
                // calculate residuals
                rii = data(nn,ii) - expiijj(nn,ii);
                rjj = data(nn,jj) - expiijj(nn,jj);
                // calculate means
                mii += rii;
                mjj += rjj;
                // calculate covariances and variances
                ciijj += rii*rjj;
                vii += rii*rii;
                vjj += rjj*rjj;
            }
        }
        // means
        mii = mii / niijj;
        mjj = mjj / niijj;
        // variances and covariances
        vii = ( vii - niijj * mii * mii ) / ( niijj - 1 );
        vjj = ( vjj - niijj * mjj * mjj ) / ( niijj - 1 );
        ciijj = ( ciijj - niijj * mii * mjj ) / ( niijj - 1 );
        Q3[zz] = ciijj / sqrt( vii * vjj );
    }   // end zz ( item pairs ii and jj )

    /////////////////////////////////////////////
    // OUTPUT:
    return Rcpp::List::create(
                Rcpp::Named("itempair_stat") =itempair_stat,
                Rcpp::Named("Q3") = Q3
        );
}
///********************************************************************


///********************************************************************
//** frequencies for model fit function
///** cdm_rcpp_modelfit_cor_counts
// [[Rcpp::export]]
Rcpp::List cdm_rcpp_modelfit_cor_counts( Rcpp::IntegerMatrix data,
            Rcpp::LogicalMatrix data_resp_bool )
{
    int N = data.nrow();
    int I = data.ncol();
    Rcpp::IntegerMatrix n11(I,I);
    Rcpp::IntegerMatrix n10(I,I);
    Rcpp::IntegerMatrix n01(I,I);
    Rcpp::IntegerMatrix n00(I,I);
    for (int nn=0; nn<N; nn++){
        for (int ii=0; ii<I; ii++){
            if ( data_resp_bool(nn,ii) ){
                for (int jj=ii; jj<I; jj++){
                    if ( data_resp_bool(nn,jj) ){
                        if ( data(nn,ii) == 1 ){
                            if ( data(nn,jj) == 1 ){ n11(ii,jj) ++;    }
                            if ( data(nn,jj) == 0 ){ n10(ii,jj) ++;    }
                        }
                        if ( data(nn,ii) == 0 ){
                            if ( data(nn,jj) == 1 ){ n01(ii,jj) ++;    }
                            if ( data(nn,jj) == 0 ){ n00(ii,jj) ++;    }
                        }
                    }
                }
            }
        }
    }

    for (int ii=0; ii<I; ii++){
        for (int jj=ii; jj<I; jj++){
            n11(jj,ii) = n11(ii,jj);
            n10(jj,ii) = n01(ii,jj);
            n01(jj,ii) = n10(ii,jj);
            n00(jj,ii) = n00(ii,jj);
        }
    }

    //-- output
    return Rcpp::List::create(
            Rcpp::Named("n11") = n11,
            Rcpp::Named("n10") = n10,
            Rcpp::Named("n01") = n01,
            Rcpp::Named("n00") = n00
        );
}
///********************************************************************

