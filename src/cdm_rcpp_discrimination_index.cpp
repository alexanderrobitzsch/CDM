//// File Name: cdm_rcpp_discrimination_index.cpp
//// File Version: 0.858


#include <Rcpp.h>
// #include <RcppArmadillo.h>

using namespace Rcpp;
// using namespace arma;



///********************************************************************
///** cdm_rcpp_discrimination_index_compare_vectors
// [[RcppNOexport]]
bool cdm_rcpp_discrimination_index_compare_vectors( Rcpp::NumericVector patt0,
        Rcpp::NumericVector patt1, int patt_index)
{
    int K = patt0.size();
    bool comp_vec = FALSE;
    if (patt0[ patt_index ] > patt1[ patt_index ]){
        comp_vec = TRUE;
    }
    bool comp_vec0 = comp_vec;
    if(comp_vec){
        for (int kk=0; kk<K; kk++){
            if ( kk != patt_index){
                if ( patt0[kk] != patt1[kk] ){
                    comp_vec0 = FALSE;
                }
            }
            if (! comp_vec0) { break;}
        }
    }

    /// OUTPUT
    return comp_vec0;
}
///********************************************************************


///********************************************************************
///** cdm_rcpp_discrimination_index_attribute_patterns
// [[Rcpp::export]]
Rcpp::IntegerMatrix cdm_rcpp_discrimination_index_attribute_patterns( Rcpp::NumericMatrix attr_patt)
{
    int K = attr_patt.ncol();
    int AP = attr_patt.nrow();
    Rcpp::NumericVector patt0(K);
    Rcpp::NumericVector patt1(K);
    bool comp_vec=FALSE;
    int NH = K*AP / 2;
    Rcpp::IntegerMatrix comp_matrix(NH,3);
    int cc=0;
    bool comp_ii = TRUE;

    for (int aa=0; aa<K; aa++){
        for (int ii=0; ii<AP; ii++){
            patt0 = attr_patt(ii,_);
            for (int jj=0; jj<AP; jj++){
                comp_ii = TRUE;
                patt1 = attr_patt(jj,_);
                comp_vec = cdm_rcpp_discrimination_index_compare_vectors( patt0, patt1, aa);
                if (comp_vec){
                    comp_matrix(cc,0) = aa;
                    comp_matrix(cc,1) = ii;
                    comp_matrix(cc,2) = jj;
                    cc ++;
                    comp_ii = FALSE;
                }
                if ( ! comp_ii){ break; }
            }
        }
    }  // end skill aa

    ///---- OUTPUT
    return comp_matrix;
}
///********************************************************************


///********************************************************************
///** cdm_rcpp_abs_difference
// [[RcppNOexport]]
double cdm_rcpp_abs_difference( double x, double y )
{
    double d = x - y;
    if ( d < 0){ d = - d;}
    return d;
}
///********************************************************************


///********************************************************************
///** cdm_rcpp_discrimination_index_calc
// [[Rcpp::export]]
Rcpp::NumericMatrix cdm_rcpp_discrimination_index_calc( Rcpp::IntegerMatrix comp_matrix,
        Rcpp::NumericVector probs, Rcpp::NumericVector dim_probs, int K )
{
    int I = dim_probs[0];
    int ncat = dim_probs[1];
    Rcpp::NumericMatrix discrim_item(I,K);
    discrim_item.fill(0);
    int AC = comp_matrix.nrow();
    double val0 = 0;
    double val1 = 0;
    int skill = 0;
    int cc1 = 0;
    int cc2 = 0;
    int ind1 = 0;
    int ind2 = 0;

    for (int ac = 0; ac < AC; ac++){
        skill = comp_matrix(ac, 0);
        cc1 = comp_matrix(ac, 1);
        cc2 = comp_matrix(ac, 2);
        for (int ii = 0; ii<I; ii++){ // item
            val0 = discrim_item(ii, skill);
            val1=0;
            for (int hh=0; hh<ncat; hh++){
                ind1 = ii + hh*I + I*ncat*cc1;
                ind2 = ii + hh*I + I*ncat*cc2;
                val1 += cdm_rcpp_abs_difference( probs[ind1], probs[ind2] );
            }
            val1 = 0.5*val1;
            if ( val1 > val0 ){
                discrim_item(ii, skill ) = val1;
            }
        }
    }
    ///---- OUTPUT
    return discrim_item;
}
///********************************************************************


///********************************************************************
///** cdm_rcpp_discrimination_index_test_level
// [[Rcpp::export]]
double cdm_rcpp_discrimination_index_test_level( Rcpp::NumericMatrix discrim_item_attribute )
{
    double I = discrim_item_attribute.nrow();
    int K = discrim_item_attribute.ncol();
    double discrim_test = 0;
    double temp=0; double temp1=0;

    for (int ii=0; ii<I; ii++){
        temp=0;
        for (int kk=0; kk<K; kk++){
            temp1 = discrim_item_attribute(ii,kk);
            if (temp1 > temp){
                temp = temp1;
            }
        }
        discrim_test = discrim_test + temp;
    }
    discrim_test = discrim_test / I;

    ///---- OUTPUT
    return discrim_test;
}
///********************************************************************


///********************************************************************
///** cdm_rcpp_discrimination_index_calc
// [[Rcpp::export]]
Rcpp::NumericVector cdm_rcpp_discrimination_index_idi( Rcpp::NumericVector probs,
        Rcpp::NumericVector dim_probs, int K )
{
    int I = dim_probs[0];
    int ncat = dim_probs[1];
    int TP = dim_probs[2];
    Rcpp::NumericVector idi(I);
    double minval=0;
    double maxval=0;
    double prob=0;
    double diff=0;

    for (int ii=0; ii<I; ii++){      // item ii
        for (int hh=0; hh<ncat; hh++){    // category hh
            minval=1;
            maxval=0;
            for (int tt=0; tt<TP; tt++){  // skill pattern tt
                prob = probs[ ii + hh*I + tt*I*ncat ];
                if (prob < minval ){
                    minval = prob;
                }
                if (prob > maxval ){
                    maxval = prob;
                }
            }
            diff = maxval - minval;
            if ( diff > idi[ii] ){
                idi[ii] = diff;
            }
        }
    }
    ///---- OUTPUT
    return idi;
}
///********************************************************************
