//// File Name: itemfit_sx2_calc_scoredistribution_cdm.cpp
//// File Version: 3.02


// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

///********************************************************************
///**  calc_scoredistribution_cdm
// [[Rcpp::export]]
Rcpp::NumericMatrix calc_scoredistribution_cdm( Rcpp::NumericMatrix P1, 
		Rcpp::NumericMatrix Q1 )
{

	int TP= P1.nrow();  
	int I= P1.ncol() ;  
	Rcpp::NumericMatrix scoredist(TP,I+1) ;  
	Rcpp::NumericMatrix scoredist0(TP,I+1);  

	scoredist(_,0) = Q1(_,0) ;  
	scoredist(_,1) = P1(_,0) ;  

	for (int ii=1 ; ii < I ; ii++ ){  
		scoredist0 = scoredist ;  
		for (int hh=0;hh<TP;hh++){  
			scoredist(hh,ii+1) = P1(hh,ii)*scoredist0(hh,ii) ;  
		} 
		for (int kk=0; kk < ii ; kk++){  
			for (int hh=0;hh<TP;hh++){  
				scoredist(hh,ii-kk) = Q1(hh,ii) * scoredist0(hh,ii-kk) + P1(hh,ii) * scoredist0(hh,ii-kk-1);
			} 
		}  
		for (int hh=0;hh<TP;hh++){  
			scoredist(hh,0) = Q1(hh,ii)*scoredist0(hh,0) ;  
		}  
	}         
	return scoredist ;
}
///********************************************************************

