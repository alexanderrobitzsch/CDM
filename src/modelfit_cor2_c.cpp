
// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///**  modelfit_cor2_Cpp

// [[Rcpp::export]]
Rcpp::List modelfit_cor2_Cpp( Rcpp::NumericMatrix posterior, 
        Rcpp::NumericMatrix data, Rcpp::NumericMatrix dataresp, Rcpp::NumericMatrix probs1, 
        Rcpp::NumericMatrix probs0, Rcpp::NumericMatrix ip, 
        Rcpp::NumericMatrix expiijj ){
        
     int NIP = ip.nrow() ;  
     int N = posterior.nrow() ;  
     int TP = posterior.ncol() ;  
     Rcpp::NumericMatrix itempair_stat(NIP,4) ;  
     Rcpp::NumericVector psiijj(TP) ;  
     Rcpp::NumericVector Q3(NIP) ;                
     double t1 = 0 ;  
     double mii = 0 ;  
     double mjj = 0 ;  
     double vii = 0 ;  
     double vjj = 0 ;  
     double ciijj = 0 ;  
     double niijj=0 ;  
     double rii=0;  
     double rjj=0;  
       
     for (int zz=0;zz<NIP;zz++){  
       
     int ii = ip(zz,0);  
     int jj = ip(zz,1);  
       
     //		ps.iijj <- colSums( posterior[ data.resp[,ii]*data.resp[,jj]>0 , ] )  
     for (int tt=0;tt<TP;tt++){  
         t1 = 0 ;   
         for (int nn=0;nn<N;nn++){  // begin nn  
             if ( ( dataresp(nn,ii) > 0 ) & ( dataresp(nn,jj) > 0 ) ){  
                 t1 += posterior(nn,tt) ;  
             }  
         }      // end nn          
         psiijj[tt] = t1 ;        
     }              
       
     //    	 itempairs[ii1,"Exp11"] <- sum( probs[ii,2,]*probs[jj,2,] * ps.iijj )          
     //    ....  
               
     for (int vv=0;vv<TP;vv++){  
         itempair_stat(zz,0) += probs1(ii,vv) * probs1(jj,vv) * psiijj[vv] ;  
         itempair_stat(zz,1) += probs1(ii,vv) * probs0(jj,vv) * psiijj[vv] ;      
         itempair_stat(zz,2) += probs0(ii,vv) * probs1(jj,vv) * psiijj[vv] ;          
         itempair_stat(zz,3) += probs0(ii,vv) * probs0(jj,vv) * psiijj[vv] ;              
      }  
       
        /// calculation of Q3 statistic  
         mii = 0 ;  
         mjj = 0 ;  
         vii = 0 ;  
         vjj = 0 ;  
         ciijj = 0 ;  
         niijj=0 ;  
              
         for (int nn=0;nn<N;nn++){              
             // int nn = 0 ;  
             if ( ( dataresp(nn,ii) > 0 ) & ( dataresp(jj,ii) > 0 ) ){  
                  niijj ++ ;     
                  // calculate residuals  
                  rii = data(nn,ii) - expiijj(nn,ii) ;      
                  rjj = data(nn,jj) - expiijj(nn,jj) ;      
                  // calculate means  
                  mii += rii ;  
                  mjj += rjj ;  
                  // calculate covariances and variances  
                  ciijj += rii*rjj ;  
                  vii += rii*rii ;  
                  vjj += rjj*rjj ;      
                             }  
                         }      
             // means  
             mii = mii / niijj ;  
             mjj = mjj / niijj ;  
             // variances and covariances  
             vii = ( vii - niijj * mii * mii ) / ( niijj - 1 ) ;              
             vjj = ( vjj - niijj * mjj * mjj ) / ( niijj - 1 ) ;                      
             ciijj = ( ciijj - niijj * mii * mjj ) / ( niijj - 1 ) ;    
             Q3[zz] = ciijj / sqrt( vii * vjj ) ;      
                       
             }   // end zz ( item pairs ii and jj )  
                      
     /////////////////////////////////////////////  
     // OUTPUT:  
     return Rcpp::List::create(  
         Rcpp::_["itempair_stat"] =itempair_stat ,  
         Rcpp::_["Q3"] = Q3   
                 ) ;         
}
///********************************************************************


