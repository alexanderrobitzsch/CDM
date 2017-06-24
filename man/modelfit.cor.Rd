\name{modelfit.cor}
\alias{modelfit.cor}
\alias{modelfit.cor2}
\alias{modelfit.cor.din}
\alias{summary.modelfit.cor.din}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assessing Model Fit and Local Dependence by Comparing Observed and Expected 
Item Pair Correlations
}

\description{
This function computes several measures of absolute model fit and local
dependence indices for dichotomous item responses which are 
based on comparing observed and expected frequencies of item pairs
(Chen, de la Torre & Zhang, 2013; see Details).
}

\usage{
modelfit.cor(data, posterior, probs)
modelfit.cor2(data, posterior, probs)

modelfit.cor.din( dinobj , jkunits=0 )

\method{summary}{modelfit.cor.din}(object, \dots)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
An \eqn{N \times I} data frame of dichotomous item responses
}
  \item{posterior}{
A matrix containing the posterior distribution (e.g. obtained as
an output of the \code{\link{din}} function).
}
  \item{probs}{
An array of dimension [items,categories,attribute classes]
containing probabilities
}
\item{dinobj}{
 An object of class \code{\link{din}}, \code{\link{gdina}} or
 \code{\link{gdm}} (only for dichotomous item responses)
}
\item{object}{
 An object of class \code{\link{din}}, \code{\link{gdina}} or
 \code{\link{gdm}} (only for dichotomous item responses)
}
\item{jkunits}{Number of Jackknife units. The default is to use 0 units
	(no use of jackknifing). If jackknife estimation should be
	employed, use (say) at least 20 jackknife units. 
The input \code{jkunits} can be also a vector of jackknife unit identifiers.
}
\item{\dots}{Further arguments to be passed}
}


\details{
The fit statistics are based on predictions of the pairwise table
\eqn{(X_i , X_j)} of item responses. The \eqn{\chi^2} statistic \code{X2} for 
item pairs \eqn{i} and \eqn{j} is defined as
\deqn{ \chi^2_{ij} = \sum_{k=0}^1 \sum_{l=0}^1 \frac{ (n_{ij,kl}-e_{ij,kl}) ^2 }{ e_{ij,kl} }}
where \eqn{n_{ij,kl}} is the absolute frequency of \eqn{\{ X_{i}=k,X_j=l\}}
and \eqn{e_{ij,kl}} is the expected frequency using the estimated model.
Note that for calculating \eqn{e_{ij,kl}}, individual posterior distributions
are evaluated.  The \eqn{\chi^2_{ij} } statistic is chi-square distributed with one
degree of freedom and can be used for testing whether items \eqn{i} and
\eqn{j} are locally dependent. To control for multiple comparisons,
p-value adjustments according to the Holm and FDR method are conducted
(see \code{\link[stats:p.adjust]{stats::p.adjust}}).
	
% The mean \eqn{\chi^2} statistic \code{MX2} is just the average
% of all \eqn{ \chi^2_{ij}} statistics across all item pairs.

The residual covariance \code{RESIDCOV} of item pairs \eqn{(i,j)} is calculated
as \deqn{ RESIDCOV_{ij} = 
	\frac{ n_{ij,11} n_{ij,00} - n_{ij,10} n_{ij,01} }{n^2 } -
		\frac{ e_{ij,11} e_{ij,00} - e_{ij,10} e_{ij,01} }{n^2 } }
where \code{MRESIDCOV} is the average of all \code{RESIDCOV} statistics
and is the total sample size.

The statistic \code{MADcor} denotes the average absolute deviation between
observed correlations \eqn{r_{ij}} and model predicted correlations 
\eqn{\hat{r}_{ij}} of item pairs \eqn{(i,j)}:
\deqn{ MADcor = \frac{1}{ J(J-1)/2 } \sum_{i < j} | r_{ij} - \hat{r}_{ij} |}

The SRMSR (standardized root mean square root of squared residuals,
Maydeu-Olivaras, 2013) is also based on comparing these correlations
\deqn{ SRMSR = \sqrt{ \frac{1}{ J(J-1)/2 } \sum_{i < j} 
	( r_{ij} - \hat{r}_{ij} )^2 } }

For calculating \code{MADQ3} and \code{MADaQ3}, 
residuals \eqn{\varepsilon_{ni} = X_{ni} - e_{ni}} of
observed and expected responses for respondents \eqn{n} and items \eqn{i} are
constructed. Then, the average of the absolute values of pairwise correlations
of these residuals is computed for \code{MADQ3}. For \code{MADaQ3}, the average 
of the centered pairwise values (i.e. by subtracting the average Q3 statistic)
is calculated.

The difference of Fisher transformed correlations (Chen et al., 2013) is also
computed and used for assessing statistical inference.

For every of the fit statistics \code{MADcor}, \code{MADacor}, \code{SRMSR}, \code{MX2},
\code{100*MADRESIDCOV} and \code{MADQ3} it holds that smaller values 
(values near to zero) indicate better fit.

Standard errors and confidence intervals of fit statistics are obtained
by Jackknife estimation.
}


\value{
A list with following entries
  \item{modelfit.stat}{Model fit statistics:
  	  
   \code{MADcor}: mean of absolute deviations in observed and expected correlations 
        (DiBello et al., 2007) 

   \code{SRMSR}: standardized mean square rooot of squared residuals 
        (Maydeu-Olivares, 2013; Maydeu-Olivares & Joe, 2014) 
                
%    \code{MX2}: Mean of \eqn{\chi^2} statistics of all item pairs 
%    (Chen & Thissen, 1997)
    
    \code{MADRESIDCOV}: Mean of absolute deviations of residual covariances
    (McDonald & Mok, 1995) 
    
    \code{MADQ3}: Mean of absolute values of \eqn{Q_3} statistic (Yen, 1984)

    \code{MADaQ3}: Mean of absolute values of centered \eqn{Q_3} statistic
    
            }
\item{modelfit.test}{Test of global absolute model fit using test
	statistics of all item pairs. The statistic \code{max(X2)} is the
	maximum of all \eqn{\chi^2_{ij}} statistics accompanied with a p value
	obtained by the Holm procedure. A similar statistic \code{abs(fcor)}
	is created as the absolute value of the deviations of Fisher
	transformed correlations as used in Chen et al. (2013).
}           
  \item{itempairs}{
  	  Fit of itempairs which can be used for inspection of local
  	  dependence. The \eqn{\chi^2_{ij}} statistic is denoted by \code{X2}
  	  (Chen & Thissen, 1997), the statistic \eqn{r_{ij}} based on absolute 
  	  deviations of observed and predicted correlations is \code{fcor}
  	  (Chen et al., 2013).
  }
   
}

\references{
Chen, J., de la Torre, J., & Zhang, Z. (2013). 
Relative and absolute fit evaluation in cognitive diagnosis modeling. 
\emph{Journal of Educational Measurement}, \bold{50}, 123-140.	

Chen, W., & Thissen, D. (1997). Local dependence indexes for item pairs 
using item response theory. \emph{Journal of Educational and Behavioral Statistics},
\bold{22}, 265-289.

DiBello, L. V., Roussos, L. A., & Stout, W. F. (2007). Review of
cognitively diagnostic assessment and a summary of psychometric models.
In C. R. Rao and S. Sinharay (Eds.), \emph{Handbook of Statistics},
Vol. 26 (pp. 979--1030). Amsterdam: Elsevier.

Maydeu-Olivares, A. (2013). Goodness-of-fit assessment of item response
theory models (with discussion). 
\emph{Measurement: Interdisciplinary Research and Perspectives},
\bold{11}, 71-137.

Maydeu-Olivares, A., & Joe, H. (2014). Assessing approximate fit in categorical 
data analysis. \emph{Multivariate Behavioral Research}, \bold{49}, 305-328.


McDonald, R. P., & Mok, M. M.-C. (1995). Goodness of fit in item response models.
\emph{Multivariate Behavioral Research}, \bold{30}, 23-40.

Yen, W. M. (1984). Effects of local item dependence on the fit and equating 
performance of the three-parameter logistic model. 
\emph{Applied Psychological Measurement}, \bold{8}, 125-145.
}
%\author{
%Alexander Robitzsch
%}
\note{
The function does not handle sample weights properly.	
		
The function \code{modelfit.cor2} has the same functionality as
\code{modelfit.cor} but it is much faster because it is based on
\pkg{Rcpp} code.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{	
#############################################################################
# EXAMPLE 1: Model fit for sim.dina
#############################################################################
data(sim.dina)
data(sim.qmatrix)

#*** Model 1: DINA model for DINA simulated data
mod1 <- CDM::din(sim.dina, q.matr = sim.qmatrix, rule = "DINA" )
fmod1 <- CDM::modelfit.cor.din(mod1 , jkunits=10)
summary(fmod1)
  ##   Test of Global Model Fit
  ##          type value     p
  ##   1   max(X2) 8.728 0.113
  ##   2 abs(fcor) 0.143 0.080
  ##   
  ##   Fit Statistics
  ##                     est jkunits jk_est jk_se est_low est_upp
  ##   MADcor          0.030      10  0.020 0.005   0.010   0.030
  ##   SRMSR           0.040      10  0.023 0.006   0.011   0.035
  ##   100*MADRESIDCOV 0.671      10  0.445 0.125   0.200   0.690
  ##   MADQ3           0.062      10  0.037 0.008   0.021   0.052
  ##   MADaQ3          0.059      10  0.034 0.008   0.019   0.050

# look at first five item pairs with highest degree of local dependence
itempairs <- fmod1$itempairs
itempairs <- itempairs[ order( itempairs$X2 , decreasing=TRUE ) , ]
itempairs[ 1:5 , c("item1","item2" , "X2" , "X2_p" , "X2_p.holm" , "Q3") ]
  ##      item1 item2       X2        X2_p X2_p.holm          Q3
  ##   29 Item5 Item8 8.728248 0.003133174 0.1127943 -0.26616414
  ##   32 Item6 Item8 2.644912 0.103881881 1.0000000  0.04873154
  ##   21 Item3 Item9 2.195011 0.138458201 1.0000000  0.05948456
  ##   10 Item2 Item4 1.449106 0.228671389 1.0000000 -0.08036216
  ##   30 Item5 Item9 1.393583 0.237800911 1.0000000 -0.01934420

#*** Model 2: DINO model for DINA simulated data
mod2 <- CDM::din(sim.dina, q.matr = sim.qmatrix, rule = "DINO" )
fmod2 <- CDM::modelfit.cor.din(mod2 , jkunits=10 )   # 10 jackknife units
summary(fmod2)
  ##   Test of Global Model Fit
  ##          type  value     p
  ##   1   max(X2) 13.139 0.010
  ##   2 abs(fcor)  0.199 0.001
  ##   
  ##   Fit Statistics
  ##                     est jkunits jk_est jk_se est_low est_upp
  ##   MADcor          0.056      10  0.041 0.007   0.026   0.055
  ##   SRMSR           0.072      10  0.045 0.019   0.007   0.083
  ##   100*MADRESIDCOV 1.225      10  0.878 0.183   0.519   1.236
  ##   MADQ3           0.073      10  0.055 0.012   0.031   0.080
  ##   MADaQ3          0.073      10  0.066 0.012   0.042   0.089

#*** Model 3: estimate DINA model with gdina function
mod3 <- CDM::gdina( sim.dina , q.matr = sim.qmatrix , rule="DINA" )
fmod3 <- CDM::modelfit.cor.din( mod3 , jkunits=0 )  # no Jackknife estimation
summary(fmod3)
  ##   Test of Global Model Fit
  ##          type value     p
  ##   1   max(X2) 8.756 0.111
  ##   2 abs(fcor) 0.143 0.078
  ##   
  ##   Fit Statistics
  ##                     est
  ##   MADcor          0.030
  ##   SRMSR           0.040
  ##   MX2             0.719
  ##   100*MADRESIDCOV 0.668
  ##   MADQ3           0.062
  ##   MADaQ3          0.059
  
#############################################################################
# EXAMPLE 2: Simulated Example DINA model
#############################################################################

set.seed(9765)
# specify Q-matrix
Q <- matrix( c(1,0, 0,1 , 1,1 ) , nrow=3 , ncol=2 , byrow=TRUE )
q.matrix <- Q[ rep(1:3,4) , ]
I <- nrow(q.matrix)

# simulate data
guess <- stats::runif(I, 0 , .3 )
slip <- stats::runif( I , 0 , .4 )
N <- 150   # number of persons
dat <- sim.din( N=N, q.matrix=q.matrix , slip=slip , guess=guess )$dat

#*** estmate DINA model
mod1 <- CDM::din( dat , q.matrix = q.matrix, rule = "DINA" )
fmod1 <- CDM::modelfit.cor.din(mod1 , jkunits=10)
summary(fmod1)
  ##  Test of Global Model Fit
  ##         type  value     p
  ##  1   max(X2) 10.697 0.071
  ##  2 abs(fcor)  0.277 0.026
  ##  
  ##  Fit Statistics
  ##                    est jkunits jk_est jk_se est_low est_upp
  ##  MADcor          0.052      10  0.026 0.010   0.006   0.045
  ##  SRMSR           0.074      10  0.048 0.013   0.022   0.074
  ##  100*MADRESIDCOV 1.259      10  0.646 0.213   0.228   1.063
  ##  MADQ3           0.080      10  0.047 0.010   0.027   0.068
  ##  MADaQ3          0.079      10  0.046 0.010   0.027   0.065  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Model fit}
\keyword{Local dependence}
\keyword{summary}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
