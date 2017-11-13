%% File Name: data.hr.Rd
%% File Version: 0.12

\name{data.hr}
\alias{data.hr}

\docType{data}

\title{
Dataset \code{data.hr} (Ravand et al., 2013)
}

\description{
Dataset \code{data.hr} used for illustrating some functionalities
of the \pkg{CDM} package (Ravand, Barati, & Widhiarso, 2013).
}

\usage{
data(data.hr)
}


\format{
The format of the dataset is:
  
\code{List of 2} \cr
\code{ $ data    : num [1:1550, 1:35] 1 0 1 1 1 0 1 1 1 0 ...} \cr
\code{ $ q.matrix:'data.frame':} \cr
\code{  ..$ Skill1: int [1:35] 0 0 0 0 0 0 1 0 0 0 ...} \cr
\code{  ..$ Skill2: int [1:35] 0 0 0 0 1 0 0 0 0 0 ...} \cr
\code{  ..$ Skill3: int [1:35] 0 1 1 1 1 0 0 1 0 0 ...} \cr
\code{  ..$ Skill4: int [1:35] 1 0 0 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ Skill5: int [1:35] 0 0 0 0 0 1 0 0 1 1 ...} \cr

}


%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
% }


\source{
Simulated data according to Ravand et al. (2013).
}

\references{
Ravand, H., Barati, H., & Widhiarso, W. (2013). Exploring diagnostic capacity
of a high stakes reading comprehension test: A pedagogical demonstration. 
\emph{Iranian Journal of Language Testing}, \bold{3}(1), 1-27.
}


\examples{
\dontrun{
	
dat <- CDM::data.hr$data	
Q <- CDM::data.hr$q.matrix

#*************
# Model 1: DINA model
mod1 <- CDM::din( dat, q.matrix = Q )
summary(mod1)       # summary

# plot results
plot(mod1)

# inspect coefficients
coef(mod1)

# posterior distribution
posterior <- mod1$posterior
round( posterior[ 1:5 , ] , 4 )  # first 5 entries

# estimate class probabilities
mod1$attribute.patt

# individual classifications
mod1$pattern[1:5,]   # first 5 entries

#*************
# Model 2: GDINA model
mod2 <- CDM::gdina( dat, q.matrix = Q)
summary(mod2)  

#*************
# Model 3: Reduced RUM model
mod3 <- CDM::gdina( dat, q.matrix = Q, rule="RRUM" )
summary(mod3)  

#--------
# model comparisons

# DINA vs GDINA
anova( mod1 , mod2 )
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   1 Model 1 -31391.27 62782.54   101 62984.54 63524.49 195.9099 20  0
  ##   2 Model 2 -31293.32 62586.63   121 62828.63 63475.50       NA NA NA

# RRUM vs. GDINA
anova( mod2 , mod3 )
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   2 Model 2 -31356.22 62712.43   105 62922.43 63483.76 125.7924 16  0
  ##   1 Model 1 -31293.32 62586.64   121 62828.64 63475.50       NA NA NA

# DINA vs. RRUM
anova(mod1,mod3)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   1 Model 1 -31391.27 62782.54   101 62984.54 63524.49 70.11246  4  0
  ##   2 Model 2 -31356.22 62712.43   105 62922.43 63483.76       NA NA NA

#-------
# model fit

# DINA
fmod1 <- CDM::modelfit.cor.din( mod1 , jkunits=0)
summary(fmod1)
  ##   Test of Global Model Fit
  ##          type    value       p
  ##   1   max(X2) 16.35495 0.03125
  ##   2 abs(fcor)  0.10341 0.01416
  ##   
  ##   Fit Statistics
  ##                       est
  ##   MADcor          0.01911
  ##   SRMSR           0.02445
  ##   MX2             0.93157
  ##   100*MADRESIDCOV 0.39100
  ##   MADQ3           0.02373

# GDINA
fmod2 <- CDM::modelfit.cor.din( mod2 , jkunits=0)
summary(fmod2)
  ##   Test of Global Model Fit
  ##          type   value p
  ##   1   max(X2) 7.73670 1
  ##   2 abs(fcor) 0.07215 1
  ##   
  ##   Fit Statistics
  ##                       est
  ##   MADcor          0.01830
  ##   SRMSR           0.02300
  ##   MX2             0.82584
  ##   100*MADRESIDCOV 0.37390
  ##   MADQ3           0.02383

# RRUM
fmod3 <- CDM::modelfit.cor.din( mod3, jkunits=0)
summary(fmod3)
  ##   Test of Global Model Fit
  ##          type    value       p
  ##   1   max(X2) 15.49369 0.04925
  ##   2 abs(fcor)  0.10076 0.02201
  ##   
  ##   Fit Statistics
  ##                       est
  ##   MADcor          0.01868
  ##   SRMSR           0.02374
  ##   MX2             0.87999
  ##   100*MADRESIDCOV 0.38409
  ##   MADQ3           0.02416
}
}
\keyword{datasets}
