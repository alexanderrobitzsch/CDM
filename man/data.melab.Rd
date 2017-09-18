%% File Name: data.melab.Rd
%% File Version: 0.10
%% File Last Change: 2017-01-31 14:08:30

\name{data.melab}
\alias{data.melab}
\docType{data}

\title{
MELAB Data (Li, 2011)
}

\description{
This is a simulated dataset according to the MELAB reading
study (Li, 2011; Li & Suen, 2013). Li (2011) investigated the Fusion
model (RUM model) for calibrating this dataset. The dataset in this package
is simulated assuming the reduced RUM model (RRUM).
}

\usage{
data(data.melab)
}


\format{
The format of the dataset is:

\code{List of 3} \cr
\code{ $ data        : num [1:2019, 1:20] 0 1 0 1 1 0 0 0 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:20] "I1" "I2" "I3" "I4" ...} \cr
\code{ $ q.matrix    :'data.frame':} \cr
\code{  ..$ skill1: int [1:20] 1 1 0 0 1 1 0 1 0 1 ...} \cr
\code{  ..$ skill2: int [1:20] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ skill3: int [1:20] 0 0 0 1 0 1 1 0 1 0 ...} \cr
\code{  ..$ skill4: int [1:20] 1 0 1 0 1 0 0 1 0 1 ...} \cr
\code{ $ skill.labels:'data.frame':} \cr
\code{  ..$ skill      : Factor w/ 4 levels "skill1","skill2",..: 1 2 3 4} \cr
\code{  ..$ skill.label: Factor w/ 4 levels "connecting and synthesizing",..: 4 3 2 1} \cr
}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
Simulated data according to Li (2011).
}

\references{
Li, H. (2011). A cognitive diagnostic analysis of the MELAB reading test. 
\emph{Spaan Fellow}, \bold{9}, 17-46.
	
Li, H., & Suen, H. K. (2013). Constructing and validating a Q-matrix for 
cognitive diagnostic analyses of a reading test. 
\emph{Educational Assessment}, \bold{18}, 1-25.
}


\examples{
\dontrun{	
data(data.melab)
data <- data.melab$data
q.matrix <- data.melab$q.matrix

#*** Model 1: Reduced RUM model
mod1 <- CDM::gdina( data , q.matrix , rule="RRUM" )
summary(mod1)

#*** Model 2: GDINA model
mod2 <- CDM::gdina( data , q.matrix , rule="GDINA" )
summary(mod2)

#*** Model 3: DINA model
mod3 <- CDM::gdina( data , q.matrix , rule="DINA" )
summary(mod3)

#*** Model 4: 2PL model
mod4 <- CDM::gdm( data , theta.k=seq(-6,6,len=21) , center )
summary(mod4)

#----
# Model comparisons

#*** RRUM vs. GDINA
anova(mod1,mod2)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df       p
  ##   1 Model 1 -20252.74 40505.48    69 40643.48 41030.60 30.88801 18 0.02966
  ##   2 Model 2 -20237.30 40474.59    87 40648.59 41136.69       NA NA      NA

  ##  -> GDINA is not superior to RRUM (according to AIC and BIC)

#*** DINA vs. RRUM
anova(mod1,mod3)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   2 Model 2 -20332.52 40665.04    55 40775.04 41083.61 159.5566 14  0
  ##   1 Model 1 -20252.74 40505.48    69 40643.48 41030.60       NA NA NA

  ##  -> RRUM fits the data significantly better than the DINA model

#*** RRUM vs. 2PL (use only AIC and BIC for comparison)
anova(mod1,mod4)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   2 Model 2 -20390.19 40780.38    43 40866.38 41107.62 274.8962 26  0
  ##   1 Model 1 -20252.74 40505.48    69 40643.48 41030.60       NA NA NA

  ## -> RRUM fits the data better than 2PL

#----
# Model fit statistics

# RRUM
fmod1 <- CDM::modelfit.cor.din( mod1 , jkunits=0)
summary(fmod1)
  ##   Test of Global Model Fit
  ##          type    value       p
  ##   1   max(X2) 10.10408 0.28109
  ##   2 abs(fcor)  0.06726 0.24023
  ##   
  ##   Fit Statistics
  ##                       est
  ##   MADcor          0.01708
  ##   SRMSR           0.02158
  ##   MX2             0.96590
  ##   100*MADRESIDCOV 0.27269
  ##   MADQ3           0.02781

  ##  -> not a significant misfit of the RRUM model

# GDINA
fmod2 <- CDM::modelfit.cor.din( mod2 , jkunits=0)
summary(fmod2)
  ##   Test of Global Model Fit
  ##          type    value       p
  ##   1   max(X2) 10.40294 0.23905
  ##   2 abs(fcor)  0.06817 0.20964
  ##   
  ##   Fit Statistics
  ##                       est
  ##   MADcor          0.01703
  ##   SRMSR           0.02151
  ##   MX2             0.94468
  ##   100*MADRESIDCOV 0.27105
  ##   MADQ3           0.02713
}
}


\keyword{datasets}
