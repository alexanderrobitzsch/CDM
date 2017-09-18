%% File Name: IRT.compareModels.Rd
%% File Version: 0.12
%% File Last Change: 2017-01-31 14:08:31

\name{IRT.compareModels}
\alias{IRT.compareModels}
\alias{summary.IRT.compareModels}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparisons of Several Models
}

\description{
Performs model comparisons based on information criteria and likelihood
ratio test. This function allows all objects for which the
\code{\link[stats:logLik]{logLik}} (\pkg{stats}) S3 method is defined.
The output of \code{\link{IRT.modelfit}} can also be used as
input for this function.
}

\usage{
IRT.compareModels(object, ...)

\method{summary}{IRT.compareModels}(object, extended=TRUE, \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object
}
\item{extended}{Optional logical indicating whether all or or only
a subset of fit statistics should be printed.}
  \item{\dots}{
Further objects to be passed.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with following entries
  \item{IC}{Data frame with information criteria}
  \item{LRtest}{Data frame with all (useful) pairwise
     likelihood ratio tests}
}


%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
The function is based on \code{\link{IRT.IC}}.

For comparing two models see \code{\link{anova.din}}.

For computing absolute model fit see \code{\link{IRT.modelfit}}. 
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Model comparison sim.dina dataset
#############################################################################	
	
data(sim.dina)
data(sim.qmatrix)
dat <- sim.dina
q.matrix <- sim.qmatrix

#*** Model 0: DINA model with equal guessing and slipping parameters
mod0 <- CDM::din( dat , q.matrix , guess.equal=TRUE , slip.equal = TRUE )
summary(mod0)

#*** Model 1: DINA model
mod1 <- CDM::din( dat , q.matrix )
summary(mod1)

#*** Model 2: DINO model
mod2 <- CDM::din( dat , q.matrix , rule="DINO")
summary(mod2)

#*** Model 3: Additive GDINA model
mod3 <- CDM::gdina( dat , q.matrix , rule="ACDM")
summary(mod3)

#*** Model 4: GDINA model
mod4 <- CDM::gdina( dat , q.matrix , rule="GDINA")
summary(mod4)

# model comparisons
res <- CDM::IRT.compareModels( mod0 , mod1 , mod2 , mod3 , mod4 )
res
  ##   > res
  ##   $IC
  ##     Model   loglike Deviance Npars Nobs      AIC      BIC     AIC3     AICc     CAIC
  ##   1  mod0 -2176.482 4352.963     9  400 4370.963 4406.886 4379.963 4371.425 4415.886
  ##   2  mod1 -2042.378 4084.756    25  400 4134.756 4234.543 4159.756 4138.232 4259.543
  ##   3  mod2 -2086.805 4173.610    25  400 4223.610 4323.396 4248.610 4227.086 4348.396
  ##   4  mod3 -2048.233 4096.466    32  400 4160.466 4288.193 4192.466 4166.221 4320.193
  ##   5  mod4 -2026.633 4053.266    41  400 4135.266 4298.917 4176.266 4144.887 4339.917
  ##   
# -> The DINA model (mod1) performed best in terms of AIC.  
  ##   $LRtest
  ##     Model1 Model2      Chi2 df            p
  ##   1   mod0   mod1 268.20713 16 0.000000e+00
  ##   2   mod0   mod2 179.35362 16 0.000000e+00
  ##   3   mod0   mod3 256.49745 23 0.000000e+00
  ##   4   mod0   mod4 299.69671 32 0.000000e+00
  ##   5   mod1   mod3 -11.70967  7 1.000000e+00
  ##   6   mod1   mod4  31.48959 16 1.164415e-02
  ##   7   mod2   mod3  77.14383  7 5.262457e-14
  ##   8   mod2   mod4 120.34309 16 0.000000e+00
  ##   9   mod3   mod4  43.19926  9 1.981445e-06
  ##   
# -> The GDINA model (mod4) was superior to the other models in terms
#    of the likelihood ratio test.

# get an overview with summary
summary(res)
summary(res,extended=FALSE)

#*******************
# applying model comparison for objects of class IRT.modelfit

# compute model fit statistics
fmod0 <- CDM::IRT.modelfit(mod0)
fmod1 <- CDM::IRT.modelfit(mod1)
fmod4 <- CDM::IRT.modelfit(mod4)

# model comparison
res <- CDM::IRT.compareModels( fmod0 , fmod1 ,  fmod4 )
res
  ##   $IC
  ##        Model   loglike Deviance Npars Nobs      AIC      BIC     AIC3
  ##   mod0  mod0 -2176.482 4352.963     9  400 4370.963 4406.886 4379.963
  ##   mod1  mod1 -2042.378 4084.756    25  400 4134.756 4234.543 4159.756
  ##   mod4  mod4 -2026.633 4053.266    41  400 4135.266 4298.917 4176.266
  ##            AICc     CAIC      maxX2   p_maxX2     MADcor      SRMSR
  ##   mod0 4371.425 4415.886 118.172707 0.0000000 0.09172287 0.10941300
  ##   mod1 4138.232 4259.543   8.728248 0.1127943 0.03025354 0.03979948
  ##   mod4 4144.887 4339.917   2.397241 1.0000000 0.02284029 0.02989669
  ##        X100.MADRESIDCOV      MADQ3     MADaQ3
  ##   mod0        1.9749936 0.08840892 0.08353917
  ##   mod1        0.6713952 0.06184332 0.05923058
  ##   mod4        0.5148707 0.07477337 0.07145600
  ##   
  ##   $LRtest
  ##     Model1 Model2      Chi2 df          p
  ##   1   mod0   mod1 268.20713 16 0.00000000
  ##   2   mod0   mod4 299.69671 32 0.00000000
  ##   3   mod1   mod4  31.48959 16 0.01164415
}	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Model comparison}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
