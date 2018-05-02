%% File Name: data.jang.Rd
%% File Version: 0.14

\name{data.jang}
\alias{data.jang}
\docType{data}
\title{
Dataset Jang (2009)
}
\description{
Simulated dataset according to the Jang (2005) L2 reading comprehension
study.
}

\usage{
data(data.jang)
}


\format{
  The format is:

\code{List of 2} \cr
\code{ $ data    : num [1:1500, 1:37] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:37] "I1" "I2" "I3" "I4" ...} \cr
\code{ $ q.matrix:'data.frame':} \cr
\code{  ..$ CDV: int [1:37] 1 0 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ CIV: int [1:37] 0 0 1 0 0 0 1 0 1 1 ...} \cr
\code{  ..$ SSL: int [1:37] 1 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ TEI: int [1:37] 0 0 0 0 0 0 0 1 0 0 ...} \cr
\code{  ..$ TIM: int [1:37] 0 0 0 1 1 1 0 0 0 0 ...} \cr
\code{  ..$ INF: int [1:37] 0 1 0 0 0 0 1 0 0 0 ...} \cr
\code{  ..$ NEG: int [1:37] 0 0 0 0 1 0 1 0 0 0 ...} \cr
\code{  ..$ SUM: int [1:37] 0 0 0 0 1 0 0 0 0 0 ...} \cr
\code{  ..$ MCF: int [1:37] 0 0 0 0 0 0 0 0 0 0 ...} \cr

}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
Simulated dataset.
}


\references{
Jang, E. E. (2009). Cognitive diagnostic assessment of L2 reading comprehension
ability: Validity arguments for Fusion Model application to LanguEdge assessment.
\emph{Language Testing, 26}, 31-73.
}


\examples{
\dontrun{
data(data.jang, package="CDM")

data <- data.jang$data
q.matrix <- data.jang$q.matrix

#*** Model 1: Reduced RUM model
mod1 <- CDM::gdina( data, q.matrix, rule="RRUM", conv.crit = .001, increment.factor=1.025 )
summary(mod1)

#*** Model 2: Additive model (identity link)
mod2 <- CDM::gdina( data, q.matrix, rule="ACDM", conv.crit = .001, linkfct="identity" )
summary(mod2)

#*** Model 3: DINA model
mod3 <- CDM::gdina( data, q.matrix, rule="DINA", conv.crit = .001 )
summary(mod3)

anova(mod1,mod2)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   1 Model 1 -30315.03 60630.06   153 60936.06 61748.98 88.29627  0  0
  ##   2 Model 2 -30270.88 60541.76   153 60847.76 61660.68       NA NA NA
anova(mod1,mod3)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   2 Model 2 -30373.99 60747.97   129 61005.97 61691.38 117.9128 24  0
  ##   1 Model 1 -30315.03 60630.06   153 60936.06 61748.98       NA NA NA

# RRUM
summary( CDM::modelfit.cor.din( mod1 , jkunits=0) )
  ##          type    value       p
  ##   1   max(X2) 11.79073 0.39645
  ##   2 abs(fcor)  0.09541 0.07422
  ##                       est
  ##   MADcor          0.01834
  ##   SRMSR           0.02300
  ##   MX2             0.86718
  ##   100*MADRESIDCOV 0.38690
  ##   MADQ3           0.02413

# additive model (identity)
summary( CDM::modelfit.cor.din( mod2 , jkunits=0) )
  ##          type   value       p
  ##   1   max(X2) 9.78958 1.00000
  ##   2 abs(fcor) 0.08770 0.22993
  ##                       est
  ##   MADcor          0.01721
  ##   SRMSR           0.02158
  ##   MX2             0.69163
  ##   100*MADRESIDCOV 0.36343
  ##   MADQ3           0.02423

# DINA model
summary( CDM::modelfit.cor.din( mod3 , jkunits=0) )
  ##          type    value       p
  ##   1   max(X2) 13.48449 0.16020
  ##   2 abs(fcor)  0.10651 0.01256
  ##                       est
  ##   MADcor          0.01999
  ##   SRMSR           0.02495
  ##   MX2             0.92820
  ##   100*MADRESIDCOV 0.42226
  ##   MADQ3           0.02258
}
}
\keyword{datasets}
