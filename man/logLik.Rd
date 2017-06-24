\name{logLik}
\alias{logLik.din}
\alias{logLik.gdina}
\alias{logLik.mcdina}
\alias{logLik.gdm}
\alias{logLik.slca}

\title{Extract Log-Likelihood}
\description{
  Extracts the log-likelihood from either \code{din}, \code{gdina},
  \code{mcdina}, \code{slca} or \code{gdm} objects.
}
\usage{
\method{logLik}{din}(object, \dots)

\method{logLik}{gdina}(object, \dots)

\method{logLik}{mcdina}(object, \dots)

\method{logLik}{gdm}(object, \dots)

\method{logLik}{slca}(object, \dots)
}

\arguments{
  \item{object}{ An object inheriting from either class \code{din}, 
  class \code{gdina}, \code{slca} or class \code{gdm}. 
  	}
  \item{\dots}{Additional arguments}
}

\seealso{
	\code{\link{din}}, \code{\link{gdina}}, \code{\link{gdm}},
	\code{\link{mcdina}}, \code{\link{slca}}
}

\examples{
# logLik method | DINA model
data(sim.dina)
data(sim.qmatrix)
d1 <- CDM::din(sim.dina, q.matr = sim.qmatrix, rule = "DINA")
summary(d1)
lld1 <- logLik(d1)
  ##   > lld1
  ##   'log Lik.' -2042.378 (df=25)
  ##   > attr(lld1,"df")
  ##   [1] 25
  ##   > attr(lld1,"nobs")
  ##   [1] 400	
nobs(lld1)  

# AIC and BIC
AIC(lld1)
BIC(lld1)
}	

\keyword{methods}
\keyword{logLik}

