\name{coef}
\alias{coef.din}
\alias{coef.gdina}
\alias{coef.mcdina}
\alias{coef.gdm}
\alias{coef.slca}


\title{Extract Estimated Item Parameters and Skill Class Distribution
Parameters}
\description{
  Extracts the estimated parameters from either 
  \code{din}, \code{gdina},  \code{gdina} or \code{gdm} objects.
}
\usage{
\method{coef}{din}(object, \dots)

\method{coef}{gdina}(object, \dots)

\method{coef}{mcdina}(object, \dots)

\method{coef}{gdm}(object, \dots)

\method{coef}{slca}(object, \dots)
}

\arguments{
  \item{object}{An object inheriting from either class \code{din}, 
  class \code{gdina}, class \code{mcdina}, class \code{slca}
  or class \code{gdm}. 
  			}
  \item{\dots}{Additional arguments to be passed.}
}

%%\details{
%%%% nothing yet
%%}

\value{
   A vector, a matrix or a data frame of the estimated parameters for the fitted model.
	}

%%\references{
%% include something here
%%}

%%\author{
%% CDM authors
%%}

\seealso{
	\code{\link{din}}, \code{\link{gdina}}, \code{\link{gdm}},
	\code{\link{mcdina}}, \code{\link{slca}}
}

\examples{
# use sim.dina dataset
data(sim.dina)

# DINA model
d1 <- CDM::din(sim.dina, q.matr = sim.qmatrix)
coef(d1)

\dontrun{
# GDINA model
d2 <- CDM::gdina(sim.dina, q.matr = sim.qmatrix)
coef(d2)

# GDM model (use only 10 iterations for computation time reasons)
theta.k <- seq(-4,4,len=11)
d3 <- CDM::gdm( sim.dina, irtmodel="2PL", theta.k=theta.k, Qmatrix=as.matrix(sim.qmatrix), 
             centered.latent=TRUE , maxiter=10 )
coef(d3) 
}
}

\keyword{methods}
\keyword{coef}
