%% File Name: IRT.likelihood.Rd
%% File Version: 0.17

\name{IRT.likelihood}
\alias{IRT.likelihood}
\alias{IRT.posterior}
\alias{IRT.likelihood.din}
\alias{IRT.likelihood.gdina}
\alias{IRT.likelihood.mcdina}
\alias{IRT.likelihood.gdm}
\alias{IRT.likelihood.slca}
\alias{IRT.likelihood.reglca}
\alias{IRT.posterior.din}
\alias{IRT.posterior.gdina}
\alias{IRT.posterior.mcdina}
\alias{IRT.posterior.gdm}
\alias{IRT.posterior.slca}
\alias{IRT.posterior.reglca}


%- Also NEED an '\alias' for EACH other topic documented here.

\title{
S3 Methods for Extracting of the Individual Likelihood and the Individual Posterior 
}

\description{
Functions for extracting the individual likelihood and
individual posterior distribution.
}

\usage{
IRT.likelihood(object, \dots)

IRT.posterior(object, \dots)

\method{IRT.likelihood}{din}(object, \dots)
\method{IRT.posterior}{din}(object, \dots)

\method{IRT.likelihood}{gdina}(object, \dots)
\method{IRT.posterior}{gdina}(object, \dots)

\method{IRT.likelihood}{gdm}(object, \dots)
\method{IRT.posterior}{gdm}(object, \dots)

\method{IRT.likelihood}{mcdina}(object, \dots)
\method{IRT.posterior}{mcdina}(object, \dots)

\method{IRT.likelihood}{reglca}(object, \dots)
\method{IRT.posterior}{reglca}(object, \dots)

\method{IRT.likelihood}{slca}(object, \dots)
\method{IRT.posterior}{slca}(object, \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of classes \code{\link{din}}, \code{\link{gdina}},
\code{\link{mcdina}}, \code{\link{gdm}}, \code{\link{slca}},
\code{\link{reglca}}.
}
  \item{\dots}{
More arguments to be passed.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
For both functions \code{IRT.likelihood} and \code{IRT.posterior},
it is a matrix with attributes
  \item{theta}{Uni- or multidimensional skill space (theta grid in
  item response models).}
  \item{prob.theta}{Probability distribution of \code{theta}}
  \item{skillspace}{Design matrix and estimated parameters for
  skill space distribution (only for \code{IRT.posterior.slca})}
  \item{G}{Number of groups}
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
\code{\link[GDINA:indlogLik]{GDINA::indlogLik}},
\code{\link[GDINA:indlogPost]{GDINA::indlogPost}}
}
%%% TAM, sirt and mirt links

\examples{
#############################################################################
# EXAMPLE 1: Extracting likelihood and posterior from a DINA model
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

#*** estimate model
mod1 <- CDM::din( sim.dina, q.matrix=sim.qmatrix, rule="DINA")
#*** extract likelihood
likemod1 <- CDM::IRT.likelihood(mod1)
str(likemod1)
# extract theta
attr(likemod1, "theta" )
#*** extract posterior
pomod1 <- CDM::IRT.posterior( mod1 )
str(pomod1)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Individual likelihood}
\keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
