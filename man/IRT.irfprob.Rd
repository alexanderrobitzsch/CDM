%% File Name: IRT.irfprob.Rd
%% File Version: 0.171

\name{IRT.irfprob}
\alias{IRT.irfprob}
\alias{IRT.irfprob.din}
\alias{IRT.irfprob.gdina}
\alias{IRT.irfprob.mcdina}
\alias{IRT.irfprob.gdm}
\alias{IRT.irfprob.slca}
\alias{IRT.irfprob.reglca}


%- Also NEED an '\alias' for EACH other topic documented here.

\title{
S3 Methods for Extracting Item Response Functions
}

\description{
This S3 method extracts item response functions evaluated
at a grid of abilities (skills). Item response functions can
be plotted using the \code{\link{IRT.irfprobPlot}} function.
}

\usage{
IRT.irfprob(object, \dots)

\method{IRT.irfprob}{din}(object, \dots)

\method{IRT.irfprob}{gdina}(object, \dots)

\method{IRT.irfprob}{gdm}(object, \dots)

\method{IRT.irfprob}{mcdina}(object, \dots)

\method{IRT.irfprob}{reglca}(object, \dots)

\method{IRT.irfprob}{slca}(object, \dots)
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
An array with item response probabilities (items \eqn{\times}
categories \eqn{\times} skill classes [\eqn{\times} group]) and attributes
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
Plot functions for item response curves: \code{\link{IRT.irfprobPlot}}.

For extracting the individual likelihood or posterior see
\code{\link{IRT.likelihood}} or \code{\link{IRT.posterior}}.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Extracting item response functions mcdina model
#############################################################################

data(data.cdm02, package="CDM")

dat <- data.cdm02$data
q.matrix <- data.cdm02$q.matrix

#-- estimate model
mod1 <- CDM::mcdina( dat, q.matrix=q.matrix)
#-- extract item response functions
prmod1 <- CDM::IRT.irfprob(mod1)
str(prmod1)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{Individual likelihood}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
