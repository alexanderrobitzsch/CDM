%% File Name: IRT.IC.Rd
%% File Version: 0.11

\name{IRT.IC}
\alias{IRT.IC}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Information Criteria
}

\description{
Computes several information criteria for objects which do have
the \code{\link[stats:logLik]{logLik}} (\pkg{stats}) S3 method
(e.g. \code{\link{din}}, \code{\link{gdina}}, \code{\link{gdm}}, ...) .
}

\usage{
IRT.IC(object)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Objects which do have the \code{\link[stats:logLik]{logLik}} (\pkg{stats})
S3 method.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector with deviance and several information criteria.
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
See also \code{\link{anova.din}} for model comparisons.
A general method is defined in \code{\link{IRT.compareModels}}.
}


\examples{
#############################################################################
# EXAMPLE 1: DINA example information criteria
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

#*** Model 1: DINA model
mod1 <- CDM::din( sim.dina, q.matrix=sim.qmatrix )
summary(mod1)
IRT.IC(mod1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Information criteria}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
