%% File Name: IRT.expectedCounts.Rd
%% File Version: 0.13

\name{IRT.expectedCounts}
\alias{IRT.expectedCounts}
\alias{IRT.expectedCounts.din}
\alias{IRT.expectedCounts.gdina}
\alias{IRT.expectedCounts.mcdina}
\alias{IRT.expectedCounts.gdm}
\alias{IRT.expectedCounts.slca}
\alias{IRT.expectedCounts.reglca}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S3 Method for Extracting Expected Counts
}

\description{
This S3 method extracts expected counts from model output.
}

\usage{
IRT.expectedCounts(object, ...)

\method{IRT.expectedCounts}{din}(object, \dots)

\method{IRT.expectedCounts}{gdina}(object, \dots)

\method{IRT.expectedCounts}{gdm}(object, \dots)

\method{IRT.expectedCounts}{mcdina}(object, \dots)

\method{IRT.expectedCounts}{slca}(object, \dots)

\method{IRT.expectedCounts}{reglca}(object, \dots)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of classes \code{\link{din}}, \code{\link{gdina}},
\code{\link{mcdina}}, \code{\link{gdm}} or \code{\link{slca}}.
}
  \item{\dots}{
More arguments to be passed.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
An array with expected counts. The dimensions are items,
categories, latent classes and groups.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Expected counts gdm function
#############################################################################

data(data.fraction1, package="CDM")
dat <- data.fraction1$data
theta.k <- seq( -6, 6, len=11 )   # discretized ability

#--- Model 1: Rasch model
mod1 <- CDM::gdm( dat, irtmodel="1PL", theta.k=theta.k, skillspace="normal",
               centered.latent=TRUE )
emod1 <- CDM::IRT.expectedCounts(mod1)
str(emod1)

#############################################################################
# EXAMPLE 2: Expected counts gdina function
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

#--- Model 1: estimation of the GDINA model
mod1 <- CDM::gdina( data=sim.dina, q.matrix=sim.qmatrix)
summary(mod1)
emod1 <- CDM::IRT.expectedCounts(mod1)
str(emod1)

#--- Model 2: GDINA model with two groups
mod2 <- CDM::gdina( data=CDM::sim.dina, q.matrix=CDM::sim.qmatrix,
                   group=rep(1:2, each=200) )
summary(mod2)
emod2 <- CDM::IRT.expectedCounts( mod2 )
str(emod2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Expected counts}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
