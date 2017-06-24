\name{IRT.itemfit}
\alias{IRT.itemfit}
\alias{IRT.itemfit.din}
\alias{IRT.itemfit.gdina}
\alias{IRT.itemfit.gdm}
\alias{IRT.itemfit.slca}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
S3 Methods for Computing Item Fit
}

\description{
This S3 method computes some selected item fit statistic.
}

\usage{
IRT.itemfit(object, \dots)

\method{IRT.itemfit}{din}(object, method="RMSEA", \dots)

\method{IRT.itemfit}{gdina}(object, method="RMSEA", \dots)

\method{IRT.itemfit}{gdm}(object, method="RMSEA", \dots)

\method{IRT.itemfit}{slca}(object, method="RMSEA", \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of classes \code{\link{din}}, \code{\link{gdina}},
\code{\link{gdm}} or \code{\link{slca}}.
}
\item{method}{Method for computing item fit statistic. Until now,
	only \code{method="RMSEA"} (see \code{\link{itemfit.rmsea}})
	can be used.
		}	
  \item{\dots}{
More arguments to be passed.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Vector or data frame with item fit statistics.
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
For extracting the individual likelihood or posterior see
\code{\link{IRT.likelihood}} or \code{\link{IRT.posterior}}.
}
%%% TAM, sirt and mirt links

\examples{
\dontrun{	
#############################################################################
# EXAMPLE 1: DINA model item fit
#############################################################################

data(sim.dina)
data(sim.qmatrix)

# estimate model
mod1 <- CDM::din(sim.dina, sim.qmatrix)
# compute item fit
IRT.itemfit( mod1 )
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Item fit}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
