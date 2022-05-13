%% File Name: IRT.factor.scores.Rd
%% File Version: 0.12

\name{IRT.factor.scores}
\alias{IRT.factor.scores}
\alias{IRT.factor.scores.din}
\alias{IRT.factor.scores.gdina}
\alias{IRT.factor.scores.mcdina}
\alias{IRT.factor.scores.gdm}
\alias{IRT.factor.scores.slca}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
S3 Methods for Extracting Factor Scores (Person Classifications)
}

\description{
This S3 method extracts factor scores or skill classifications.
}

\usage{
IRT.factor.scores(object, \dots)

\method{IRT.factor.scores}{din}(object, type="MLE", \dots)

\method{IRT.factor.scores}{gdina}(object, type="MLE", \dots)

\method{IRT.factor.scores}{mcdina}(object, type="MLE", \dots)

\method{IRT.factor.scores}{gdm}(object, type="EAP", \dots)

\method{IRT.factor.scores}{slca}(object, type="MLE", \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of classes \code{\link{din}}, \code{\link{gdina}},
\code{\link{mcdina}}, \code{\link{gdm}} or \code{\link{slca}}.
}
\item{type}{Type of estimated factor score. This can be
    \code{"MLE"}, \code{"MAP"} or \code{"EAP"}.
    The type \code{EAP} cannot be used for objects of class \code{slca}.
        }
  \item{\dots}{
More arguments to be passed.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A matrix or a vector with classified scores.
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
For extracting the individual likelihood or the individual posterior see
\code{\link{IRT.likelihood}} or \code{\link{IRT.posterior}}.
}


\examples{
#############################################################################
# EXAMPLE 1: Extracting factor scores in the DINA model
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

# estimate DINA model
mod1 <- CDM::din( sim.dina, q.matrix=sim.qmatrix)
summary(mod1)
# MLE
fsc1a <- CDM::IRT.factor.scores(mod1)
# MAP
fsc1b <- CDM::IRT.factor.scores(mod1, type="MAP")
# EAP
fsc1c <- CDM::IRT.factor.scores(mod1, type="EAP")
# compare classification for skill 1
stats::xtabs( ~ fsc1a[,1] + fsc1b[,1] )
graphics::boxplot( fsc1c[,1] ~ fsc1a[,1] )
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Factor scores}
% \keyword{IRT.factor.scores}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
