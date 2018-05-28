%% File Name: IRT.classify.Rd
%% File Version: 0.07

\name{IRT.classify}
\alias{IRT.classify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Individual Classification for Fitted Models
}
\description{
Computes individual classifications based on a fitted model.
}
\usage{
IRT.classify(object, type="MLE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Fitted model for which methods \code{\link{IRT.likelihood}} and
\code{\link{IRT.posterior}} are defined.
}
\item{type}{
Type of classification: \code{"MLE"} (maximum likelihood estimate) or
\code{"MAP"} (maximum of posterior distribution)
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
List with entries

\item{class_theta}{Individual classification}
\item{class_index}{Class index of individual classification}
\item{class_maxval}{Maximum value corresponding to individual classification}
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
See \code{\link{IRT.factor.scores}} for similar functionality.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Individual classification data.ecpe
#############################################################################

data(data.ecpe, package="CDM")
dat <- data.ecpe$dat[,-1]
Q <- data.ecpe$q.matrix

#** estimate GDINA model
mod <- CDM::gdina(dat, q.matrix=Q)
summary(mod)

#** classify individuals
cmod <- CDM::IRT.classify(mod)
str(cmod)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
