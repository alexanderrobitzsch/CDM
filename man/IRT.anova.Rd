%% File Name: IRT.anova.Rd
%% File Version: 0.03

\name{IRT.anova}
\alias{IRT.anova}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Helper Function for Conducting Likelihood Ratio Tests
}

\description{
This is a helper function for conducting likelihood ratio tests 
and can be generally used for objects for which the 
\code{\link[stats:logLik]{logLik}} method is defined.
}

\usage{
IRT.anova(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object for which the \code{\link[stats:logLik]{logLik}}
method is defined.
}
  \item{\dots}{
A further object to be passed
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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
See also \code{\link{IRT.compareModels}} for model comparisons
of several models.

See also as \code{\link{anova.din}}.
}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{anova}
\keyword{Likelihood ratio test}% __ONLY ONE__ keyword per line
