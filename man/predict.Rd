%% File Name: predict.Rd
%% File Version: 0.14

\name{predict}

\alias{IRT.predict}

\alias{predict.din}
\alias{predict.gdina}
\alias{predict.mcdina}
\alias{predict.gdm}
\alias{predict.slca}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Expected Values and Predicted Probabilities from Item Response Response Models
}


\description{
This function computes expected values for each person and each 
item based on the individual posterior distribution. The output
of this function can be the basis of creating item and
person fit statistics.
}


\usage{
IRT.predict(object, dat, group=1)

\method{predict}{din}(object, group=1, \dots)

\method{predict}{gdina}(object, group=1, \dots)

\method{predict}{mcdina}(object, group=1, \dots)

\method{predict}{gdm}(object, group=1, \dots)

\method{predict}{slca}(object, group=1, \dots)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
Object for the S3 methods \code{\link{IRT.irfprob}} and 
\code{\link{IRT.posterior}} are defined. In the \pkg{CDM} packages,
these are the objects of class \code{din}, \code{gdina}, \code{mcdina},
\code{slca} or \code{gdm}.
}
\item{dat}{
Dataset with item responses
}
\item{group}{Group index for use}
\item{\dots}{Further arguments to be passed.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with following entries
\item{expected}{Array with expected values (persons \eqn{\times} 
classes \eqn{\times} items)}
\item{probs.categ}{Array with expected probabilities for
  each category (persons \eqn{\times} categories \eqn{\times} 
classes \eqn{\times} items)}
\item{variance}{Array with variance in predicted values for each
  person and each item.}
\item{residuals}{Array with residuals for each person and each item}
\item{stand.resid}{Array with standardized residuals for each
person and each item}
}

%
%  res <- list( "expected" = pred , "probs.categ" = pred.categ , 
%   "variance" = var1 , "residuals" = resid1 , "stand.resid" = sresid1 )


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
# EXAMPLE 1: Fitted Rasch model in TAM package
#############################################################################

#--- Model 1: Rasch model
library(TAM)
mod1 <- TAM::tam.mml(resp=TAM::sim.rasch) 
# apply IRT.predict function
prmod1 <- CDM::IRT.predict(mod1, mod1$resp )
str(prmod1)
}

#############################################################################
# EXAMPLE 2: Predict function for din
#############################################################################

# DINA Model
mod1 <- CDM::din( CDM::sim.dina, q.matr = CDM::sim.qmatrix, rule = "DINA" )
summary(mod1)
# apply predict method
prmod1 <- CDM::IRT.predict( mod1, sim.dina )
str(prmod1)
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Predicted values}
\keyword{Residuals}% __ONLY ONE__ keyword per line
