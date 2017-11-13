%% File Name: skillspace.approximation.Rd
%% File Version: 0.08

\name{skillspace.approximation}
\alias{skillspace.approximation}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Skill Space Approximation
}

\description{
This function appoximates the skill space with \eqn{K} skills to
approximate a (typically high-dimensional) skill space of \eqn{2^K} classes by
\eqn{L} classes \eqn{(L < 2^K)}. 
}
\usage{
skillspace.approximation(L, K, nmax = 5000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L}{
Number of skill classes used for approximation
}
  \item{K}{
Number of skills
}
  \item{nmax}{
Number of quasi-randomly generated skill classes using the \code{QUnif}
function in \pkg{sfsmisc}
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A matrix containing skill classes in rows
}

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}

\note{
This function uses the \code{QUnif} function from the \pkg{sfsmisc}
package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{gdina}} (Example 9).
}

\examples{
#############################################################################
# EXAMPLE 1: Approximate a skill space of K=8 eight skills by 20 classes
#############################################################################

# => 2^8 = 256 latent classes if all latent classes would be used
CDM::skillspace.approximation( L=20  , K=8 )
  ##             [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
  ##   P00000000    0    0    0    0    0    0    0    0
  ##   P00000001    0    0    0    0    0    0    0    1
  ##   P00001011    0    0    0    0    1    0    1    1
  ##   P00010011    0    0    0    1    0    0    1    1
  ##   P00101001    0    0    1    0    1    0    0    1
  ##   [...]
  ##   P11011110    1    1    0    1    1    1    1    0
  ##   P11100110    1    1    1    0    0    1    1    0
  ##   P11111111    1    1    1    1    1    1    1    1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Skill space approximation}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
