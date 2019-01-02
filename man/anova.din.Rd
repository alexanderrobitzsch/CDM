%% File Name: anova.din.Rd
%% File Version: 1.27

\name{anova}
\alias{anova.din}
\alias{anova.gdina}
\alias{anova.mcdina}
\alias{anova.gdm}
\alias{anova.slca}
\alias{anova.reglca}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Likelihood Ratio Test for Model Comparisons
}
\description{
This function compares two models estimated with \code{din}, \code{gdina}
or \code{gdm} using a likelihood ratio test.
}
\usage{
\method{anova}{din}(object,\dots)

\method{anova}{gdina}(object,\dots)

\method{anova}{gdm}(object,\dots)

\method{anova}{mcdina}(object,\dots)

\method{anova}{reglca}(object,\dots)

\method{anova}{slca}(object,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Two objects of class \code{din}, \code{gdina}, \code{mcdina},
\code{slca}, \code{gdm}, \code{reglca}
}
  \item{\dots}{
Further arguments to be passed
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
%Alexander Robitzsch
%}
\note{
This function is based on \code{\link{IRT.anova}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    \code{\link{din}}, \code{\link{gdina}}, \code{\link{gdm}},
    \code{\link{mcdina}}, \code{\link{slca}}
}
\examples{
#############################################################################
# EXAMPLE 1: anova with din objects
#############################################################################

# Model 1
d1 <- CDM::din(sim.dina, q.matr=sim.qmatrix )
# Model 2 with equal guessing and slipping parameters
d2 <- CDM::din(sim.dina, q.matr=sim.qmatrix, guess.equal=TRUE, slip.equal=TRUE)
# model comparison
anova(d1,d2)
  ##     Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   2    d2 -2176.482 4352.963     9 4370.963 4406.886 268.2071 16  0
  ##   1    d1 -2042.378 4084.756    25 4134.756 4234.543       NA NA NA

\dontrun{
#############################################################################
# EXAMPLE 2: anova with gdina objects
#############################################################################

# Model 3: GDINA model
d3 <- CDM::gdina( sim.dina, q.matr=sim.qmatrix )

# Model 4: DINA model
d4 <- CDM::gdina( sim.dina, q.matr=sim.qmatrix, rule="DINA")

# model comparison
anova(d3,d4)
  ##     Model   loglike Deviance Npars      AIC      BIC    Chisq df       p
  ##   2    d4 -2042.293 4084.586    25 4134.586 4234.373 31.31995 16 0.01224
  ##   1    d3 -2026.633 4053.267    41 4135.266 4298.917       NA NA      NA
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Likelihood ratio test}
%% \keyword{anova}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
