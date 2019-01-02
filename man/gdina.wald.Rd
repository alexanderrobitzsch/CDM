%% File Name: gdina.wald.Rd
%% File Version: 0.34

\name{gdina.wald}
\alias{gdina.wald}
\alias{summary.gdina.wald}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wald Statistic for Item Fit of the DINA and ACDM Rule for GDINA Model
}
\description{
This function tests with a Wald test for the GDINA model whether a DINA or a ACDM
condensation rule leads to a sufficient item fit compared
to the saturated GDINA rule (de la Torre & Lee, 2013). The Wald test
is accompanied by the RMSEA fit and weighted and unweighted
distance measures (\code{wgtdist}, \code{uwgtdist}), see Details
(compare Ma, Iaconangelo, & de la Torre, 2016).
}
\usage{
gdina.wald(object)

\method{summary}{gdina.wald}(object, digits=3,
    vars=c("X2", "p", "sig", "RMSEA", "wgtdist"),  \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A fitted \code{\link{gdina}} model
}
\item{digits}{Number of digits after decimal used
   for rounding.}
\item{vars}{Vector including variables which should
  be displayed in \code{summary}. See the output \code{stats}.
}
\item{\dots}{Further arguments to be passed}
}

\details{
Let \eqn{P_j( \alpha _l)} the estimated item response function for the
GDINA model and \eqn{\hat{P}_j( \alpha _l)} the item response
model for the approximated model (DINA, DINO or ACDM).
The unweighted distance \code{uwgtdist} as a measure of misfit is defined as
\deqn{uwgtdist=\frac{1}{2^K} \sum_l ( P_j( \alpha _l) - \hat{P}_j( \alpha _l) )^2}
The weighted distance    \code{wgtdist} measures the discrepancy
with respected to the probabilities \eqn{w_l=P( \alpha_l)} of estimated
skill classes
\deqn{wgtdist=\sum_l w_l (P_j( \alpha _l) - \hat{P}_j( \alpha _l) )^2}
}

\value{
  \item{stats}{Data frame with Wald statistic for every item,
  corresponding p values and a RMSEA fit statistic}
}

\references{
de la Torre, J., & Lee, Y. S. (2013). Evaluating the Wald test for
item-level comparison of saturated and reduced models in cognitive diagnosis.
\emph{Journal of Educational Measurement, 50}, 355-373.

Ma, W., Iaconangelo, C., & de la Torre, J. (2016). Model similarity,
model selection, and attribute classification.
\emph{Applied Psychological Measurement, 40}(3), 200-217.
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the \code{\link[GDINA:modelcomp]{GDINA::modelcomp}} function in the
\pkg{GDINA} package for similar functionality.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Wald test for DINA simulated data sim.dina
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

# Model 1: estimate GDINA model
mod1 <- CDM::gdina( sim.dina, q.matrix=sim.qmatrix,  rule="GDINA")
summary(mod1)

# perform Wald test
res1 <- CDM::gdina.wald( mod1 )
summary(res1)
# -> results show that all but one item fit according to the DINA rule

# select some output
summary(res1, vars=c("wgtdist", "p") )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Wald test}
%% \keyword{GDINA model}% __ONLY ONE__ keyword per line
