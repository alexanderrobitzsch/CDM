%% File Name: itemfit.rmsea.Rd
%% File Version: 1.15

\name{itemfit.rmsea}
\alias{itemfit.rmsea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RMSEA Item Fit
}
\description{
This function estimates a chi squared based measure of item fit
in cognitive diagnosis models similar to the RMSEA itemfit
implemented in mdltm (von Davier, 2005;
cited in Kunina-Habenicht, Rupp & Wilhelm, 2009).

The RMSEA statistic is also called as the RMSD statistic, see
\code{\link{IRT.RMSD}}.
}
\usage{
itemfit.rmsea(n.ik, pi.k, probs, itemnames=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.ik}{
An array of four dimensions: Classes x items x categories x groups
}
  \item{pi.k}{
An array of two dimensions: Classes x groups
}
  \item{probs}{
An array of three dimensions: Classes x items x categories
}
  \item{itemnames}{
An optional vector of item names. Default is \code{NULL}.
}
}
\details{
For item \eqn{j}, the RMSEA itemfit in this function is calculated
as follows:
\deqn{ RMSEA_j=\sqrt{ \sum_k \sum_c \pi ( \bold{\theta}_c)
    \left( P_j ( \bold{\theta}_c ) -
\frac{n_{jkc}}{N_{jc}} \right)^2 } }
where \eqn{c} denotes the class of the skill vector
\eqn{\bold{\theta}}, \eqn{k} is the item category,
\eqn{\pi ( \bold{\theta}_c)} is the estimated class probability
of \eqn{\bold{\theta}_c},
\eqn{P_j} is the estimated item response function,
\eqn{n_{jkc}} is the expected number of students with
skill \eqn{\bold{\theta}_c} on
item \eqn{j} in category \eqn{k} and
\eqn{N_{jc}} is the expected number of students with
skill \eqn{\bold{\theta}_c} on
item \eqn{j}.
}
\value{
A list with two entries:
\item{rmsea}{Vector of RMSEA item statistics}
\item{rmsea.groups}{Matrix of group-wise RMSEA item statistics}
}
\references{
Kunina-Habenicht, O., Rupp, A. A., & Wilhelm, O. (2009).
A practical illustration of multidimensional diagnostic skills profiling:
Comparing results from confirmatory factor analysis and diagnostic
classification models.
\emph{Studies in Educational Evaluation, 35}, 64--70.

von Davier, M. (2005). \emph{A general diagnostic model applied to language
testing data}. ETS Research Report RR-05-16. ETS, Princeton, NJ: ETS.
}
%\author{
%Alexander Robitzsch
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
This function is used in \code{\link{din}}, \code{\link{gdina}} and
\code{\link{gdm}}.
}
%\examples{
%%%
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Item fit}
%% \keyword{RMSEA}
