%% File Name: IRT.frequencies.Rd
%% File Version: 0.15

\name{IRT.frequencies}
\alias{IRT.frequencies}
\alias{IRT_frequencies_default}
\alias{IRT_frequencies_wrapper}
\alias{IRT.frequencies.din}
\alias{IRT.frequencies.gdina}
\alias{IRT.frequencies.slca}
\alias{IRT.frequencies.mcdina}
\alias{IRT.frequencies.gdm}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S3 Method for Computing Observed and Expected Frequencies of Univariate and
Bivariate Marginals
}

\description{
This S3 method computes observed and expected frequencies for univariate and
bivariate distributions.
}

\usage{
IRT.frequencies(object, \dots)

IRT_frequencies_default(data, post, probs, weights=NULL)

IRT_frequencies_wrapper(object, \dots)

\method{IRT.frequencies}{din}(object, \dots)

\method{IRT.frequencies}{gdina}(object, \dots)

\method{IRT.frequencies}{mcdina}(object, \dots)

\method{IRT.frequencies}{gdm}(object, \dots)

\method{IRT.frequencies}{slca}(object, \dots)
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
\item{data}{Item response data as extracted by \code{\link{IRT.data}}}
\item{post}{Individual posterior distribution as extracted by \code{\link{IRT.posterior}}}
\item{probs}{Individual posterior distribution as extracted by \code{\link{IRT.irfprob}}}
\item{weights}{Optional vector of weights as included as the attribute \code{weights}
in \code{\link{IRT.data}}}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List with following entries

\item{uni_obs}{Univariate observed distribution}
\item{uni_exp}{Univariate expected distribution}
\item{M_obs}{Univariate observed means}
\item{M_exp}{Univariate expected means}
\item{SD_obs}{Univariate observed standard deviations}
\item{SD_exp}{Univariate expected standard deviations}
\item{biv_obs}{Bivariate observed frequencies}
\item{biv_exp}{Bivariate expected frequencies}
\item{biv_N}{Bivariate sample size}
\item{cov_obs}{Observed covariances}
\item{cov_cor}{Expected covariances}
\item{cor_obs}{Observed correlations}
\item{cor_exp}{Expected correlations}
\item{chisq}{Chi square statistic of local independence}
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
# EXAMPLE 1: Usage IRT.frequencies
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

# estimate GDINA model
mod1 <- CDM::gdina( data=sim.dina,  q.matrix=sim.qmatrix)
summary(mod1)

# direct usage of IRT.frequencies
fres1 <- CDM::IRT.frequencies(mod1)

# use of the default function with input data
data <- CDM::IRT.data(object)
post <- CDM::IRT.posterior(object)
probs <- CDM::IRT.irfprob(object)
fres2 <- CDM::IRT_frequencies_default(data=data, post=post, probs=probs)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{Expected counts}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
