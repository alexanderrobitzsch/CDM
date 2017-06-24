\name{deltaMethod}
\alias{deltaMethod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variance Matrix of a Nonlinear Estimator Using the Delta Method
}
\description{
Computes the variance of a nonlinear parameter using the
delta method.
}
\usage{
deltaMethod(derived.pars, est, Sigma, h = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{derived.pars}{
Vector of derived parameters written in \R formula framework
(see Examples).
}
  \item{est}{
Vector of parameter estimates
}
  \item{Sigma}{
Covariance matrrix of parameters
}
  \item{h}{
Numerical differentiation parameter
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
 \item{coef}{Vector of nonlinear parameters}
 \item{vcov}{Covariance matrix of nonlinear parameters}
  \item{se}{Vector of standard errors}
  \item{A}{First derivative of nonlinear transformation}
  \item{univarTest}{Data frame containing univariate summary of
  nonlinear parameters}
  \item{WaldTest}{Multivariate parameter test for nonlinear paramter}
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
See \code{\link[car:deltaMethod]{car::deltaMethod}} or
\code{msm::deltamethod}.
}

%% \code{\link{msm::deltamethod}}.

\examples{
#############################################################################
# EXAMPLE 1: Nonlinear parameter
#############################################################################	

#-- parameter estimate
est <- c( 510.67 , 102.57)
names(est) <- c("mu" , "sigma")
#-- covariance matrix
Sigma <- matrix( c(5.83 , 0.45 , 0.45 , 3.21 ) , nrow=2 , ncol=2 )
colnames(Sigma) <- rownames(Sigma) <- names(est)
#-- define derived nonlinear parameters
derived.pars <- list( "d" = ~ I( ( mu - 508 ) / sigma ) ,
                      "dsig" = ~ I( sigma / 100 - 1) )

#*** apply delta method
res <- CDM::deltaMethod( derived.pars , est , Sigma )	
res
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
