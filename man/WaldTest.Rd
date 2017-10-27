%% File Name: WaldTest.Rd
%% File Version: 0.07

\name{WaldTest}
\alias{WaldTest}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wald Test for a Linear Hypothesis
}

\description{
Computes a Wald Test for a parameter \eqn{\bold{\theta}}
with respect to a linear hypothesis 
\eqn{ \bold{R} \bold{\theta} = \bold{c}}.
}

\usage{
WaldTest( delta , vcov , R , nobs , cvec = NULL , eps=1E-10 )
}


%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{delta}{
Estimated parameter
}
\item{vcov}{Estimated covariance matrix}
\item{R}{Hypothesis matrix}
\item{nobs}{Number of observations}
\item{cvec}{Hypothesis vector}
\item{eps}{Numerical value is added as ridge parameter of
the covariance matrix}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A vector containing the \eqn{\chi^2} statistic (\code{X2}), 
degrees of freedom (\code{df}),
p value (\code{p}) and RMSEA statistic (\code{RMSEA}).
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


%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Wald test}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
