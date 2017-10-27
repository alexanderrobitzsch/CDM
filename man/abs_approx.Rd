%% File Name: abs_approx.Rd
%% File Version: 0.03

\name{abs_approx}
\alias{abs_approx}
\alias{abs_approx_D1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Differentiable Aproximation of the Absolute Value Function
}
\description{
Approximation of absolute value function which is defined
as \eqn{|x| \approx \sqrt{ x^2 + \varepsilon}}.
}
\usage{
abs_approx(x, eps = 1e-05)

abs_approx_D1(x, eps = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector
}
  \item{eps}{
Constant \eqn{\varepsilon} used for approximation
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
\code{\link[base:abs]{base::abs}}
}
\examples{
#############################################################################
# EXAMPLE 1: Approximation abs function
#############################################################################	

x <- seq( -1 , 1 , len=101 )
#*** plot abs function and its approximation
graphics::plot( x , base::abs(x) , type="l" , lwd=2)
for (vv in 2:4){
    graphics::lines( x , CDM::abs_approx(x, eps=10^(-vv) ) , lty = vv , col=vv , lwd=2)
}
#*** plot derivative of approximation of abs function
graphics::plot( x , x , type="n" , ylab= "Derivative abs" , ylim = c(-1,1) )
for (vv in 2:4){
    graphics::lines( x , CDM::abs_approx_D1(x, eps=10^(-vv) ) , lty = vv , col=vv , lwd=2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
