%% File Name: numerical_Hessian.Rd
%% File Version: 0.26

\name{numerical_Hessian}
\alias{numerical_Hessian}
\alias{numerical_Hessian_partial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Numerical Computation of the Hessian Matrix
}
\description{
Computes numerically the Hessian matrix of a given function for
all coordinates (\code{numerical_Hessian}) or for a selected
direction (\code{numerical_Hessian_partial}).
}

\usage{
numerical_Hessian(par, FUN, h = 1e-05, gradient = FALSE,
       hessian = TRUE, diag_only = FALSE, ...)

numerical_Hessian_partial(par , FUN , h = 1e-05, coordinate = 1, ... )
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{
Parameter vector
}
  \item{FUN}{
Specified function with argument vector \code{x}
}
  \item{h}{
Numerical differentiation parameter. Can be also a vector.
The increment in the numerical approximation of the derivative is
defined as \eqn{h_i \max ( 1 , \theta_i)} where \eqn{\theta_i}
  denotes the \eqn{i}th parameter.
}
  \item{gradient}{
Logical indicating whether the gradient should be calculated.
}
\item{hessian}{Logical indicating whether the Hessian
matrix should be calculated.}
\item{diag_only}{Logical indicating whether only the diagonal of the
hessian should be computed.}
\item{\dots}{Further arguments to be passed to \code{FUN}.}
\item{coordinate}{Coordinate index for partial derivative}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Gradient vector or Hessian matrix or a list of both elements
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
See the \pkg{numDeriv} package and the
\code{\link[mirt:numerical_deriv]{mirt::numerical_deriv}}
function from the \pkg{mirt} package.
}

\examples{
#############################################################################
# EXAMPLE 1: Toy example for Hessian matrix
#############################################################################

# define function
f <- function(x){
     3*x[1]^3 - 4*x[2]^2 - 5*x[1]*x[2] + 10 * x[1] * x[3]^2 + 6*x[2]*sqrt(x[3])
}
# define point for evaluating partial derivatives
par <- c(3,8,4)

#--- compute gradient
CDM::numerical_Hessian( par = par , FUN = f , gradient=TRUE , hessian = FALSE)
\dontrun{
mirt::numerical_deriv(par = par , f = f, gradient=TRUE)

#--- compute Hessian matrix
CDM::numerical_Hessian( par = par , FUN = f )
mirt::numerical_deriv(par = par , f = f, gradient=FALSE)
numerical_Hessian( par = par , FUN = f , h = 1E-4 )

#--- compute gradient and Hessian matrix
CDM::numerical_Hessian( par = par , FUN = f , gradient=TRUE , hessian=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
