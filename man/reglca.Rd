%% File Name: reglca.Rd
%% File Version: 0.07

\name{reglca}
\alias{reglca}
\alias{summary.reglca}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Regularized Latent Class Analysis
}
\description{
Estimates the regularized latent class model for dichotomous
responses based on regularization methods 
(Chen, Liu, Xu, & Ying, 2015; Chen, Li, Liu, & Ying, 2017).
The SCAD and MCP penalty functions are available.
}

\usage{
reglca(dat, nclasses, weights = NULL, regular_type = "scad", regular_lam = 0, 
   sd_noise_init = 1, item_probs_init = NULL, class_probs_init = NULL, 
   random_starts = 1, random_iter = 20, conv = 1e-05, h = 1e-04, mstep_iter = 10, 
   maxit = 1000, verbose = TRUE)
   
\method{summary}{reglca}(object, digits = 4 , file = NULL ,  \dots)   
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Matrix with dichotomous item responses. \code{NA}s are allowed.
}
  \item{nclasses}{
Number of classes
}
  \item{weights}{
Optional vector of sampling weights
}
  \item{regular_type}{
%%     ~~Describe \code{regular_type} here~~
}
  \item{regular_lam}{
%%     ~~Describe \code{regular_lam} here~~
}
  \item{sd_noise_init}{
%%     ~~Describe \code{sd_noise_init} here~~
}
  \item{item_probs_init}{
%%     ~~Describe \code{item_probs_init} here~~
}
  \item{class_probs_init}{
%%     ~~Describe \code{class_probs_init} here~~
}
  \item{random_starts}{
%%     ~~Describe \code{random_starts} here~~
}
  \item{random_iter}{
%%     ~~Describe \code{random_iter} here~~
}
  \item{conv}{
%%     ~~Describe \code{conv} here~~
}
  \item{h}{
%%     ~~Describe \code{h} here~~
}
  \item{mstep_iter}{
%%     ~~Describe \code{mstep_iter} here~~
}
  \item{maxit}{
%%     ~~Describe \code{maxit} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{object}{A required object of class \code{gdina}, obtained
    from a call to the function \code{\link{gdina}}.  }
  \item{digits}{Number of digits after decimal separator to display.  }
\item{file}{Optional file name for a file in which \code{summary}
should be sinked.}      
\item{\dots}{Further arguments to be passed.}
}

\details{
ADD DESCRIPTION
}

\value{
A list containing following elements (selection; ADD MORE ELEMENTS)
  \item{item_probs}{Class-specific item probabilities}
  \item{class_probs}{Latent class probabilities}
}

\references{
Chen, Y., Liu, J., Xu, G., & Ying, Z. (2015). Statistical analysis of Q-matrix 
based diagnostic classification models. 
\emph{Journal of the American Statistical Association}, \bold{110}, 850-866.

Chen, Y., Li, X., Liu, J., & Ying, Z. (2017). 
Regularized latent class analysis with application in cognitive diagnosis. 
\emph{Psychometrika}, \bold{82}, 660-692.
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the \code{\link{gdina}} and \code{\link{slca}} functions 
for regularized estimation.
}
\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Estimating a regularized LCA for DINA data 
#############################################################################
	
#---- simulate data
I <- 12  # number of items
# define Q-matrix
q.matrix <- matrix(0,I,2)
q.matrix[ 1:(I/3) , 1 ] <- 1
q.matrix[ I/3 + 1:(I/3) , 2 ] <- 1
q.matrix[ 2*I/3 + 1:(I/3) , c(1,2) ] <- 1
N <- 1000  # number of persons
guess <- rep(seq(.1,.3,length=I/3) , 3)
slip <- .1
rho <- 0.3  # skill correlation
set.seed(987)
dat <- CDM::sim.din( N=N , q.matrix=q.matrix , guess = guess , slip=slip , 
    mean = 0*c( .2 , -.2 ) , Sigma = matrix( c( 1 , rho,rho,1) , 2 , 2 ) )
dat <- dat$dat

#--- Model 1: Four latent classes without regularization
mod1 <- CDM::reglca(dat=dat, nclasses=4, regular_lam = 0, random_starts=3, 
               random_iter=10, conv=1E-4)
summary(mod1)

#--- Model 2: Four latent classes with regularization
mod2 <- CDM::reglca(dat=dat, nclasses=4, regular_lam = 0.03, regular_type="scad",
               random_starts=3, random_iter=10, conv=1E-4)
summary(mod2)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
