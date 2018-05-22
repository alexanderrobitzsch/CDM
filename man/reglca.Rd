%% File Name: reglca.Rd
%% File Version: 0.19

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
reglca(dat, nclasses, weights=NULL, group=NULL, regular_type="scad",
   regular_lam=0, sd_noise_init=1, item_probs_init=NULL, class_probs_init=NULL,
   random_starts=1, random_iter=20, conv=1e-05, h=1e-04, mstep_iter=10,
   maxit=1000, verbose=TRUE)

\method{summary}{reglca}(object, digits=4, file=NULL,  \dots)
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
\item{group}{
Optional vector for grouping variable
}
  \item{regular_type}{
Regularization type. Can be \code{scad} or \code{mcp}. See \code{\link{gdina}} for
more information.
}
  \item{regular_lam}{
Regularization parameter \eqn{\lambda}
}
  \item{sd_noise_init}{
Standard deviation for amount of noise in generating random starting values
}
  \item{item_probs_init}{
Optional matrix of initial item response probabilities
}
  \item{class_probs_init}{
Optional vector of class probabilities
}
  \item{random_starts}{
Number of random starts
}
  \item{random_iter}{
Number of initial iterations for random starts
}
  \item{conv}{
Convergence criterion
}
  \item{h}{
Numerical differentiation parameter
}
  \item{mstep_iter}{
Number of iterations in the M-step
}
  \item{maxit}{
Maximum number of iterations
}
  \item{verbose}{
Logical indicating whether convergence progress should be displayed
}
  \item{object}{A required object of class \code{gdina}, obtained
    from a call to the function \code{\link{gdina}}.  }
  \item{digits}{Number of digits after decimal separator to display.  }
\item{file}{Optional file name for a file in which \code{summary}
should be sinked.}
\item{\dots}{Further arguments to be passed.}
}

\details{
The regularized latent class model for dichotomous item responses assumes \eqn{C}
latent classes. The item response probabilities \eqn{P(X_i=1|c)=p_{ic}} are estimated
in such a way such that the number of different \eqn{p_{ic}} values per item is
minimized. This approach eases interpretability and enables to recover the
structure of a true (but unknown) cognitive diagnostic model.
}

\value{
A list containing following elements (selection):

\item{item_probs}{Item response probabilities}
\item{class_probs}{Latent class probabilities}
\item{p.aj.xi}{Individual posterior}
\item{p.xi.aj}{Individual likelihood}
\item{loglike}{Log-likelihood value}
\item{Npars}{Number of estimated parameters}
\item{Nskillpar}{Number of skill class parameters}
\item{G}{Number of groups}
\item{n.ik}{Expected counts}
\item{Nipar}{Number of item parameters}
\item{n_reg}{Number of regularized parameters}
\item{n_reg_item}{Number of regularized parameters per item}
\item{item}{Data frame with item parameters}
\item{pjk}{Item response probabilities (in an array)}
\item{N}{Number of persons}
\item{I}{Number of items}
}

\references{
Chen, Y., Liu, J., Xu, G., & Ying, Z. (2015). Statistical analysis of Q-matrix
based diagnostic classification models.
\emph{Journal of the American Statistical Association, 110}, 850-866.

Chen, Y., Li, X., Liu, J., & Ying, Z. (2017).
Regularized latent class analysis with application in cognitive diagnosis.
\emph{Psychometrika, 82}, 660-692.
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
q.matrix[ 1:(I/3), 1 ] <- 1
q.matrix[ I/3 + 1:(I/3), 2 ] <- 1
q.matrix[ 2*I/3 + 1:(I/3), c(1,2) ] <- 1
N <- 1000  # number of persons
guess <- rep(seq(.1,.3,length=I/3), 3)
slip <- .1
rho <- 0.3  # skill correlation
set.seed(987)
dat <- CDM::sim.din( N=N, q.matrix=q.matrix, guess=guess, slip=slip,
    mean=0*c( .2, -.2 ), Sigma=matrix( c( 1, rho,rho,1), 2, 2 ) )
dat <- dat$dat

#--- Model 1: Four latent classes without regularization
mod1 <- CDM::reglca(dat=dat, nclasses=4, regular_lam=0, random_starts=3,
               random_iter=10, conv=1E-4)
summary(mod1)

#--- Model 2: Four latent classes with regularization
mod2 <- CDM::reglca(dat=dat, nclasses=4, regular_lam=0.03, regular_type="scad",
               random_starts=3, random_iter=10, conv=1E-4)
summary(mod2)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
