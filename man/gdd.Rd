%% File Name: gdd.Rd
%% File Version: 0.14

\name{gdd}
\alias{gdd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generalized Distance Discriminating Method
}

\description{
Performs the generalized distance discriminating method 
(GDD; Sun, Xin, Zhang, & de la Torre, 2013) for
dichotomous data which is a method for classifying students into
skill profiles based on a preliminary unidimensional calibration.
}

\usage{
gdd(data, q.matrix, theta, b, a, skillclasses = NULL)
}


\arguments{
  \item{data}{
Data frame with \eqn{N \times J} item responses
}
  \item{q.matrix}{
The Q-matrix
}
  \item{theta}{
Estimated person ability
}
  \item{b}{
Estimated item intercept from a 2PL model (see Details)
}
  \item{a}{
Estimated item slope from a 2PL model (see Details)
}
  \item{skillclasses}{
Optional matrix of skill classes used for estimation
}
}

\details{
Note that the parameters in the arguments follow the item response model
\deqn{ logit P( X_{nj} = 1 | \theta_n ) = b_j + a_j \theta_n }
which is employed in the \code{gdm} function.
}

\value{
A list with following entries

  \item{skillclass.est}{Estimated skill class}
  \item{distmatrix}{Distances for every person and every skill class}
  \item{skillspace}{Used skill space for estimation}
  \item{theta}{Used person parameter estimate}
}


\references{
Sun, J., Xin, T., Zhang, S., & de la Torre, J. (2013). 
A polytomous extension of the generalized distance discriminating method. 
\emph{Applied Psychological Measurement, 37}, 503-521.
}

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
#############################################################################
# EXAMPLE 1: GDD for sim.dina
#############################################################################	

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

data <- sim.dina
q.matrix <- sim.qmatrix

# estimate 1PL (use irtmodel="2PL" for 2PL estimation)
mod <- CDM::gdm( data , irtmodel="1PL" , theta.k=seq(-6,6,len=21) , 
                    decrease.increments=TRUE , conv=.001 , globconv=.001)
# extract item parameters in parametrization b + a*theta
b <- mod$b[,1]
a <- mod$a[,,1]
# extract person parameter estimate
theta <- mod$person$EAP.F1

# generalized distance discriminating method
res <- CDM::gdd( data , q.matrix , theta=theta , b=b, a=a )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Generalized distance discriminating method (GDD)}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
