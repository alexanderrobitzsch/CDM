%% File Name: sim.gdina.Rd
%% File Version: 1.17

\name{sim.gdina}
\alias{sim.gdina}
\alias{sim.gdina.prepare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation of the GDINA model
}
\description{
The function \code{sim.gdina.prepare} creates necessary design matrices
\code{Mj}, \code{Aj} and \code{necc.attr}. In most cases, only the list 
of item parameters \code{delta} must be modified by the user when
applying the simulation function \code{sim.gdina}. The distribution of latent
classes \eqn{\alpha} is represented by an underlying multivariate normal distribution
\eqn{\alpha^\ast} for which a mean vector \code{thresh.alpha} and a 
covariance matrix \code{cov.alpha} must be specified.
Alternatively, a matrix of skill classes \code{alpha}
can be given as an input.

Note that this version of \code{sim.gdina} only works for dichotomous attributes.
}
\usage{
sim.gdina(n, q.matrix, delta, link = "identity",  thresh.alpha=NULL, 
    cov.alpha=NULL, alpha=NULL, Mj, Aj, necc.attr)
    
sim.gdina.prepare( q.matrix )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of persons
}
  \item{q.matrix}{
Q-matrix (see \code{\link{sim.din}})
}
  \item{delta}{
List with \eqn{J} entries where \eqn{J} is the number of items. 
Every list element corresponds to the parameter of an item.
}
  \item{link}{
Link function. Choices are \code{identity} (default), \code{logit} and \code{log}.
}
  \item{thresh.alpha}{
Vector of thresholds (means) of \eqn{\alpha^\ast}
}
  \item{cov.alpha}{
Covariance matrix of \eqn{\alpha^\ast}
}
  \item{alpha}{
Matrix of skill classes if they should not be simulated
}
  \item{Mj}{
Design matrix, see \code{\link{gdina}}
}
  \item{Aj}{
Design matrix, see \code{\link{gdina}}
}
  \item{necc.attr}{
List with \eqn{J} entries containing necessary attributes
for each item
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The output of \code{sim.gdina} is a list with following entries:
  \item{data}{Simulated item responses}
  \item{alpha}{Data frame with simulated attributes}
  \item{q.matrix}{Used Q-matrix}
  \item{delta}{Used delta item parameters}
  \item{Aj}{Design matrices \eqn{A_j}}
  \item{Mj}{Design matrices \eqn{M_j}}
  \item{link}{Used link function}

The function \code{sim.gdina.prepare} possesses the following values as output
in a list: \code{delta}, \code{necc.attr}, \code{Aj} and \code{Mj}.
}
\references{
  de la Torre, J. (2011). The generalized {DINA} model framework. 
  \emph{Psychometrika}, \bold{76}, 179--199.    
}
%\author{
%Alexander Robitzsch, ...
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For estimating the GDINA model see \code{\link{gdina}}.

See the \code{\link[GDINA:simGDINA]{GDINA::simGDINA}} function in the
\pkg{GDINA} package for similar functionality.
}
\examples{
#############################################################################
# EXAMPLE 1: Simulating the GDINA model
#############################################################################

n <- 50             # number of persons
# define Q-matrix
q.matrix <- matrix(  c(1,1,0 , 0,1,1, 1,0,1, 1,0,0,
    0,0,1, 0,1,0,  1,1,1,  0,1,1, 0,1,1) , ncol=3 , byrow=TRUE)
# thresholds for attributes alpha^\ast
thresh.alpha <- c( .65 , 0 , -.30 )
# covariance matrix for alpha^\ast
cov.alpha <- matrix(1,3,3)
cov.alpha[1,2] <- cov.alpha[2,1] <- .4
cov.alpha[1,3] <- cov.alpha[3,1] <- .6
cov.alpha[3,2] <- cov.alpha[2,3] <- .8

# prepare design matrix by applying sim.gdina.prepare function
rp <- CDM::sim.gdina.prepare( q.matrix )
delta <- rp$delta
necc.attr <- rp$necc.attr
Aj <- rp$Aj
Mj <- rp$Mj
# define delta parameters
# intercept - main effects - second order interactions - ...
str(delta)  # => modify the delta parameter list which contains only zeroes as default
##   List of 9
##    $ : num [1:4] 0 0 0 0
##    $ : num [1:4] 0 0 0 0
##    $ : num [1:4] 0 0 0 0
##    $ : num [1:2] 0 0
##    $ : num [1:2] 0 0
##    $ : num [1:2] 0 0
##    $ : num [1:8] 0 0 0 0 0 0 0 0
##    $ : num [1:4] 0 0 0 0
##    $ : num [1:4] 0 0 0 0
delta[[1]] <- c( .2 , .1 , .15 , .4 )
delta[[2]] <- c( .2 , .3 , .3 , -.2 )
delta[[3]] <- c( .2 , .2 , .2 , 0 )
delta[[4]] <- c( .15 , .6 )
delta[[5]] <- c( .1 , .7 )
delta[[6]] <- c( .25 , .65 )
delta[[7]] <- c( .25 , .1 , .1 , .1 , 0 , 0 , 0 , .25 )
delta[[8]] <- c( .2 , 0 , .3 , -.1 )
delta[[9]] <- c( .2 , .2 , 0 , .3 )

#******************************************
# Now, the "real simulation" starts
sim.res <- sim.gdina( n=n, q.matrix =q.matrix, delta=delta, link = "identity", 
                thresh.alpha=thresh.alpha , cov.alpha=cov.alpha ,
                Mj=Mj , Aj=Aj , necc.attr =necc.attr)
# sim.res$data      # simulated data
# sim.res$alpha     # simulated alpha

\dontrun{
#############################################################################
# EXAMPLE 2: Simulation based on already estimated GDINA model for data.ecpe
#############################################################################

data(data.ecpe)
dat <- data.ecpe$data
q.matrix <- data.ecpe$q.matrix

#***
# (1) estimate GDINA model
mod <- CDM::gdina( data=dat[,-1] , q.matrix=q.matrix )

#***
# (2) simulate data according to GDINA model
set.seed(977)

# prepare design matrix by applying sim.gdina.prepare function
rp <- CDM::sim.gdina.prepare( q.matrix )
necc.attr <- rp$necc.attr

# number of subjects to be simulated
n <- 3000
# simulate attribute patterns
probs <- mod$attribute.patt$class.prob   # probabilities
patt <- mod$attribute.patt.splitted      # response patterns
alpha <- patt[ sample( 1:(length(probs) ) , n , prob=probs , replace=TRUE) , ]

# simulate data using estimated item parameters
sim.res <- sim.gdina( n=n, q.matrix =q.matrix, delta=mod$delta, link = "identity", 
                alpha=alpha , Mj=mod$Mj , Aj= mod$Aj , necc.attr = rp$necc.attr)               
# extract data
dat <- sim.res$data

#############################################################################
# EXAMPLE 3: Simulation based on already estimated RRUM model for data.ecpe
#############################################################################

data(data.ecpe)
dat <- data.ecpe$data
q.matrix <- data.ecpe$q.matrix

#***
# (1) estimate reduced RUM model
mod <- CDM::gdina( data=dat[,-1] , q.matrix=q.matrix , rule="RRUM" )
summary(mod)

#***
# (2) simulate data according to RRUM model
set.seed(977)

# prepare design matrix by applying sim.gdina.prepare function
rp <- CDM::sim.gdina.prepare( q.matrix )
necc.attr <- rp$necc.attr

# number of subjects to be simulated
n <- 5000
# simulate attribute patterns
probs <- mod$attribute.patt$class.prob   # probabilities
patt <- mod$attribute.patt.splitted      # response patterns
alpha <- patt[ sample( 1:(length(probs) ) , n , prob=probs , replace=TRUE) , ]

# simulate data using estimated item parameters
sim.res <- sim.gdina( n=n, q.matrix =q.matrix, delta=mod$delta, link = mod$link , 
                alpha=alpha , Mj=mod$Mj , Aj=mod$Aj , necc.attr = rp$necc.attr)               
# extract data
dat <- sim.res$data
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{GDINA}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
