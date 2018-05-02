%% File Name: sim.din.Rd
%% File Version: 2.12

\name{sim.din}
\alias{sim.din}

\title{
  Data Simulation Tool for DINA, DINO and mixed DINA and DINO Data
}

\description{
  \code{sim.din} can be used to simulate dichotomous response data according to a CDM
  model.  The model type DINA or DINO can be specified item wise.  The number of items,
  the sample size, and two parameters for each item,
  the slipping and guessing parameters, can be set explicitly.
}

\usage{
  sim.din(N=0, q.matrix, guess = rep(0.2, nrow(q.matrix)),
    slip = guess, mean = rep(0, ncol(q.matrix)), Sigma = diag(ncol(q.matrix)),
    rule = "DINA", alpha=NULL)
}

\arguments{
  \item{N}{A numeric value specifying the number \eqn{N} of requested
    response patterns. If \code{alpha} is specified, then \code{N} is set
    by default to 0.}

  \item{q.matrix}{A required binary \eqn{J \times K} matrix describing which
    of the \eqn{K} attributes are required, coded by 1, and which attributes
    are not required, coded by 0, to master the items.   }

  \item{guess}{An optional vector of guessing parameters.  Default is
    0.2 for each item.  }

  \item{slip}{An optional vector of slipping parameters.  Default is
    0.2 for each item.  }

  \item{mean}{A numeric vector of length \code{ncol(q.matrix)} indicating
    the mean vector of the continuous version of the dichotomous skill vector.
    Default is \code{rep(0, length = ncol(q.matrix))}.
    That is, having a probability of \code{0.5} for possessing each of the
    attributes.  }

  \item{Sigma}{A matrix of dimension \code{ncol(q.matrix)} times \code{ncol(q.matrix)}
     specifying the covariance matrix of the continuous version of the dichotomous skill
     vector (i.e., the tetrachoric correlation of the dichotomous skill vector).
     Default is \code{diag( 1, ncol(q.matrix))}.  That is, by default the possession of
     the attributes is assumed to be uncorrelated.  }

  \item{rule}{An optional character string or vector of character strings
      specifying the model rule that is used.  The character strings must be
      of \code{"DINA"} or \code{"DINO"}.  If a vector of character strings is
      specified, implying an itemwise condensation rule, the vector must
      be of length \eqn{J}, which is the number of used items.  The default is
    the condensation rule \code{"DINA"} for all items.  }
  \item{alpha}{A matrix of attribute patterns which can be given as an input
    instead of underlying latent variables. If \code{alpha} is not \code{NULL},
    then \code{mean} and \code{Sigma} are ignored.
        }
}

\value{A list with following entries
    \item{dat}{A matrix of simulated dichotomous response data
            according to the specified CDM model.
                }
    \item{alpha}{Simulated attributes}
}

\references{
  Rupp, A. A., Templin, J. L., & Henson, R. A. (2010). \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}. New York: The Guilford
  Press.
}

\seealso{
  \code{\link{Data-sim}} for artificial date set simulated with the help of this
  method; \code{\link{plot.din}}, the S3 method for plotting objects of
  the class \code{din}; \code{\link{summary.din}}, the S3
  method for summarizing objects of the class \code{din}, which
  creates objects of the class \code{summary.din};
  \code{\link{print.summary.din}}, the S3 method for printing
  objects of the class \code{summary.din}; \code{\link{din}},
  the main function for DINA and DINO parameter estimation,
  which creates objects of the class \code{din}.  See also
  \code{\link{CDM-package}} for general information about this package.
}

\examples{
#############################################################################
## EXAMPLE 1: simulate DINA/DINO data according to a tetrachoric correlation
#############################################################################

# define Q-matrix for 4 items and 2 attributes
q.matrix <- matrix(c(1,0,0,1,1,1,1,1), ncol = 2, nrow = 4)

# Slipping parameters
slip <- c(0.2,0.3,0.4,0.3)

# Guessing parameters
guess <- c(0,0.1,0.05,0.2)

set.seed(1567) # fix random numbers
dat1 <- CDM::sim.din(N = 200, q.matrix, slip=slip, guess=guess,
  # Possession of the attributes with high probability
  mean = c(0.5,0.2),
  # Possession of the attributes is weakly correlated
  Sigma = matrix(c(1,0.2,0.2,1), ncol=2), rule = "DINA")$dat
head(dat1)

set.seed(15367) # fix random numbers
res <- CDM::sim.din(N = 200, q.matrix, slip=slip, guess=guess, mean = c(0.5,0.2),
         Sigma = matrix(c(1,0.2,0.2,1), ncol=2), rule = "DINO")

# extract simulated data
dat2 <- res$dat
# extract attribute patterns
head( res$alpha )
  ##        [,1] [,2]
  ##   [1,]    1    1
  ##   [2,]    1    1
  ##   [3,]    1    1
  ##   [4,]    1    1
  ##   [5,]    1    1
  ##   [6,]    1    0

#  simulate data based on given attributes
#          -> 5 persons with 2 attributes -> see the Q-matrix above
alpha <- matrix( c(1,0,1,0,1,1,0,1,1,1) ,
    nrow=5,ncol=2, byrow=TRUE )
CDM::sim.din(  q.matrix = q.matrix, alpha = alpha )

\dontrun{
#############################################################################
# EXAMPLE 2: Simulation based on attribute vectors
#############################################################################
set.seed(76)
# define Q-matrix
Qmatrix <- matrix(c(1,0,1,0,1,0,0,1,0,1,0,1,1,1,1,1) , 8 , 2 , byrow=TRUE)
colnames(Qmatrix) <- c("Attr1","Attr2")
# define skill patterns
alpha.patt <- matrix(c(0,0,1,0,0,1,1,1) , 4 ,2,byrow=TRUE )
AP <- nrow(alpha.patt)
# define pattern probabilities
alpha.prob <- c( .20 , .40 , .10 , .30 )
# simulate alpha latent responses
N <- 1000     # number of persons
ind <- sample( x=1:AP , size=N, replace = TRUE , prob=alpha.prob)
alpha <- alpha.patt[ ind , ]    # (true) latent responses
# define guessing and slipping parameters
guess <- c(.26,.3,.07,.23,.24,.34,.05,.1)
slip <- c(.05,.16,.19,.03,.03,.19,.15,.05)
# simulation of the DINA model
dat <- CDM::sim.din(N=0, q.matrix= Qmatrix, guess = guess,
              slip = slip, alpha=alpha)$dat
# estimate model
res <- CDM::din( dat , q.matrix=Qmatrix )
# extract maximum likelihood estimates for individual classifications
est <- paste( res$pattern$mle.est )
# calculate classification accuracy
mean( est == apply( alpha , 1 , FUN = function(ll){ paste0(ll[1],ll[2] ) } ) )
  ##   [1] 0.935

#############################################################################
# EXAMPLE 3: Simulation based on already estimated DINA model for data.ecpe
#############################################################################

dat <- CDM::data.ecpe$data
q.matrix <- CDM::data.ecpe$q.matrix

#***
# (1) estimate DINA model
mod <- CDM::din( data=dat[,-1] , q.matrix=q.matrix , rule="DINA")

#***
# (2) simulate data according to DINA model
set.seed(977)
# number of subjects to be simulated
n <- 3000
# simulate attribute patterns
probs <- mod$attribute.patt$class.prob   # probabilities
patt <- mod$attribute.patt.splitted      # response patterns
alpha <- patt[ sample( 1:(length(probs) ) , n , prob=probs , replace=TRUE) , ]
# simulate data using estimated item parameters
res <- CDM::sim.din(N=n, q.matrix=q.matrix, guess = mod$guess$est , slip=mod$slip$est ,
              rule = "DINA", alpha=alpha)
# extract data
dat <- res$dat
}
}
