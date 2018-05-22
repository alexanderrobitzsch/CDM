%% File Name: din.validate.qmatrix.Rd
%% File Version: 1.30

\name{din.validate.qmatrix}
\alias{din.validate.qmatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Q-Matrix Validation (Q-Matrix Modification) for Mixed DINA/DINO Model
}
\description{
Q-matrix entries can be modified by the Q-matrix validation method
of de la Torre (2008). After estimating a mixed DINA/DINO model
using the \code{\link{din}} function, item parameters and the item
discrimination parameters \eqn{IDI_j} are recalculated. Q-matrix rows
are determined by maximizing the estimated item discrimination index
\eqn{IDI_j=1-s_j -g_j}.

See Chiu (2013) for an alternative estimation approach based on
residual sum of squares which is implemented in the \pkg{NPCD}
package.
}

\usage{
din.validate.qmatrix(object, IDI_diff=.02, print=TRUE)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{din}
}
  \item{IDI_diff}{
Minimum difference in IDI values for choosing a new Q-matrix vector
}
\item{print}{An optional logical indicating whether the function
    should print the progress of iteration in the estimation process.  }
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with following entries:
\item{coef.modified}{Estimated parameters by applying Q-matrix
   modifications}
\item{coef.modified.short}{A shortened matrix of \code{coef.modified}.
    Only Q-matrix rows which increase the \eqn{IDI} are displayed.
    }
\item{q.matrix.prop}{The proposed Q-matrix by Q-matrix validation.}
}


\references{
Chiu, C. Y. (2013). Statistical refinement of the Q-matrix in cognitive
diagnosis. \emph{Applied Psychological Measurement, 37}, 598-618.

de la Torre, J. (2008). An empirically based method of Q-matrix
validation for the DINA model: Development and applications.
\emph{Journal of Educational Measurement, 45}, 343-362.
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
The mixed DINA/DINO model can be estimated with \code{\link{din}}.

See also the \code{NPCD::Qrefine} function in the \pkg{NPCD} package.

See the \code{\link[GDINA:Qval]{GDINA::Qval}} function in the
\pkg{GDINA} package for extended functionality.
}


\examples{
#############################################################################
# EXAMPLE 1: Detection of a mis-specified Q-matrix
#############################################################################

set.seed(679)
# specify true Q-matrix
q.matrix <- matrix( 0, 12, 3 )
q.matrix[1:3,1] <- 1
q.matrix[4:6,2] <- 1
q.matrix[7:9,3] <- 1
q.matrix[10,] <- c(1,1,0)
q.matrix[11,] <- c(1,0,1)
q.matrix[12,] <- c(0,1,1)
# simulate data
dat <- CDM::sim.din( N=4000, q.matrix)$dat
# incorrectly modify Q-matrix rows 1 and 10
Q1 <- q.matrix
Q1[1,] <- c(1,1,0)
Q1[10,] <- c(1,0,0)
# estimate DINA model
mod <- CDM::din( dat, q.matr=Q1, rule="DINA")
# apply Q-matrix validation
res <- CDM::din.validate.qmatrix( mod )
  ## item itemindex Skill1 Skill2 Skill3 guess  slip   IDI qmatrix.orig IDI.orig delta.IDI max.IDI
  ## I001         1      1      0      0 0.309 0.251 0.440            0    0.431     0.009   0.440
  ## I010        10      1      1      0 0.235 0.329 0.437            0    0.320     0.117   0.437
  ## I010        10      1      1      1 0.296 0.301 0.403            0    0.320     0.083   0.437
  ##
  ##   Proposed Q-matrix:
  ##
  ##          Skill1 Skill2 Skill3
  ##   Item1       1      0      0
  ##   Item2       1      0      0
  ##   Item3       1      0      0
  ##   Item4       0      1      0
  ##   Item5       0      1      0
  ##   Item6       0      1      0
  ##   Item7       0      0      1
  ##   Item8       0      0      1
  ##   Item9       0      0      1
  ##   Item10      1      1      0
  ##   Item11      1      0      1
  ##   Item12      0      1      1

\dontrun{
#*****************
# Q-matrix estimation ('Qrefine') in the NPCD package
# See Chiu (2013, APM).
#*****************

library(NPCD)
Qrefine.out <- NPCD::Qrefine( dat, Q1, gate="AND", max.ite=50)
print(Qrefine.out)
  ##   The modified Q-matrix
  ##           Attribute 1 Attribute 2 Attribute 3
  ##   Item 1            1           0           0
  ##   Item 2            1           0           0
  ##   Item 3            1           0           0
  ##   Item 4            0           1           0
  ##   Item 5            0           1           0
  ##   Item 6            0           1           0
  ##   Item 7            0           0           1
  ##   Item 8            0           0           1
  ##   Item 9            0           0           1
  ##   Item 10           1           1           0
  ##   Item 11           1           0           1
  ##   Item 12           0           1           1
  ##
  ##   The modified entries
  ##        Item Attribute
  ##   [1,]    1         2
  ##   [2,]   10         2

plot(Qrefine.out)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Q-matrix validation}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
