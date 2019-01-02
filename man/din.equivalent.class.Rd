%% File Name: din.equivalent.class.Rd
%% File Version: 0.31

\name{din.equivalent.class}
\alias{din.equivalent.class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of Equivalent Skill Classes in the DINA/DINO Model
}
\description{
This function computes indistinguishable skill classes for the DINA and
DINO model (Gross & George, 2014; Zhang, DeCarlo & Ying, 2013).
}
\usage{
din.equivalent.class(q.matrix, rule="DINA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q.matrix}{The Q-matrix (see \code{\link{din}}).}
  \item{rule}{
    The condensation rule. If it is a string, then the rule applies
    to all items. If it is a vector, then for each item \code{DINA}
    or \code{DINO} rule can be chosen.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with following entries:
  \item{latent.responseM}{Matrix of latent responses}
  \item{latent.response}{Latent responses represented as a string}
  \item{S}{Matrix containing all skill classes}
  \item{gini}{Gini coefficient of the frequency distribution of
    identifiable skill classes which result in the same latent response
    }
  \item{skillclasses}{Data frame with skill class (\code{skillclass}),
    latent responses (\code{latent.response}) and an identifier for
    distinguishable skill classes (\code{distinguish.class}).
    }
}
\references{
Gross, J. & George, A. C. (2014). On prerequisite relations between
attributes in noncompensatory diagnostic classification.
\emph{Methodology, 10}(3), 100-107.

Zhang, S. S., DeCarlo, L. T., & Ying, Z. (2013).
Non-identifiability, equivalence classes, and attribute-specific classification
in Q-matrix based cognitive diagnosis models.
\emph{arXiv preprint}, \emph{arXiv:1303.0426}.
}
%\author{
%Ann Cathrice George, Alexander Robitzsch
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
# EXAMPLE 1: Equivalency classes for DINA model for fraction subtraction data
#############################################################################

#-- DINA models

data(data.fraction2, package="CDM")

# first Q-matrix
Q1 <- data.fraction2$q.matrix1
m1 <- CDM::din.equivalent.class( q.matrix=Q1, rule="DINA" )
  ## 8 Skill classes | 5  distinguishable skill classes | Gini coefficient=0.3

# second Q-matrix
Q1 <- data.fraction2$q.matrix2
m1 <- CDM::din.equivalent.class( q.matrix=Q1, rule="DINA" )
  ## 32 Skill classes | 9  distinguishable skill classes | Gini coefficient=0.5

# third Q-matrix
Q1 <- data.fraction2$q.matrix3
m1 <- CDM::din.equivalent.class( q.matrix=Q1, rule="DINA" )
  ## 8 Skill classes | 8  distinguishable skill classes | Gini coefficient=0

# original fraction subtraction data
m1 <- CDM::din.equivalent.class( q.matrix=CDM::fraction.subtraction.qmatrix, rule="DINA")
  ## 256 Skill classes | 58  distinguishable skill classes | Gini coefficient=0.659
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{DINA}
%% \keyword{DINO}
%% \keyword{Equivalent skill classes}
