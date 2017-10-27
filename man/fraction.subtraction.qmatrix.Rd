%% File Name: fraction.subtraction.qmatrix.Rd
%% File Version: 1.04

\name{fraction.subtraction.qmatrix}

\alias{fraction.subtraction.qmatrix}
\docType{data}

\title{
  Fraction Subtraction Q-Matrix
}

\description{
  The Q-Matrix corresponding to Tatsuoka (1984) fraction subtraction data set.  
}

\usage{
  data(fraction.subtraction.qmatrix)
}

\format{
  The \code{fraction.subtraction.qmatrix} data frame consists of \eqn{J=20}
  rows and \eqn{K=8} columns, specifying the attributes that are believed to be
  involved in solving the items.  Each row in the data frame represents an item
  and the entries in the row indicate whether an attribute is needed to master
  the item (denoted by a "1") or not (denoted by a "0").  The attributes for the 
  fraction subtraction data set are the following:
  \describe{
    \item{\code{alpha1}}{convert a whole number to a fraction, }
    \item{\code{alpha2}}{separate a whole number from a fraction, }
    \item{\code{alpha3}}{simplify before subtracting, }
    \item{\code{alpha4}}{find a common denominator, }
    \item{\code{alpha5}}{borrow from whole number part, }
    \item{\code{alpha6}}{column borrow to subtract the second numerator from the first, }
    \item{\code{alpha7}}{subtract numerators, }
    \item{\code{alpha8}}{reduce answers to simplest form.  }
  }
}

\details{
  This Q-matrix can be found in DeCarlo (2011).  It is the same used by
  de la Torre and Douglas (2004).  
}

\source{
  DeCarlo, L. T. (2011). On the analysis of fraction subtraction data:  
  The DINA Model, classification, latent class sizes, and the Q-Matrix.
  \emph{Applied Psychological Measurement}, \bold{35}, 8--26.
}

\references{
  de la Torre, J. and Douglas, J. (2004). Higher-order latent trait models 
  for cognitive diagnosis. \emph{Psychometrika}, \bold{69}, 333--353.
  
  Tatsuoka, C. (2002). Data analytic methods for latent partially ordered classification 
  models. \emph{Journal of the Royal Statistical Society, Series C, Applied Statistics}, 
  \bold{51}, 337--350.
  
  Tatsuoka, K. (1984) \emph{Analysis of errors in fraction addition and subtraction 
  problems}. Final Report for NIE-G-81-0002, University of Illinois, Urbana-Champaign.  
}

%\examples{
%##
%}

\keyword{datasets}
