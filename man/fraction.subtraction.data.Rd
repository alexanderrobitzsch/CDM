\name{fraction.subtraction.data}

\alias{fraction.subtraction.data}
\docType{data}

\title{
  Fraction Subtraction Data
}

\description{
  Tatsuoka's (1984) fraction subtraction data set is comprised of 
  responses to \eqn{J=20} fraction subtraction test items from \eqn{N=536} 
  middle school students.  
}

\usage{
  data(fraction.subtraction.data)
}

\format{
  The \code{fraction.subtraction.data} data frame consists of 536
  rows and 20 columns, representing the responses of the \eqn{N=536} 
  students to each of the \eqn{J=20} test items.  Each row in the data set
  corresponds to the responses of a particular student.  Thereby a "1"
  denotes that a correct response was recorded, while "0" denotes an
  incorrect response.  The other way round, each column corresponds
  to all responses to a particular item.  
}

\details{
  The items used for the fraction subtraction test originally appeared
  in Tatsuoka (1984) and are published in Tatsuoka (2002).  They 
  can also be found in DeCarlo (2011).  All test items are based on 8 
  attributes (e.g. convert a whole number to a fraction, separate a whole 
  number from a fraction or simplify before subtracting).  The complete 
  list of skills can be found in \code{\link{fraction.subtraction.qmatrix}}.  
}

\source{
  The Royal Statistical Society Datasets Website, Series C, 
  Applied Statistics, Data analytic methods for latent partially
  ordered classification models:\cr
  URL: \emph{http://www.blackwellpublishing.com/rss/Volumes/Cv51p2_read2.htm}
}

\references{
  DeCarlo, L. T. (2011). On the analysis of fraction subtraction data:  
  The DINA Model, classification, latent class sizes, and the Q-Matrix.
  \emph{Applied Psychological Measurement}, \bold{35}, 8--26.
  
  Tatsuoka, C. (2002). Data analytic methods for latent partially ordered classification 
  models. \emph{Journal of the Royal Statistical Society, Series C, Applied Statistics}, 
  \bold{51}, 337--350.
  
  Tatsuoka, K. (1984). \emph{Analysis of errors in fraction addition and subtraction 
  problems}. Final Report for NIE-G-81-0002, University of Illinois, Urbana-Champaign.
}

\seealso{
  \code{\link{fraction.subtraction.qmatrix}} for the corresponding Q-matrix.  
}

%\examples{
%}

\keyword{datasets}
