%% File Name: print.summary.din.Rd
%% File Version: 1.11

\name{print.summary.din}

\alias{print.summary.din}

\title{
  Print Method for Objects of Class summary.din
}

\description{
  S3 method to print objects of the class \code{summary.din}.
}

\usage{
  \method{print}{summary.din}(x, \dots)
}

\arguments{
  \item{x}{A required object of class \code{summary.din}, obtained
    from a call to the function \code{\link{summary.din}} (through
    generic function \code{\link[base]{summary}}).  }

  \item{\dots}{Optional parameters to be passed to or from other
    methods will be ignored.  }
}

\details{
  The \code{print} method prints the summary information about objects
  of the class \code{din} computed by \code{\link{summary.din}},
  which are the item discriminations indices, the most frequent
  skill classes and the model information criteria AIC and BIC.
  Specific summary information details such as
  individual items with their discrimination index can be accessed through
  assignment (see \sQuote{Examples}).
}

\value{
  If the argument \code{x} is of required type,
  \code{print.summary.din} prints the summary
  information in \sQuote{Details}, and invisibly returns \code{x}.
}



\seealso{
  \code{\link{plot.din}}, the S3 method for plotting objects of
  the class \code{din}; \code{\link{print.din}}, the S3 method
  for printing objects of the class \code{din};
  \code{\link{summary.din}}, the S3 method for summarizing objects
  of the class \code{din}, which creates objects of the class
  \code{summary.din}; \code{\link{din}}, the main function for
  DINA and DINO parameter estimation, which creates objects of the class
  \code{din}.  See also \code{\link{CDM-package}} for general
  information about this package.
}

\examples{
##
## (1) examples based on dataset fractions.subtraction.data
##

## In particular, accessing detailed summary through assignment
mod <- CDM::din(data=CDM::fraction.subtraction.data,
          q.matrix=CDM::fraction.subtraction.qmatrix, rule="DINA")
smod <- summary(mod)
str(smod)
}

\keyword{methods}
\keyword{print}
