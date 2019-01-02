%% File Name: summary.din.Rd
%% File Version: 2.12

\name{summary.din}

\alias{summary.din}

\title{
  Summary Method for Objects of Class din
}

\description{
  S3 method to summarize objects of the class \code{din}.
}

\usage{
\method{summary}{din}(object, top.n.skill.classes=6, overwrite=FALSE, \dots)
}

\arguments{
  \item{object}{A required object of class \code{din}, obtained
    from a call to the function \code{\link{din}}.  }

  \item{top.n.skill.classes}{A numeric, specifying the number of skill
    classes, starting with the most frequent, to be returned.
    Default value is 6.  }

  \item{overwrite}{An optional boolean, specifying wether or not
    the method is supposed to overwrite an existing \code{log.file}.
    If the \code{log.file} exists and \code{overwrite} is
    \code{FALSE}, the user is asked to confirm the overwriting.  }


  \item{\dots}{Optional parameters to be passed to or from other
    methods will be ignored.}
}

\details{
  The function \code{summary.din} returns an object of the class
  \code{summary.din} (see \sQuote{Value}), for which a
  \code{print} method, \code{\link{print.summary.din}}, is
  provided.  Specific summary information details such as
  individual item parameters and their discrimination indices
  can be accessed through assignment (see \sQuote{Examples}).
}

\value{
  If the argument \code{object} is of required type,
  \code{summary.din} returns a named list, of the class
  \code{summary.din}, consisting of the following seven components:

  \item{CALL}{A character specifying the model rule, the number of
    items and the number of attributes underlying the items.  }

  \item{IDI}{A matrix giving the item discrimination
    index (IDI; Lee, de la Torre & Park, 2012) for each item \eqn{j}
    \deqn{ IDI_j=1 - s_j - g_j, }{ IDI_j=1 - s_j - g_j, }
    where a high IDI corresponds to favorable test items
    which have both low guessing and slipping rates. }

  \item{SKILL.CLASSES}{A vector giving the \code{top.n.skill.classes}
    most frequent skill classes and the corresponding class probability.  }

  \item{AIC}{A numeric giving the AIC of the specified model
    \code{object}.  }

  \item{BIC}{A numeric giving the BIC of the specified model
    \code{object}.  }

  \item{log.file}{A character giving the path and file of a specified
    log file.  }

  \item{din.object}{The object of class \code{din} for which the summary
    was requested.  }
}

\references{
Lee, Y.-S., de la Torre, J., & Park, Y. S. (2012). Relationships between
cognitive diagnosis, CTT, and IRT indices: An empirical investigation.
\emph{Asia Pacific Educational Research, 13}, 333-345.

Rupp, A. A., Templin, J. L., & Henson, R. A. (2010) \emph{Diagnostic
Measurement: Theory, Methods, and Applications}.  New York: The Guilford
Press.
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

## Parameter estimation of DINA model
# rule="DINA" is default
fractions.dina <- CDM::din(data=CDM::fraction.subtraction.data,
           q.matrix=CDM::fraction.subtraction.qmatrix, rule="DINA")

## corresponding summaries, including diagnostic accuracies,
## most frequent skill classes and information
## criteria AIC and BIC
summary(fractions.dina)

## In particular, accessing detailed summary through assignment
detailed.summary.fs <- summary(fractions.dina)
str(detailed.summary.fs)
}

%% \keyword{methods}
%% \keyword{summary}
