%% File Name: plot.din.Rd
%% File Version: 2.14

\name{plot.din}

\alias{plot.din}

\title{
  Plot Method for Objects of Class din
}

\description{
  S3 method to plot objects of the class \code{din}.
}

\usage{
  \method{plot}{din}(x, items=c(1:ncol(x$data)), pattern="",
    uncertainty=0.1, top.n.skill.classes=6, pdf.file="",
    hide.obs=FALSE, display.nr=1:4, ask=TRUE, \dots)
}

\arguments{
  \item{x}{A required object of class \code{din}, obtained from a
    call to the function \code{\link{din}}.  }

  \item{items}{An index vector giving the items to be visualized in the first
    plot, see \sQuote{Details}.  The default is
    \code{items=c(1:ncol(x$data))}, which is all items.  }

  \item{pattern}{An optional character or a numeric vector specifying a response pattern
    of an respondent, whose attributes are analyzed in a separate
    graphic.  It is required to choose a pattern from the empirical
    data set (see Example).  }

  \item{uncertainty}{A numeric between 0 and 0.5 giving the
    uncertainty bounds for deriving the observed skill occurrence probabilities
    in plot 2 and the simplified deterministic attribute profiles in plot 4.  }

  \item{top.n.skill.classes}{A numeric, specifying the number of skill classes,
    starting with the most frequent, to be labeled in plot 3.
    Default value is 6.  }

  \item{pdf.file}{An optional character string. If specified the graphics
    obtained from the function \code{plot.din} are provided in a pdf file.
    The default is \code{pdf.file=""}, which is not providing a pdf file.
    Otherwise specify a directory and filename ending with \code{.pdf} where to
    write the document.  }

  \item{hide.obs}{An optional logical value. If set to \code{TRUE}, the
    polygonal chain for observed frequencies of skill class probabilities in the
    second graphic is not displayed.  }

  \item{display.nr}{An optional numeric or numeric vector. If specified, only the plots in
    \code{display.nr} are displayed. Default is \code{display.nr=1:4} causing the display of
    all four plots.   }

  \item{ask}{An optional logical indicating whether a request for a user input
        is necessary before the next figure is drawn.}

  \item{\dots}{Optional graphical parameters to be passed to or from other
    methods will be ignored.  }
}

\details{
  The \code{plot} method graphs the results obtained from a CDM analysis.
  Four graphics to analyze the fitted model are produced, respectively.

  The first graphic depicts the parameter estimates their diagnostic accuracy
  for each of chosen the items in \code{items}.  Parameter estimates are
  splitted in guessing and slipping errors for each item.  See \code{\link{din}}
  for further information.

  The second graphic shows the estimated occurrence probabilities of the attributes
  underlying the items.

  The third graphic illustrates the distribution of the skill class occurrence
  probabilities.  The \cr \code{top.n.skill.classes} most frequent skill classes are labeled.

  The forth plot is a parallel coordinate plot of the individual skill profiles.
  Each line represents an individual skill profile.  For each of these skill profiles
  on the vertical lines the individual probabilities of mastering the corresponding
  attributes are drawn.

  If in \code{pattern} an empirical response pattern is specified, the fifth plot
  shows the individual skill profile of an examinee having this response pattern.
  For each attribute, having a mastering probability below \eqn{0.5 - uncertainty}
  the examinee is classified as non-master of the corresponding attribute.  For
  mastering probabilities higher than \eqn{0.5 + uncertainty} the examinee is
  classified as master of the corresponding attribute.
}

\value{
  If the argument \code{x} is of required type,
  and if the optional arguments \code{items}, \code{uncertainty},\cr
  \code{top.n.skill.classes} and \code{pdf.file} are specified as required, the
  \code{plot.din} produces several graphics to analyze a CDM model.
}

%\author{
%  Alexander Robitzsch, Thomas Kiefer, Ann Cathrice George, Ali Uenlue
%  Maintainer: Alexander Robitzsch <a.robitzsch@bifie.at>,
%              Thomas Kiefer <t.kiefer@bifie.at>,
%              Ann Cathrice George <george@statistik.tu-dortmund.de>,
%              Ali Uenlue <ali.uenlue@tum.de>
%}

% \references{
%  Rupp, A. A., Templin, J. L., & Henson, R. A. (2010). \emph{Diagnostic
%  Measurement: Theory, Methods, and Applications}. New York: The Guilford
%  Press.
%}

\seealso{
  \code{\link{print.din}}, the S3 method for printing objects of
  the class \code{din}; \code{\link{summary.din}}, the S3
  method for summarizing objects of the class \code{din}, which
  creates objects of the class \code{summary.din};
  \code{\link{print.summary.din}}, the S3 method for printing
  objects of the class \code{summary.din}; \code{\link{din}},
  the main function for DINA and DINO parameter estimation, which
  creates objects of the class \code{din}.  See also \code{\link{CDM-package}}
  for general information about this package.
}

\examples{
##
## (1) examples based on dataset fractions.subtraction.data
##

data(fraction.subtraction.data)
data(fraction.subtraction.qmatrix)

## Fix the guessing parameters of items 5, 8 and 9 equal to .20
# define a constraint.guess matrix
constraint.guess <-  matrix(c(5,8,9, rep(0.2, 3)), ncol=2)
fractions.dina.fixed <- CDM::din(data=fraction.subtraction.data,
  q.matrix=fraction.subtraction.qmatrix,
  constraint.guess=constraint.guess)

## The second plot shows the expected (MAP) and observed skill
## probabilities. The third plot visualizes the skill class
## occurrence probabilities; Only the 'top.n.skill.classes' most frequent
## skill classes are labeled; it is obvious that the skill class '11111111'
## (all skills are mastered) is the most probable in this population.
## The fourth plot shows the skill probabilities conditional on response
## patterns; in this population the skills 3 and 6 seem to be
## mastered easier than the others. The fifth plot shows the
## skill probabilities conditional on a specified response
## pattern; it is shown whether a skill is mastered (above
## .5+'uncertainty') unclassifiable (within the boundaries) or
## not mastered (below .5-'uncertainty'). In this case, the
## 527th respondent was chosen; if no response pattern is
## specified, the plot will not be shown (of course)
pattern <- paste(fraction.subtraction.data[527, ], collapse="")
plot(fractions.dina.fixed, pattern=pattern, display.nr=4)

# It is also possible to input a vector of item responses
plot(fractions.dina.fixed, pattern=fraction.subtraction.data[527, ],display.nr=4)

#uncertainty=0.1, top.n.skill.classes=6 are default
plot(fractions.dina.fixed, uncertainty=0.1, top.n.skill.classes=6,
  pattern=pattern)
}

\keyword{methods}
\keyword{print}
