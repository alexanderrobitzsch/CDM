%% File Name: data.fraction.Rd
%% File Version: 0.23


\name{data.fraction}
\alias{data.fraction}
\alias{data.fraction1}
\alias{data.fraction2}
\alias{data.fraction3}
\alias{data.fraction4}
\alias{data.fraction5}



\docType{data}
\title{
Fraction Subtraction Dataset with Different Subsets of Data and Different
Q-Matrices
}

\description{
Contains different sub-datasets of the fraction subtraction data of Tatsuoka
with different Q-matrix specifications.
}

\usage{
data(data.fraction1)
data(data.fraction2)
data(data.fraction3)
data(data.fraction4)
data(data.fraction5)
}


\format{
\itemize{

\item The dataset \code{data.fraction1} is the fraction subtraction data set with
536 students and 15 items. The Q-matrix was defined in de la Torre (2009).
This dataset is a list with the dataset (\code{data}) and
the Q-matrix as entries.

The format is:

\code{List of 2} \cr
\code{ $ data    :'data.frame':} \cr
\code{  ..$ T01: int [1:536] 0 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T02: int [1:536] 1 1 1 1 1 0 0 1 0 0 ...} \cr
\code{  ..$ T03: int [1:536] 0 1 1 1 1 1 0 0 0 0 ...} \cr
\code{  ..$ T04: int [1:536] 1 1 1 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ T05: int [1:536] 0 1 0 0 0 1 1 0 1 1 ...} \cr
\code{  ..$ T06: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T07: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T08: int [1:536] 1 1 0 1 1 0 0 0 1 1 ...} \cr
\code{  ..$ T09: int [1:536] 1 1 1 1 0 1 0 0 1 0 ...} \cr
\code{  ..$ T10: int [1:536] 1 1 1 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ T11: int [1:536] 1 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T12: int [1:536] 0 1 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ T13: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T14: int [1:536] 1 1 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ T15: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{ $ q.matrix: int [1:15, 1:5] 1 1 1 1 0 1 1 1 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:15] "T01" "T02" "T03" "T04" ...} \cr
\code{  .. ..$ : chr [1:5] "QT1" "QT2" "QT3" "QT4" ...} \cr


\item The dataset \code{data.fraction2} is the fraction subtraction data set
with 536 students and 11 items. For this data set, several \eqn{Q} matrices are
available. The data is a list. The first entry \code{data}
contains the data frame. The entry \code{q.matrix1} contains
the Q-matrix of Henson, Templin and Willse (2009).
The third entry \code{q.matrix2} is an alternative
Q-matrix of de la Torre (2009). The fourth entry is a
modified Q-matrix of \code{q.matrix1}.

The format is:

\code{ $ data     :'data.frame':} \cr
\code{  ..$ H01: int [1:536] 1 1 1 1 1 0 0 1 0 0 ...} \cr
\code{  ..$ H02: int [1:536] 1 1 1 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ H03: int [1:536] 0 1 0 0 0 1 1 0 1 1 ...} \cr
\code{  ..$ H04: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ H05: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ H06: int [1:536] 1 1 0 1 1 0 0 0 1 1 ...} \cr
\code{  ..$ H08: int [1:536] 1 1 1 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ H09: int [1:536] 1 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ H10: int [1:536] 0 1 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ H11: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ H13: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{ $ q.matrix1: int [1:11, 1:3] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:11] "H01" "H02" "H03" "H04" ...} \cr
\code{  .. ..$ : chr [1:3] "QH1" "QH2" "QH3"} \cr
\code{ $ q.matrix2: int [1:11, 1:5] 1 1 0 1 1 1 1 1 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:11] "H01" "H02" "H03" "H04" ...} \cr
\code{  .. ..$ : chr [1:5] "QT1" "QT2" "QT3" "QT4" ...} \cr
\code{ $ q.matrix3: num [1:11, 1:3] 0 0 0 1 0 0 0 0 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:11] "H01" "H02" "H03" "H04" ...} \cr
\code{  .. ..$ : chr [1:3] "Dim1" "Dim2" "Dim3"} \cr

\item The dataset \code{data.fraction3} contains 12 items and was
used in de la Torre (2011).

\code{List of 2} \cr
\code{ $ data    :'data.frame':       536 obs. of  12 variables:} \cr
\code{  ..$ B01: int [1:536] 0 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ B02: int [1:536] 1 1 1 1 1 0 0 1 0 0 ...} \cr
\code{  ..$ B03: int [1:536] 0 1 1 1 1 1 0 0 0 0 ...} \cr
\code{  ..$ B04: int [1:536] 0 1 0 0 0 1 1 0 1 1 ...} \cr
\code{  ..$ B05: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ B06: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ B07: int [1:536] 1 1 0 1 1 0 0 0 1 1 ...} \cr
\code{  ..$ B08: int [1:536] 1 1 1 1 0 1 0 0 1 0 ...} \cr
\code{  ..$ B09: int [1:536] 1 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ B10: int [1:536] 0 1 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ B11: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ B12: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{ $ q.matrix:'data.frame':       12 obs. of  5 variables:} \cr
\code{  ..$ item: Factor w/ 13 levels "","B01","B02",..: 2 3 4 5 6 7 8 9 10 11 ...} \cr
\code{  ..$ QA1 : int [1:12] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ QA2 : int [1:12] 0 1 0 0 1 1 1 0 0 0 ...} \cr
\code{  ..$ QA3 : int [1:12] 0 1 0 1 1 1 0 1 1 1 ...} \cr
\code{  ..$ QA4 : int [1:12] 0 1 0 0 1 1 0 0 0 1 ...} \cr

\item The dataset \code{data.fraction4} contains 17 items and was
used in de la Torre and Douglas (2004) and Chen, Liu, Xu and Ying (2015).

\code{List of 2} \cr
\code{ $ data    :'data.frame':       536 obs. of  17 variables:} \cr
\code{  ..$ A01: int [1:536] 0 0 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ A02: int [1:536] 0 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ A03: int [1:536] 0 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ A04: int [1:536] 1 1 1 1 1 0 0 1 0 0 ...} \cr
\code{  ..$ A05: int [1:536] 1 1 0 1 1 0 0 0 1 1 ...} \cr
\code{  ..$ A06: int [1:536] 1 1 1 1 0 1 0 0 1 0 ...} \cr
\code{  ..$ A07: int [1:536] 1 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ A08: int [1:536] 0 0 0 1 0 0 0 0 0 1 ...} \cr
\code{  ..$ A09: int [1:536] 1 1 1 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ A10: int [1:536] 1 1 1 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ A11: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ A12: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ A13: int [1:536] 0 1 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ A14: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ A15: int [1:536] 1 1 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ A16: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ A17: int [1:536] 0 1 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ q.matrix:'data.frame':       17 obs. of  9 variables:} \cr
\code{  ..$ item: Factor w/ 18 levels "","A01","A02",..: 2 3 4 5 6 7 8 9 10 11 ...} \cr
\code{  ..$ QA1 : int [1:17] 0 0 0 0 0 0 0 0 1 0 ...} \cr
\code{  ..$ QA2 : int [1:17] 0 0 0 1 0 1 1 1 1 1 ...} \cr
\code{  ..$ QA3 : int [1:17] 0 0 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ QA4 : int [1:17] 1 1 1 0 0 0 0 1 0 0 ...} \cr
\code{  ..$ QA5 : int [1:17] 0 0 0 1 0 0 1 0 0 1 ...} \cr
\code{  ..$ QA6 : int [1:17] 1 0 0 0 0 0 1 0 0 0 ...} \cr
\code{  ..$ QA7 : int [1:17] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ QA8 : int [1:17] 0 0 0 0 1 0 0 1 0 0 ...} \cr


\item The dataset \code{data.fraction5} contains 15 items and was
used as an example for the multiple strategy DINA model in
de la Torre and Douglas (2008) and Hou and de la Torre (2014).
The two Q-matrices for coding the multiple strategies are contained
in one matrix \code{q.matrix} by joining the columns of both matrices.

\code{List of 2} \cr
\code{ $ data    :'data.frame':       536 obs. of  15 variables:} \cr
\code{  ..$ T01: int [1:536] 0 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T02: int [1:536] 1 1 1 1 1 0 0 1 0 0 ...} \cr
\code{  ..$ T03: int [1:536] 0 1 1 1 1 1 0 0 0 0 ...} \cr
\code{  ..$ T04: int [1:536] 1 1 1 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ T05: int [1:536] 0 1 0 0 0 1 1 0 1 1 ...} \cr
\code{  ..$ T06: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T07: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T08: int [1:536] 1 1 0 1 1 0 0 0 1 1 ...} \cr
\code{  ..$ T09: int [1:536] 1 1 1 1 0 1 0 0 1 0 ...} \cr
\code{  ..$ T10: int [1:536] 1 1 1 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ T11: int [1:536] 1 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T12: int [1:536] 0 1 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ T13: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ T14: int [1:536] 1 1 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ T15: int [1:536] 1 1 0 1 0 0 0 0 0 0 ...} \cr
\code{ $ q.matrix:'data.frame':       15 obs. of  15 variables:} \cr
\code{  ..$ item: Factor w/ 16 levels "","T01","T02",..: 2 3 4 5 6 7 8 9 10 11 ...} \cr
\code{  ..$ SA1 : int [1:15] 0 1 1 1 0 1 1 1 1 1 ...} \cr
\code{  ..$ SA2 : int [1:15] 0 1 0 1 0 1 1 1 0 0 ...} \cr
\code{  ..$ SA3 : int [1:15] 0 1 0 1 1 1 1 0 1 1 ...} \cr
\code{  ..$ SA4 : int [1:15] 0 1 0 1 0 1 1 0 0 1 ...} \cr
\code{  ..$ SA5 : int [1:15] 0 0 0 1 0 0 0 0 0 1 ...} \cr
\code{  ..$ SA6 : int [1:15] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ SA7 : int [1:15] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ SB1 : int [1:15] 0 1 1 1 0 1 1 1 1 1 ...} \cr
\code{  ..$ SB2 : int [1:15] 0 0 0 0 1 1 1 1 0 1 ...} \cr
\code{  ..$ SB3 : int [1:15] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ SB4 : int [1:15] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ SB5 : int [1:15] 0 0 0 1 1 0 0 0 0 1 ...} \cr
\code{  ..$ SB6 : int [1:15] 0 1 0 1 1 1 1 0 1 0 ...} \cr
\code{  ..$ SB7 : int [1:15] 0 0 0 0 1 0 0 0 0 0 ...} \cr

}

}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
See \code{\link{fraction.subtraction.data}} for more information
about the data source.
}


\seealso{
\code{\link[GDINA:frac20]{GDINA::frac20}}
}


\references{
  Chen, Y., Liu, J., Xu, G. and Ying, Z. (2015).
  Statistical analysis of Q-matrix based diagnostic classification models.
  \emph{Journal of the American Statistical Association, 110}(510),
  850-866.

  de la Torre, J. (2009). DINA model parameter estimation:
  A didactic. \emph{Journal of Educational and Behavioral
  Statistics, 34}, 115-130.

  de la Torre, J. (2011). The generalized DINA model framework.
  \emph{Psychometrika, 76}, 179-199.

  de la Torre, J., & Douglas, J. A. (2004).
  Higher-order latent trait models for cognitive diagnosis.
  \emph{Psychometrika, 69}, 333-353.

  de la Torre, J., & Douglas, J. A. (2008).
  Model evaluation and multiple strategies in cognitive diagnosis:
  An analysis of fraction subtraction data.
  \emph{Psychometrika, 73}, 595-624.

  Henson, R. A., Templin, J. T., & Willse, J. T. (2009).
  Defining a family of cognitive diagnosis models using
  log-linear models with latent variables.
  \emph{Psychometrika, 74}, 191-210.

  Huo, Y., & de la Torre, J. (2014). Estimating a cognitive diagnostic model for
  multiple strategies via the EM algorithm.
  \emph{Applied Psychological Measurement, 38}, 464-485.

}
%\examples{
%% data(data.fraction1)
%}
\keyword{datasets}
