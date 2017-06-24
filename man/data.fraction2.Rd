\name{data.fraction2}
\alias{data.fraction2}
\docType{data}
\title{
Fraction Subtraction Dataset 2
}
\description{
This is the fraction subtraction data set with 536 students
and 11 items. For this data set, several \eqn{Q} matrices are available.
The Q-matrix was defined in de la Torre (2009).
}
\usage{data(data.fraction2)}
\format{
The data is a list. The first entry \code{data}
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
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
See \code{\link{fraction.subtraction.data}} for more information
about data source.
}


\references{
  de la Torre, J. (2009). DINA model parameter estimation:
  A didactic. \emph{Journal of Educational and Behavioral
  Statistics}, \bold{34}, 115--130.
  
  Henson, R. A., Templin, J. T., & Willse, J. T. (2009).
  Defining a family of cognitive diagnosis models using
  log-linear models with latent variables.
  \emph{Psychometrika}, \bold{74}, 191-210.
}
%\examples{
%% data(data.fraction2)
%% ## maybe str(data.fraction2) ; plot(data.fraction2) ...
%}
\keyword{datasets}
