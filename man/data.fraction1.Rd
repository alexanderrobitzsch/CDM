\name{data.fraction1}
\alias{data.fraction1}
\docType{data}
\title{
Fraction Subtraction Dataset 1
}
\description{
This is the fraction subtraction data set with 536 students
and 15 items. The Q-matrix was defined in de la Torre (2009).
}
\usage{data(data.fraction1)}
\format{
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
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
See \code{\link{fraction.subtraction.data}} for more information
about data source.
}


\seealso{
\code{\link[GDINA:frac20]{GDINA::frac20}}
}


\references{
  de la Torre, J. (2009). DINA model parameter estimation:
  A didactic. \emph{Journal of Educational and Behavioral
  Statistics}, \bold{34}, 115--130.
}
%\examples{
%% data(data.fraction1)
%}
\keyword{datasets}
