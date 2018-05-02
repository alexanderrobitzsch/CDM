%% File Name: data.pisa00R.Rd
%% File Version: 0.09

\name{data.pisa00R}
\alias{data.pisa00R.ct}
\alias{data.pisa00R.cc}

\docType{data}

\title{
PISA 2000 Reading Study (Chen & de la Torre, 2014)
}


\description{
This is a sub-dataset of the PISA 2000 of German students
including 26 items of the reading test. The 26 items was analyzed in
Chen and de la Torre (2014) and a subset of 20 items was
analyzed in Chen and Chen (2016).
}


\usage{
data(data.pisa00R.ct)
data(data.pisa00R.cc)
}


\format{
\itemize{

\item The format of the dataset \code{data.pisa00R.ct}
(Chen & de la Torre, 2014) is:

\code{List of 3} \cr
\code{ $ data    :'data.frame':       1095 obs. of  111 variables:} \cr
\code{  .. [list output truncated]} \cr
\code{ $ q.matrix: num [1:26, 1:8] 0 1 0 0 0 1 0 0 0 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{ $ skills  : chr [1:8] "Locating information"  ...} \cr

\item The format of the dataset \code{data.pisa00R.cc}
(Q-matrix in Chen and Chen, 2016)

\code{List of 2} \cr
\code{ $ q.matrix:'data.frame':       20 obs. of  5 variables:} \cr
\code{  ..$ A1: num [1:20] 1 1 0 0 1 1 1 0 0 0 ...} \cr
\code{  ..$ A2: num [1:20] 0 0 0 1 0 1 1 1 1 1 ...} \cr
\code{  ..$ A3: num [1:20] 1 1 0 1 1 0 1 0 1 0 ...} \cr
\code{  ..$ A4: num [1:20] 0 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ A5: num [1:20] 0 0 1 0 0 0 0 1 0 1 ...} \cr
\code{ $ skills  : Named chr [1:5] "Identifying Explicit Information"  ...} \cr
\code{  ..- attr(*, "names")= chr [1:5] "A1" "A2" "A3" "A4" ...} \cr

}

}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}


%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

\references{
Chen, H., & Chen, J. (2016). Exploring reading comprehension skill
relationships through the G-DINA model.
\emph{Educational Psychology, 36}(6), 1049-1064.

Chen, J., & de la Torre, J. (2014). A procedure for diagnostically modeling
extant large-scale assessment data: the case of the programme for international
student assessment in reading. \emph{Psychology, 5}(18), 1967-1978.
}

\examples{
#############################################################################
# EXAMPLE 1: PISA items from Chen and de la Torre (2014)
#            dichotomize item responses
#############################################################################

data(data.pisa00R.ct, package="CDM")
dat <- data.pisa00R.ct$data
Q <- data.pisa00R.ct$q.matrix
resp <- dat[, rownames(Q)]

#** extract item-wise maximum
maxK <- apply( resp, 2, max, na.rm=TRUE )
#** dichotomize response data
resp1 <- resp
for (ii in seq(1,ncol(resp)) ){
    resp1[,ii] <- 1 * ( resp[,ii] == maxK[ii] )
}
}


\keyword{datasets}
