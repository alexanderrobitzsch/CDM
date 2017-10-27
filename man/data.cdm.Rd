%% File Name: data.cdm.Rd
%% File Version: 0.14

\name{data.cdm}

\alias{data.cdm}
\alias{data.cdm01}
\alias{data.cdm02}
\alias{data.cdm03}
\alias{data.cdm04}

\docType{data}
\title{
Several Datasets for the \pkg{CDM} Package
}

\description{
Several datasets for the \pkg{CDM} package
}

\usage{
data(data.cdm01)
data(data.cdm02)
data(data.cdm03)
data(data.cdm04)
}


\format{
\itemize{

\item Dataset \code{data.cdm01}

This dataset is a multiple choice dataset and used in the \code{\link{mcdina}}
function. The format is:

\code{List of 3} \cr
\code{ $ data    :'data.frame':} \cr
\code{  ..$ I1 : int [1:5003] 3 3 4 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ I2 : int [1:5003] 1 1 3 1 1 2 1 1 2 1 ...} \cr
\code{  ..$ I3 : int [1:5003] 4 3 2 3 2 2 2 2 1 2 ...} \cr
\code{  ..$ I4 : int [1:5003] 3 3 3 2 2 2 2 3 3 1 ...} \cr
\code{  ..$ I5 : int [1:5003] 2 2 2 3 1 1 2 3 2 1 ...} \cr
\code{  ..$ I6 : int [1:5003] 3 1 1 1 1 2 1 1 1 1 ...} \cr
\code{  ..$ I7 : int [1:5003] 1 1 2 2 1 3 1 1 1 3 ...} \cr
\code{  ..$ I8 : int [1:5003] 1 1 1 1 1 2 1 4 3 3 ...} \cr
\code{  ..$ I9 : int [1:5003] 3 2 1 1 1 1 3 3 1 3 ...} \cr
\code{  ..$ I10: int [1:5003] 2 1 2 1 1 2 2 2 2 1 ...} \cr
\code{  ..$ I11: int [1:5003] 2 2 2 2 1 2 1 2 1 1 ...} \cr
\code{  ..$ I12: int [1:5003] 1 2 1 1 2 1 1 1 1 2 ...} \cr
\code{  ..$ I13: int [1:5003] 2 1 1 1 2 1 2 2 1 1 ...} \cr
\code{  ..$ I14: int [1:5003] 1 1 1 1 1 2 1 1 2 1 ...} \cr
\code{  ..$ I15: int [1:5003] 1 2 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ I16: int [1:5003] 1 2 2 1 2 2 2 1 1 1 ...} \cr
\code{  ..$ I17: int [1:5003] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ group   : int [1:5003] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ q.matrix:'data.frame':} \cr
\code{  ..$ item : int [1:52] 1 1 1 1 2 2 2 2 3 3 ...} \cr
\code{  ..$ categ: int [1:52] 1 2 3 4 1 2 3 4 1 2 ...} \cr
\code{  ..$ A1   : int [1:52] 0 1 0 1 0 1 1 1 0 0 ...} \cr
\code{  ..$ A2   : int [1:52] 0 0 1 1 0 0 0 1 0 0 ...} \cr
\code{  ..$ A3   : int [1:52] 0 0 0 0 0 0 0 0 0 0 ...} \cr

\item Dataset \code{data.cdm02}

Multiple choice dataset with a Q-matrix designed for polytomous
attributes.

\code{List of 2} \cr
\code{ $ data    :'data.frame':} \cr
\code{  ..$ I1 : int [1:3000] 3 3 4 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ I2 : int [1:3000] 1 1 3 1 1 2 1 1 2 1 ...} \cr
\code{  ..$ I3 : int [1:3000] 4 3 2 3 2 2 2 2 1 2 ...} \cr
\code{  [...] } \cr
\code{  ..$ B17: num [1:3000] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ B18: num [1:3000] 1 1 1 1 2 2 2 2 2 2 ...} \cr
\code{ $ q.matrix:'data.frame':} \cr
\code{  ..$ item : int [1:100] 1 1 1 1 2 2 2 2 3 3 ...} \cr
\code{  ..$ categ: int [1:100] 1 2 3 4 1 2 3 4 1 2 ...} \cr
\code{  ..$ A1   : num [1:100] 0 1 0 1 0 1 1 1 0 0 ...} \cr
\code{  ..$ A2   : num [1:100] 0 0 1 1 0 0 0 1 0 0 ...} \cr
\code{  ..$ A3   : num [1:100] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ B1   : num [1:100] 0 0 0 0 0 0 0 0 0 0 ...} \cr

\item Dataset \code{data.cdm03}:

This dataset is a resimulated dataset from Chiu, Koehn & Wu (2016) where
the data generating model is a reduced RUM model. See Example 1.

\code{List of 2} \cr
\code{ $ data   : num [1:725, 1:16] 0 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:16] "I01" "I02" "I03" "I04" ...} \cr
\code{ $ qmatrix:'data.frame':        16 obs. of  6 variables:} \cr
\code{  ..$ item: Factor w/ 16 levels "I01","I02","I03",..: 1 2 3 4 5 6 7 8 9 10 ...} \cr
\code{  ..$ A1  : int [1:16] 1 0 0 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ A2  : int [1:16] 0 1 0 0 1 1 0 0 0 0 ...} \cr
\code{  ..$ A3  : int [1:16] 0 0 1 1 1 1 0 0 0 0 ...} \cr
\code{  ..$ A4  : int [1:16] 0 0 0 0 0 0 1 1 1 1 ...} \cr
\code{  ..$ A5  : int [1:16] 0 0 0 0 0 0 0 0 0 0 ...} \cr


\item Dataset \code{data.cdm04}:

Simulated dataset for the sequential DINA model 
(as described in Ma & de la Torre, 2016).
The dataset contains 1000 persons and 12 items which measure 2 skills.

\code{List of 3} \cr
\code{ $ data     : num [1:1000, 1:12] 0 0 0 1 1 0 0 0 0 0 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:12] "I1" "I2" "I3" "I4" ...} \cr
\code{ $ q.matrix1:'data.frame':      18 obs. of  4 variables:} \cr
\code{  ..$ Item: chr [1:18] "I1" "I2" "I3" "I4" ...} \cr
\code{  ..$ Cat : int [1:18] 1 1 1 1 1 1 1 2 1 2 ...} \cr
\code{  ..$ A1  : int [1:18] 1 1 1 0 0 0 1 1 1 1 ...} \cr
\code{  ..$ A2  : int [1:18] 0 0 0 1 1 1 0 0 0 0 ...} \cr
\code{ $ q.matrix2:'data.frame':      18 obs. of  4 variables:} \cr
\code{  ..$ Item: chr [1:18] "I1" "I2" "I3" "I4" ...} \cr
\code{  ..$ Cat : int [1:18] 1 1 1 1 1 1 1 2 1 2 ...} \cr
\code{  ..$ A1  : num [1:18] 1 1 1 0 0 0 1 1 1 1 ...} \cr
\code{  ..$ A2  : num [1:18] 0 0 0 1 1 1 0 0 0 0 ...} \cr

}
  
}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

\references{
Chiu, C.-Y., Koehn, H.-F., & Wu, H.-M. (2016).
Fitting the reduced RUM with Mplus: A tutorial. 
\emph{International Journal of Testing, 16}(4), 331-351.	

Ma, W., & de la Torre, J. (2016).
A sequential cognitive diagnosis model for polytomous responses.
\emph{British Journal of Mathematical and Statistical Psychology, 69}(3),
253-275.
}



\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Reduced RUM model, Chiu et al. (2016)
#############################################################################

data(data.cdm03)
dat <- data.cdm03$data
qmatrix <- data.cdm03$qmatrix

#*** Model 1: Reduced RUM
mod1 <- CDM::gdina( dat , q.matrix = qmatrix[,-1], rule="RRUM" )
summary(mod1)

#*** Model 2: Additive model with identity link function
mod2 <- CDM::gdina( dat , q.matrix = qmatrix[,-1], rule="ACDM" )
summary(mod2)

#*** Model 3: Additive model with logit link function
mod3 <- CDM::gdina( dat , q.matrix = qmatrix[,-1], rule="ACDM", linkfct="logit")
summary(mod3)
}
}

\keyword{datasets}
