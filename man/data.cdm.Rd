%% File Name: data.cdm.Rd
%% File Version: 0.41

\name{data.cdm}

\alias{data.cdm}
\alias{data.cdm01}
\alias{data.cdm02}
\alias{data.cdm03}
\alias{data.cdm04}
\alias{data.cdm05}
\alias{data.cdm06}
\alias{data.cdm07}
\alias{data.cdm08}

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
data(data.cdm05)
data(data.cdm06)
data(data.cdm07)
data(data.cdm08)
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

This is a resimulated dataset from Chiu, Koehn and Wu (2016) where
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


\item Dataset \code{data.cdm05}:

Example dataset used in Philipp, Strobl, de la Torre and Zeileis (2018).
This dataset is a sub-dataset of the \code{probability} dataset in
the \pkg{pks} package (Heller & Wickelmaier, 2013).

\code{List of 3} \cr
\code{ $ data    :'data.frame':       504 obs. of  12 variables:} \cr
\code{  ..$ b101: num [1:504] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ b102: num [1:504] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ b103: num [1:504] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ b104: num [1:504] 1 1 1 1 0 1 0 0 0 1 ...} \cr
\code{  ..$ b105: num [1:504] 1 0 1 1 1 1 0 1 1 1 ...} \cr
\code{  ..$ b106: num [1:504] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ b107: num [1:504] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ b108: num [1:504] 1 1 1 1 1 1 0 1 1 1 ...} \cr
\code{  ..$ b109: num [1:504] 1 1 0 1 1 0 0 1 1 0 ...} \cr
\code{  ..$ b110: num [1:504] 0 0 0 1 0 0 0 0 0 1 ...} \cr
\code{  ..$ b111: num [1:504] 0 1 0 0 0 1 0 0 0 0 ...} \cr
\code{  ..$ b112: num [1:504] 1 1 0 1 0 1 0 1 0 0 ...} \cr
\code{ $ q.matrix:'data.frame':       12 obs. of  4 variables:} \cr
\code{  ..$ pb: num [1:12] 1 0 0 0 1 1 1 1 1 0 ...} \cr
\code{  ..$ cp: num [1:12] 0 1 0 0 1 1 0 0 0 1 ...} \cr
\code{  ..$ un: num [1:12] 0 0 1 0 0 0 1 1 0 0 ...} \cr
\code{  ..$ id: num [1:12] 0 0 0 1 0 0 0 0 1 1 ...} \cr
\code{ $ skills  : Named chr [1:4] "how to calculate the classic probability "} \cr
\code{  ..- attr(*, "names")= chr [1:4] "pb" "cp" "un" "id"} \cr


\item Dataset \code{data.cdm06}:

Resimulated example dataset from Chen and Chen (2017).

\code{List of 3} \cr
\code{ $ data    :'data.frame':       2733 obs. of  15 variables:} \cr
\code{  ..$ I01: num [1:2733] 1 0 0 1 0 0 0 1 1 1 ...} \cr
\code{  ..$ I02: num [1:2733] 1 0 0 1 1 0 1 0 0 1 ...} \cr
\code{  ..$ I03: num [1:2733] 0 0 0 1 1 0 1 0 1 0 ...} \cr
\code{  ..$ I04: num [1:2733] 1 1 0 0 0 0 1 1 1 0 ...} \cr
\code{  ..$ I05: num [1:2733] 1 0 1 1 0 1 1 1 1 1 ...} \cr
\code{  ..$ I06: num [1:2733] 0 0 0 1 1 0 0 0 1 1 ...} \cr
\code{  ..$ I07: num [1:2733] 1 1 1 0 0 1 1 0 1 1 ...} \cr
\code{  ..$ I08: num [1:2733] 0 0 0 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ I09: num [1:2733] 1 0 0 1 1 1 0 1 0 1 ...} \cr
\code{  ..$ I10: num [1:2733] 0 0 0 1 0 1 1 0 1 1 ...} \cr
\code{  ..$ I11: num [1:2733] 0 1 0 1 1 1 1 0 1 1 ...} \cr
\code{  ..$ I12: num [1:2733] 0 1 0 1 0 0 0 1 1 1 ...} \cr
\code{  ..$ I13: num [1:2733] 0 0 1 1 0 1 0 0 0 1 ...} \cr
\code{  ..$ I14: num [1:2733] 0 0 0 1 1 0 1 1 0 0 ...} \cr
\code{  ..$ I15: num [1:2733] 0 0 0 1 0 0 1 0 1 1 ...} \cr
\code{ $ q.matrix:'data.frame':       15 obs. of  5 variables:} \cr
\code{  ..$ RI: num [1:15] 1 1 1 0 1 1 1 1 0 0 ...} \cr
\code{  ..$ JS: num [1:15] 1 0 0 1 0 0 0 0 0 1 ...} \cr
\code{  ..$ GI: num [1:15] 0 1 0 1 0 0 1 1 1 1 ...} \cr
\code{  ..$ II: num [1:15] 0 1 1 0 1 0 1 0 0 0 ...} \cr
\code{  ..$ MI: num [1:15] 0 0 1 0 0 0 0 0 1 0 ...} \cr
\code{ $ skills  : chr [1:5, 1:2] "Retrieving explicit information "  ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:5] "RI" "JS" "GI" "II" ...} \cr
\code{  .. ..$ : chr [1:2] "skill" "description"} \cr


\item Dataset \code{data.cdm07}:

This is a resimulated dataset from the social anxiety disorder data
concerning social phobia which involve 13 dichotomous questions
(Fang, Liu & Ling, 2017). The simulation was based on a latent class
model with five classes. The dataset was also used in Chen, Li, Liu
and Ying (2017).

\code{ $ data    : num [1:863, 1:13] 1 0 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:13] "I1" "I2" "I3" "I4" ...} \cr
\code{ $ q.matrix: num [1:13, 1:3] 1 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:13] "I1" "I2" "I3" "I4" ...} \cr
\code{  .. ..$ : chr [1:3] "A1" "A2" "A3"} \cr
\code{ $ items   : atomic [1:13] 1 speaking in front of other people? ...} \cr
\code{  ..- attr(*, "stem")= chr "Have you ever had a strong fear or avoidance of ..."} \cr

\item Dataset \code{data.cdm08}:

This is a simulated dataset involving four skills and three misconceptions
for the model for simultaneously identifying skills and
misconceptions (SISM; Kuo, Chen & de la Torre, 2018). The Q-matrix follows
the specification in their simulation study.

\code{List of 2} \cr
\code{ $ data    :'data.frame':       1300 obs. of  20 variables:} \cr
\code{  ..$ I01: num [1:1300] 1 0 0 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ I02: num [1:1300] 0 0 0 0 1 1 1 1 1 1 ...} \cr
\code{  ..$ I03: num [1:1300] 0 0 0 0 1 1 1 1 1 1 ...} \cr
\code{  ..$ I04: num [1:1300] 1 1 0 1 0 1 1 0 1 1 ...} \cr
\code{  ..$ I05: num [1:1300] 1 1 1 0 1 1 0 1 1 1 ...} \cr
\code{  ..[...] } \cr
\code{  ..$ I18: num [1:1300] 0 1 0 0 0 0 0 0 0 1 ...} \cr
\code{  ..$ I19: num [1:1300] 1 1 0 0 0 0 0 1 1 1 ...} \cr
\code{  ..$ I20: num [1:1300] 1 1 0 0 0 1 0 1 0 1 ...} \cr
\code{ $ q.matrix:'data.frame':       20 obs. of  7 variables:} \cr
\code{  ..$ S1: num [1:20] 1 0 0 0 0 0 0 1 0 0 ...} \cr
\code{  ..$ S2: num [1:20] 0 1 0 0 0 0 0 0 1 0 ...} \cr
\code{  ..$ S3: num [1:20] 0 0 1 0 0 0 0 0 0 1 ...} \cr
\code{  ..$ S4: num [1:20] 0 0 0 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ B1: num [1:20] 0 0 0 0 1 0 0 1 1 0 ...} \cr
\code{  ..$ B2: num [1:20] 0 0 0 0 0 1 0 0 0 0 ...} \cr
\code{  ..$ B3: num [1:20] 0 0 0 0 0 0 1 0 0 1 ...} \cr


}

}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

\references{
Chen, H., & Chen, J. (2017). Cognitive diagnostic research on chinese
students' English listening skills and implications on skill training.
\emph{English Language Teaching, 10}(12), 107-115.

Chen, Y., Li, X., Liu, J., & Ying, Z. (2017).
Regularized latent class analysis with application in cognitive diagnosis.
\emph{Psychometrika, 82}, 660-692.

Chiu, C.-Y., Koehn, H.-F., & Wu, H.-M. (2016).
Fitting the reduced RUM with Mplus: A tutorial.
\emph{International Journal of Testing, 16}(4), 331-351.

Fang, G., Liu, J., & Ying, Z. (2017). On the identifiability of
diagnostic classification models. \emph{arXiv}, 1706.01240.

Heller, J. and Wickelmaier, F. (2013). Minimum discrepancy estimation in
probabilistic knowledge structures.
\emph{Electronic Notes in Discrete Mathematics, 42}, 49-56.

Kuo, B.-C., Chen, C.-H., & de la Torre, J. (2018).
A cognitive diagnosis model for identifying coexisting skills and misconceptions.
\emph{Applied Psychological Measurement, 42}(3), 179-191.

Ma, W., & de la Torre, J. (2016).
A sequential cognitive diagnosis model for polytomous responses.
\emph{British Journal of Mathematical and Statistical Psychology, 69}(3),
253-275.

Philipp, M., Strobl, C., de la Torre, J., & Zeileis, A. (2018).
On the estimation of standard errors in cognitive diagnosis models.
\emph{Journal of Educational and Behavioral Statistics, 43}(1), 88-115.
}



\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Reduced RUM model, Chiu et al. (2016)
#############################################################################

data(data.cdm03, package="CDM")
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

#############################################################################
# EXAMPLE 2: GDINA model - probability dataset from the pks package
#############################################################################

data(data.cdm05, package="CDM")
dat <- data.cdm05$data
Q <- data.cdm05$q.matrix

#* estimate model
mod1 <- CDM::gdina( dat, q.matrix=Q )
summary(mod1)
}
}

\keyword{datasets}
