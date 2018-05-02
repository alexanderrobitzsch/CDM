%% File Name: data.mg.Rd
%% File Version: 0.23

\name{data.mg}
\alias{data.mg}
\docType{data}
\title{
Large-Scale Dataset with Multiple Groups
}
\description{
Large-scale dataset with multiple groups, survey weights and 11 polytomous
items.
}
\usage{data(data.mg)}
\format{
  A data frame with 38243 observations on the following 14 variables.
  \describe{
    \item{\code{idstud}}{Student identifier}
    \item{\code{group}}{Group identifier}
    \item{\code{weight}}{Survey weight}
    \item{\code{I1}}{Item 1}
    \item{\code{I2}}{Item 2}
    \item{\code{I3}}{Item 3}
    \item{\code{I4}}{Item 4}
    \item{\code{I5}}{Item 5}
    \item{\code{I6}}{Item 6}
    \item{\code{I7}}{Item 7}
    \item{\code{I8}}{Item 8}
    \item{\code{I9}}{Item 9}
    \item{\code{I10}}{Item 10}
    \item{\code{I11}}{Item 11}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Subsample of a large-scale dataset of 11 survey questions.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}


\examples{
\dontrun{
library(psych)
data(dat.mg, package="CDM")
psych::describe( data.mg )
  ##   > psych::describe(data.mg)
  ##          var     n       mean       sd     median    trimmed      mad        min        max
  ##   idstud   1 38243 1039653.91 19309.80 1037899.00 1039927.73 30240.59 1007168.00 1069949.00
  ##   group    2 38243       8.06     4.07       7.00       8.06     5.93       2.00      14.00
  ##   weight   3 38243      28.76    19.25      31.88      27.92    19.12       0.79     191.89
  ##   I1       4 37665       0.88     0.32       1.00       0.98     0.00       0.00       1.00
  ##   I2       5 37639       0.93     0.25       1.00       1.00     0.00       0.00       1.00
  ##   I3       6 37473       0.76     0.43       1.00       0.83     0.00       0.00       1.00
  ##   I4       7 37687       1.88     0.39       2.00       2.00     0.00       0.00       2.00
  ##   I5       8 37638       1.36     0.75       2.00       1.44     0.00       0.00       2.00
  ##   I6       9 37587       1.05     0.82       1.00       1.06     1.48       0.00       2.00
  ##   I7      10 37576       1.55     0.85       2.00       1.57     1.48       0.00       3.00
  ##   I8      11 37044       0.45     0.50       0.00       0.44     0.00       0.00       1.00
  ##   I9      12 37249       0.48     0.50       0.00       0.47     0.00       0.00       1.00
  ##   I10     13 37318       0.63     0.48       1.00       0.66     0.00       0.00       1.00
  ##   I11     14 37412       1.35     0.80       1.00       1.35     1.48       0.00       3.00
}
}


\keyword{datasets}
