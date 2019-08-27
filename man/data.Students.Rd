%% File Name: data.Students.Rd
%% File Version: 0.198

\name{data.Students}
\alias{data.Students}
\docType{data}
\title{
Dataset Student Questionnaire
}
\description{
This dataset contains item responses of students at
a scale of cultural activities (\code{act}),
mathematics self concept (\code{sc}) and
mathematics joyment (\code{mj}).
}
\usage{data(data.Students)}
\format{
  A data frame with 2400 observations on the following 15 variables.
  \describe{
   \item{\code{urban}}{Urbanization level: 1=town, 0=otherwise}
    \item{\code{female}}{A dummy variable for female student}
    \item{\code{act1}}{Visit a museum (0=never, 1=once or twice a year,
          2=more than twice a year)}
    \item{\code{act2}}{Visit a theater or classical concert (0,1,2)}
    \item{\code{act3}}{Visit a rock or pop concert (0,1,2)}
    \item{\code{act4}}{Visit a cinema (0,1,2)}
    \item{\code{act5}}{Visit a public library (0,1,2)}
    \item{\code{sc1}}{Item 1 self concept "I am usually good at math."
    (0=do not agree at all,
    1=rather do not agree, 2=rather agree, 3=completely agree)}
    \item{\code{sc2}}{Item 2 self concept: "Mathematics is harder for me than many of my
    classmates." (0,1,2,3) (reversed)}
    \item{\code{sc3}}{Item 3 self concept: "I am just not good at math."
    (0,1,2,3) (reversed)}
    \item{\code{sc4}}{Item 4 self concept: "I'm learning fast in math." (0,1,2,3)}
    \item{\code{mj1}}{Item 1 mathematics joyment:
    "I would like more math at school." (0,1,2,3)}
    \item{\code{mj2}}{Item 2 mathematics joyment:
    "I like to learn mathematics." (0,1,2,3)}
    \item{\code{mj3}}{Item 3 mathematics joyment:
    "Math is boring." (0,1,2,3) (reversed)}
    \item{\code{mj4}}{Item 4 mathematics joyment: "I like math." (0,1,2,3)}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Subsample of students from an Austrian survey
of 8th grade students.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}



\examples{
\dontrun{
library(psych)
data(data.Students, package="CDM")
psych::describe(data.Students)
  ##          var    n mean   sd median trimmed  mad min max range  skew kurtosis   se
  ##   urban    1 2400 0.31 0.46    0.0    0.27 0.00   0   1     1  0.81    -1.34 0.01
  ##   female   2 2400 0.51 0.50    1.0    0.51 0.00   0   1     1 -0.03    -2.00 0.01
  ##   act1     3 2248 0.65 0.73    0.5    0.56 0.74   0   2     2  0.64    -0.88 0.02
  ##   act2     4 2230 0.47 0.69    0.0    0.34 0.00   0   2     2  1.13    -0.06 0.01
  ##   act3     5 2218 0.33 0.60    0.0    0.21 0.00   0   2     2  1.62     1.48 0.01
  ##   act4     6 2342 1.35 0.76    2.0    1.44 0.00   0   2     2 -0.69    -0.96 0.02
  ##   act5     7 2223 0.52 0.74    0.0    0.40 0.00   0   2     2  1.05    -0.41 0.02
  ##   sc1      8 2352 0.96 0.80    1.0    0.91 1.48   0   3     3  0.45    -0.39 0.02
  ##   sc2      9 2347 0.90 0.88    1.0    0.81 1.48   0   3     3  0.66    -0.41 0.02
  ##   sc3     10 2335 0.86 0.96    1.0    0.73 1.48   0   3     3  0.84    -0.35 0.02
  ##   sc4     11 2337 1.29 0.90    1.0    1.24 1.48   0   3     3  0.24    -0.71 0.02
  ##   mj1     12 2351 2.26 0.82    2.0    2.37 1.48   0   3     3 -0.94     0.28 0.02
  ##   mj2     13 2345 1.89 0.91    2.0    1.95 1.48   0   3     3 -0.35    -0.80 0.02
  ##   mj3     14 2334 1.47 1.02    1.0    1.47 1.48   0   3     3  0.10    -1.11 0.02
  ##   mj4     15 2346 1.59 0.99    2.0    1.62 1.48   0   3     3 -0.03    -1.06 0.02
}
}
%% \keyword{datasets}
