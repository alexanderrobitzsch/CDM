%% File Name: data.timss07.G4.lee.Rd
%% File Version: 0.29

\name{data.timss07.G4.lee}
\alias{data.timss07.G4.lee}
\alias{data.timss07.G4.py}
\alias{data.timss07.G4.Qdomains}

\docType{data}
\title{
TIMSS 2007 Mathematics 4th Grade (Lee et al., 2011)
}
\description{
TIMSS 2007 (Grade 4) dataset with 25 mathematics (dichotomized) items used
in Lee, Park and Taylan (2011), Park and Lee (2014) and Park, Xing and
Lee (xxxx). The dataset includes a sample of 698 Austrian students.
}

\usage{
data(data.timss07.G4.lee)
data(data.timss07.G4.py)
data(data.timss07.G4.Qdomains)
}



\format{

\itemize{

\item The dataset \code{data.timss07.G4.lee} is a
list containing dichotomous item responses (\code{data};
information on booklet and gender included),
the Q-matrix (\code{q.matrix}) and descriptions
of the skills (\code{skillinfo}) used in Lee et al. (2011).

The format is:

\code{List of 3} \cr
\code{ $ data     :'data.frame':} \cr
\code{  ..$ idstud  : int [1:698] 10110 10111 20105 20106 30203 30204 40106 40107 60111 60112 ...} \cr
\code{  ..$ idbook  : int [1:698] 4 5 4 5 4 5 4 5 4 5 ...} \cr
\code{  ..$ girl    : int [1:698] 0 0 1 1 0 1 0 1 1 1 ...} \cr
\code{  ..$ M041052 : num [1:698] 1 NA 1 NA 0 NA 1 NA 1 NA ...} \cr
\code{  ..$ M041056 : num [1:698] 1 NA 0 NA 0 NA 0 NA 1 NA ...} \cr
\code{  ..$ M041069 : num [1:698] 0 NA 0 NA 0 NA 0 NA 1 NA ...} \cr
\code{  ..$ M041076 : num [1:698] 1 NA 0 NA 1 NA 1 NA 0 NA ...} \cr
\code{  ..$ M041281 : num [1:698] 1 NA 0 NA 1 NA 1 NA 0 NA ...} \cr
\code{  ..$ M041164 : num [1:698] 1 NA 1 NA 0 NA 1 NA 1 NA ...} \cr
\code{  ..$ M041146 : num [1:698] 0 NA 0 NA 1 NA 1 NA 0 NA ...} \cr
\code{  ..$ M041152 : num [1:698] 1 NA 1 NA 1 NA 0 NA 1 NA ...} \cr
\code{  ..$ M041258A: num [1:698] 0 NA 1 NA 1 NA 0 NA 1 NA ...} \cr
\code{  ..$ M041258B: num [1:698] 1 NA 0 NA 1 NA 0 NA 1 NA ...} \cr
\code{  ..$ M041131 : num [1:698] 0 NA 0 NA 1 NA 1 NA 1 NA ...} \cr
\code{  ..$ M041275 : num [1:698] 1 NA 0 NA 0 NA 1 NA 1 NA ...} \cr
\code{  ..$ M041186 : num [1:698] 1 NA 0 NA 1 NA 1 NA 0 NA ...} \cr
\code{  ..$ M041336 : num [1:698] 1 NA 1 NA 0 NA 1 NA 0 NA ...} \cr
\code{  ..$ M031303 : num [1:698] 1 1 0 1 0 1 1 1 0 0 ...} \cr
\code{  ..$ M031309 : num [1:698] 1 0 1 1 1 1 1 1 0 0 ...} \cr
\code{  ..$ M031245 : num [1:698] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ M031242A: num [1:698] 1 1 0 1 1 1 1 1 0 0 ...} \cr
\code{  ..$ M031242B: num [1:698] 0 1 0 1 1 1 1 1 1 0 ...} \cr
\code{  ..$ M031242C: num [1:698] 1 1 0 1 1 1 1 1 1 0 ...} \cr
\code{  ..$ M031247 : num [1:698] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ M031219 : num [1:698] 1 1 1 0 1 1 1 1 1 0 ...} \cr
\code{  ..$ M031173 : num [1:698] 1 1 0 0 0 1 1 1 1 0 ...} \cr
\code{  ..$ M031085 : num [1:698] 1 0 0 1 1 1 0 0 0 1 ...} \cr
\code{  ..$ M031172 : num [1:698] 1 0 0 1 1 1 1 1 1 0 ...} \cr
\code{ $ q.matrix : int [1:25, 1:15] 1 0 0 0 0 0 0 1 0 0 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:25] "M041052" "M041056" "M041069" "M041076" ...} \cr
\code{  .. ..$ : chr [1:15] "NWN01" "NWN02" "NWN03" "NWN04" ...} \cr
\code{ $ skillinfo:'data.frame':} \cr
\code{  ..$ skillindex      : int [1:15] 1 2 3 4 5 6 7 8 9 10 ...} \cr
\code{  ..$ skill           : Factor w/ 15 levels "DOR15","DRI13",..: 12 13 14 15 8 9 10 11 4 6 ...} \cr
\code{  ..$ content         : Factor w/ 3 levels "D","G","N": 3 3 3 3 3 3 3 3 2 2 ...} \cr
\code{  ..$ content_label   : Factor w/ 3 levels "Data Display",..: 3 3 3 3 3 3 3 3 2 2 ...} \cr
\code{  ..$ subcontent      : Factor w/ 9 levels "FD","LA","LM",..: 9 9 9 9 1 1 4 6 2 8 ...} \cr
\code{  ..$ subcontent_label: Factor w/ 9 levels "Fractions and Decimals",..: 9 9 9 9 1 1 4 6 2 8 ...} \cr


\item The dataset \code{data.timss07.G4.py} uses the same items as
\code{data.timss07.G4.lee} but employs a simplified Q-matrix with 7 skills.
This Q-matrix was used in Park and Lee (2014) and Park et al. (xxxx).

\code{List of 3} \cr
\code{ $ q.matrix:'data.frame':       25 obs. of  7 variables:} \cr
\code{  ..$ N1: num [1:25] 1 0 1 1 1 0 0 1 0 0 ...} \cr
\code{  ..$ N2: num [1:25] 0 1 1 1 0 0 0 0 0 0 ...} \cr
\code{  ..$ N3: num [1:25] 0 0 0 0 1 0 0 0 0 0 ...} \cr
\code{  ..$ G4: num [1:25] 0 0 0 0 0 0 1 0 0 1 ...} \cr
\code{  ..$ G5: num [1:25] 0 0 0 0 0 1 1 1 1 1 ...} \cr
\code{  ..$ G6: num [1:25] 0 0 0 0 0 1 1 0 0 0 ...} \cr
\code{  ..$ D7: num [1:25] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ domains : Named chr [1:3] "Number" "Geometric Shapes and Measures" "Data Display"} \cr
\code{  ..- attr(*, "names")= chr [1:3] "N" "G" "D"} \cr
\code{ $ skills  : Named chr [1:7] "Whole Numbers" ...} \cr
\code{  ..- attr(*, "names")= chr [1:7] "N1" "N2" "N3" "G4" ...} \cr


\item The Q-matrix \code{data.timss07.G4.Qdomains} is a simplification
of \code{data.timss07.G4.py$q.matrix} to 3 domains and involves a
simple structure of skills.

\code{ num [1:25, 1:3] 1 1 1 1 1 0 0 1 0 0 ...} \cr
\code{ - attr(*, "dimnames")=List of 2} \cr
\code{  ..$ : chr [1:25] "M041052" "M041056" "M041069" "M041076" ...} \cr
\code{  ..$ : chr [1:3] "N" "G" "D"} \cr

}

}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
TIMSS 2007 study, 4th Grade, Austrian sample on booklets 4 and 5
}

\seealso{
A comparison of several countries based on the 25 items is conducted in
Yamaguchi and Okada (2018).
}


\references{
Lee, Y. S., Park, Y. S., & Taylan, D. (2011).
A cognitive diagnostic modeling of attribute mastery in Massachusetts,
Minnesota, and the US national sample using the TIMSS 2007.
\emph{International Journal of Testing, 11}, 144-177.

Park, Y. S., & Lee, Y. S. (2014). An extension of the DINA model using
covariates: Examining factors affecting response probability and latent
classification. \emph{Applied Psychological Measurement, 38}(5), 376-390.

Park, Y. S., Xing, K., & Lee, Y. S. (xxxx). Explanatory cognitive
diagnostic models: Incorporating latent and observed predictors.
\emph{Applied Psychological Measurement, xx}(xx), xxx-xxx.

Yamaguchi, K., & Okada, K. (2018). Comparison among cognitive diagnostic models
for the TIMSS 2007 fourth grade mathematics assessment.
\emph{PloS ONE, 13}(2), e0188691.
}



\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: DINA model Lee et al. (2011) - 15 skills
#############################################################################

data(data.timss07.G4.lee, package="CDM")
dat <- data.timss07.G4.lee$data
q.matrix <- data.timss07.G4.lee$q.matrix
# extract items
items <- grep( "M0", colnames(dat), value=TRUE )

#*** Model 1: estimate DINA model
mod1 <- CDM::din( dat[,items], q.matrix )
summary(mod1)

#############################################################################
# EXAMPLE 2: DINA models Park and Lee (2014) - 7 skills and 3 skills
#############################################################################

data(data.timss07.G4.lee, package="CDM")
data(data.timss07.G4.py, package="CDM")
data(data.timss07.G4.Qdomains, package="CDM")

dat <- data.timss07.G4.lee$data
q.matrix <- data.timss07.G4.py$q.matrix
items <- rownames(q.matrix)

#*** Model 1: estimate DINA model
mod1 <- CDM::din( dat[,items], q.matrix )
summary(mod1)

#*** Model 2: estimate DINA model with Q-matrix defined by domains
Q <- data.timss07.G4.Qdomains
mod2 <- CDM::din( dat[,items], q.matrix=Q )
summary(mod2)
}
}


\keyword{datasets}
