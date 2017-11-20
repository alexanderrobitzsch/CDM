%% File Name: data.timss03.G8.su.Rd
%% File Version: 0.07

\name{data.timss03.G8.su}
\alias{data.timss03.G8.su}
\docType{data}

\title{
TIMSS 2003 Mathematics 8th Grade (Su et al., 2013)
}

\description{
This is a dataset with a subset of 23 mathematics items from TIMSS 2003 items 
used in Su et al. (2013).
}


\usage{
data(data.timss03.G8.su)
}



\format{
The data contains scored item responses (\code{data}),
the Q-matrix (\code{q.matrix}) and further item informations (\code{iteminfo}).

The format is

\code{List of 3} \cr
\code{ $ data    :'data.frame':} \cr
\code{  ..$ idstud  : num [1:757] 1e+07 1e+07 1e+07 1e+07 1e+07 ...} \cr
\code{  ..$ idbook  : num [1:757] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ M012001 : num [1:757] 0 1 0 0 1 0 1 0 0 0 ...} \cr
\code{  ..$ M012002 : num [1:757] 1 1 0 1 0 0 1 1 1 1 ...} \cr
\code{  ..$ M012004 : num [1:757] 0 1 1 1 1 0 1 1 0 0 ...} \cr
\code{   [...]} \cr
\code{  ..$ M022234B: num [1:757] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ M022251 : num [1:757] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ M032570 : num [1:757] 1 1 0 1 0 0 1 1 1 1 ...} \cr
\code{  ..$ M032643 : num [1:757] 1 0 0 0 0 0 1 1 0 0 ...} \cr
\code{ $ q.matrix: int [1:23, 1:13] 1 0 0 0 0 0 1 0 0 0 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:23] "M012001" "M012002" "M012004" "M012016" ...} \cr
\code{  .. ..$ : chr [1:13] "S1" "S2" "S3" "S4" ...} \cr
\code{ $ iteminfo: chr [1:23, 1:9] "M012001" "M012002" "M012004" "M012016" ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:9] "item" "ItemType" "reporting_category" "content" ...} \cr
 
For a detailed description of skills \code{S1}, \code{S2}, ..., \code{S15}
see Su et al. (2013, Table 2).

}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
Subset of US 8th graders (Booklet 1) in the TIMSS 2003 mathematics study 
}

\references{
Su, Y.-L., Choi, K. M., Lee, W.-C., Choi, T., & McAninch, M. (2013).
\emph{Hierarchical cognitive diagnostic analysis for TIMSS 2003 mathematics}.
CASMA Research Report 35. Center for Advanced Studies in
Measurement and Assessment (CASMA), University of Iowa.
}


\examples{
\dontrun{
data(data.timss03.G8.su, package="CDM")

data <- data.timss03.G8.su$data[,-c(1,2)]
q.matrix <- data.timss03.G8.su$q.matrix

#*** Model 1: DINA model with complete skill space of 2^13=8192 skill classes
mod1 <- CDM::din( data , q.matrix )

#*** Model 2: Skill space approximation with 3000 skill classes instead of
#    2^13 = 8192 classes as in Model 1
ss2 <- CDM::skillspace.approximation( L = 3000 , K = ncol(q.matrix) )
mod2 <- CDM::din( data , q.matrix , skillclasses = ss2 )

#*** Model 3: DINA model with a hierarchical skill space
#   see Su et al. (2013): Fig. 6
B <- "S1 > S2 > S7 > S8
      S15 > S9
      S3 > S9
      S13 > S4 > S9
      S14 > S5 > S6 > S11"
# Note that S10 and S12 are not included in the dataset contained in this package
skill.names <- colnames(q.matrix)
ss3 <- CDM::skillspace.hierarchy(B=B , skill.names=skill.names)
# The reduced skill space "only" contains 325 skill classes
mod3 <- CDM::din( data , q.matrix , skillclasses = ss3$skillspace.reduced )
}
}
\keyword{datasets}
