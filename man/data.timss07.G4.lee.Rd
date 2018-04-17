%% File Name: data.timss07.G4.lee.Rd
%% File Version: 0.08

\name{data.timss07.G4.lee}
\alias{data.timss07.G4.lee}
\docType{data}
\title{
TIMSS 2007 Mathematics 4th Grade (Lee et al., 2011)
}
\description{
TIMSS 2007 (Grade 4) dataset with a subset of 25 mathematics items used
in Lee et al. (2011). The dataset includes a sample of 698 Austrian students.
}

\usage{
data(data.timss07.G4.lee)
}



\format{
The dataset is a list of item responses (\code{data}; information on booklet 
and gender included), the Q-matrix (\code{q.matrix}) and descriptions
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
  
}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
TIMSS 2007 study, 4th Grade, Austrian sample on booklets 4 and 5
}

\references{
Lee, Y. S., Park, Y. S., & Taylan, D. (2011). 
A cognitive diagnostic modeling of attribute mastery in Massachusetts, 
Minnesota, and the US national sample using the TIMSS 2007. 
\emph{International Journal of Testing, 11}, 144-177.
}



\examples{
\dontrun{
data(data.timss07.G4.lee, package="CDM")

dat <- data.timss07.G4.lee$data
q.matrix <- data.timss07.G4.lee$q.matrix

items <- grep( "M0" , colnames(dat) , value=TRUE )

#*** Model 1: estimate DINA model
mod1 <- CDM::din( dat[,items] , q.matrix )
}
}


\keyword{datasets}
