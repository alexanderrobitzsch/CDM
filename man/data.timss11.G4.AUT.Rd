%% File Name: data.timss11.G4.AUT.Rd
%% File Version: 0.19

\name{data.timss11.G4.AUT}
\alias{data.timss11.G4.AUT}
\alias{data.timss11.G4.AUT.part}
\alias{data.timss11.G4.sa}

\docType{data}

\title{
TIMSS 2011 Mathematics 4th Grade Austrian Students
}


\description{
This is the TIMSS 2011 dataset of 4668 Austrian fourth-graders.
See George and Robitzsch (2014, 2015, 2018) for publications using the
TIMSS 2011 dataset for cognitive diagnosis modeling. The dataset has
also been analyzed by Sedat and Arican (2015).
}


\usage{
data(data.timss11.G4.AUT)
data(data.timss11.G4.AUT.part)
data(data.timss11.G4.sa)
}


\format{
\itemize{

\item The format of the dataset \code{data.timss11.G4.AUT} is:

\code{List of 4} \cr
\code{ $ data     :'data.frame':} \cr
\code{  ..$ uidschool: int [1:4668] 10040001 10040001 10040001 10040001 10040001 10040001 10040001 10040001 10040001 10040001 ...} \cr
\code{  ..$ uidstud  : num [1:4668] 1e+13 1e+13 1e+13 1e+13 1e+13 ...} \cr
\code{  ..$ IDCNTRY  : int [1:4668] 40 40 40 40 40 40 40 40 40 40 ...} \cr
\code{  ..$ IDBOOK   : int [1:4668] 10 12 13 14 1 2 3 4 5 6 ...} \cr
\code{  ..$ IDSCHOOL : int [1:4668] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ IDCLASS  : int [1:4668] 102 102 102 102 102 102 102 102 102 102 ...} \cr
\code{  ..$ IDSTUD   : int [1:4668] 10201 10203 10204 10205 10206 10207 10208 10209 10210 10211 ...} \cr
\code{  ..$ TOTWGT   : num [1:4668] 17.5 17.5 17.5 17.5 17.5 ...} \cr
\code{  ..$ HOUWGT   : num [1:4668] 1.04 1.04 1.04 1.04 1.04 ...} \cr
\code{  ..$ SENWGT   : num [1:4668] 0.111 0.111 0.111 0.111 0.111 ...} \cr
\code{  ..$ SCHWGT   : num [1:4668] 11.6 11.6 11.6 11.6 11.6 ...} \cr
\code{  ..$ STOTWGTU : num [1:4668] 524 524 524 524 524 ...} \cr
\code{  ..$ WGTADJ1  : int [1:4668] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ WGTFAC1  : num [1:4668] 11.6 11.6 11.6 11.6 11.6 ...} \cr
\code{  ..$ JKCREP   : int [1:4668] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ JKCZONE  : int [1:4668] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ female   : int [1:4668] 1 0 1 1 1 1 1 1 0 0 ...} \cr
\code{  ..$ M031346A : int [1:4668] NA NA NA 1 1 NA NA NA NA NA ...} \cr
\code{  ..$ M031346B : int [1:4668] NA NA NA 0 0 NA NA NA NA NA ...} \cr
\code{  ..$ M031346C : int [1:4668] NA NA NA 1 1 NA NA NA NA NA ...} \cr
\code{  ..$ M031379  : int [1:4668] NA NA NA 0 0 NA NA NA NA NA ...} \cr
\code{  ..$ M031380  : int [1:4668] NA NA NA 0 0 NA NA NA NA NA ...} \cr
\code{  ..$ M031313  : int [1:4668] NA NA NA 1 1 NA NA NA NA NA ...} \cr
\code{  .. [list output truncated]} \cr
\code{ $ q.matrix1:'data.frame':} \cr
\code{  ..$ item : Factor w/ 174 levels "M031004","M031009",..: 29 30 31 32 33 25 8 5 17 163 ...} \cr
\code{  ..$ Co_DA: int [1:174] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_DK: int [1:174] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_DR: int [1:174] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_GA: int [1:174] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_GK: int [1:174] 0 0 0 0 0 0 1 1 0 0 ...} \cr
\code{  ..$ Co_GR: int [1:174] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_NA: int [1:174] 1 0 0 0 0 1 0 0 0 1 ...} \cr
\code{  ..$ Co_NK: int [1:174] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_NR: int [1:174] 0 1 1 1 1 0 0 0 1 0 ...} \cr
\code{ $ q.matrix2:'data.frame':} \cr
\code{  ..$ item  : Factor w/ 174 levels "M031004","M031009",..: 29 30 31 32 33 25 8 5 17 163 ...} \cr
\code{  ..$ CONT_D: int [1:174] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ CONT_G: int [1:174] 0 0 0 0 0 0 1 1 0 0 ...} \cr
\code{  ..$ CONT_N: int [1:174] 1 1 1 1 1 1 0 0 1 1 ...} \cr
\code{ $ q.matrix3:'data.frame':      174 obs. of  4 variables:} \cr
\code{  ..$ item  : Factor w/ 174 levels "M031004","M031009",..: 29 30 31 32 33 25 8 5 17 163 ...} \cr
\code{  ..$ COGN_A: int [1:174] 1 0 0 0 0 1 0 0 0 1 ...} \cr
\code{  ..$ COGN_K: int [1:174] 0 0 0 0 0 0 1 1 0 0 ...} \cr
\code{  ..$ COGN_R: int [1:174] 0 1 1 1 1 0 0 0 1 0 ...} \cr

\item The dataset \code{data.timss11.G4.AUT.part} is a part of
\code{data.timss11.G4.AUT} and contains only the first
three booklets (with N=1010 students). The format is

\code{List of 4} \cr
\code{ $ data     :'data.frame':      1010 obs. of  109 variables:} \cr
\code{  ..$ uidschool: int [1:1010] 10040001 10040001 10040001 10040001  ...} \cr
\code{  ..$ uidstud  : num [1:1010] 1e+13 1e+13 1e+13 1e+13 1e+13 ...} \cr
\code{  ..$ IDCNTRY  : int [1:1010] 40 40 40 40 40 40 40 40 40 40 ...} \cr
\code{  ..$ IDBOOK   : int [1:1010] 1 2 3 1 2 1 2 3 1 2 ...} \cr
\code{  ..$ IDSCHOOL : int [1:1010] 1 1 1 1 1 2 2 2 3 3 ...} \cr
\code{  ..$ IDCLASS  : int [1:1010] 102 102 102 102 102 ...} \cr
\code{  ..$ IDSTUD   : int [1:1010] 10206 10207 10208 10220 ...} \cr
\code{  ..$ TOTWGT   : num [1:1010] 17.5 17.5 17.5 17.5 17.5 ...} \cr
\code{  ..$ HOUWGT   : num [1:1010] 1.04 1.04 1.04 1.04 1.04 ...} \cr
\code{  ..$ SENWGT   : num [1:1010] 0.111 0.111 0.111 0.111 0.111 ...} \cr
\code{  ..$ SCHWGT   : num [1:1010] 11.6 11.6 11.6 11.6 11.6 ...} \cr
\code{  ..$ STOTWGTU : num [1:1010] 524 524 524 524 524 ...} \cr
\code{  ..$ WGTADJ1  : int [1:1010] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ WGTFAC1  : num [1:1010] 11.6 11.6 11.6 11.6 11.6 ...} \cr
\code{  ..$ JKCREP   : int [1:1010] 1 1 1 1 1 0 0 0 0 0 ...} \cr
\code{  ..$ JKCZONE  : int [1:1010] 1 1 1 1 1 1 1 1 2 2 ...} \cr
\code{  ..$ female   : int [1:1010] 1 1 1 1 0 1 1 1 1 1 ...} \cr
\code{  ..$ M031346A : int [1:1010] 1 NA NA 1 NA 1 NA NA 1 NA ...} \cr
\code{  ..$ M031346B : int [1:1010] 0 NA NA 1 NA 0 NA NA 0 NA ...} \cr
\code{  ..$ M031346C : int [1:1010] 1 NA NA 0 NA 0 NA NA 0 NA ...} \cr
\code{  ..$ M031379  : int [1:1010] 0 NA NA 0 NA 0 NA NA 1 NA ...} \cr
\code{  ..$ M031380  : int [1:1010] 0 NA NA 0 NA 0 NA NA 0 NA ...} \cr
\code{  ..$ M031313  : int [1:1010] 1 NA NA 0 NA 1 NA NA 0 NA ...} \cr
\code{  ..$ M031083  : int [1:1010] 1 NA NA 1 NA 1 NA NA 1 NA ...} \cr
\code{  ..$ M031071  : int [1:1010] 0 NA NA 0 NA 1 NA NA 0 NA ...} \cr
\code{  ..$ M031185  : int [1:1010] 0 NA NA 1 NA 0 NA NA 0 NA ...} \cr
\code{  ..$ M051305  : int [1:1010] 1 1 NA 1 0 0 0 NA 0 1 ...} \cr
\code{  ..$ M051091  : int [1:1010] 1 1 NA 1 1 1 1 NA 1 0 ...} \cr
\code{  .. [list output truncated]} \cr
\code{ $ q.matrix1:'data.frame':      47 obs. of  10 variables:} \cr
\code{  ..$ item : Factor w/ 174 levels "M031004","M031009",..: 29 30 31 32 33 25 8 5 17 163 ...} \cr
\code{  ..$ Co_DA: int [1:47] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_DK: int [1:47] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_DR: int [1:47] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_GA: int [1:47] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_GK: int [1:47] 0 0 0 0 0 0 1 1 0 0 ...} \cr
\code{  ..$ Co_GR: int [1:47] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_NA: int [1:47] 1 0 0 0 0 1 0 0 0 1 ...} \cr
\code{  ..$ Co_NK: int [1:47] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ Co_NR: int [1:47] 0 1 1 1 1 0 0 0 1 0 ...} \cr
\code{ $ q.matrix2:'data.frame':      47 obs. of  4 variables:} \cr
\code{  ..$ item  : Factor w/ 174 levels "M031004","M031009",..: 29 30 31 32 33 25 8 5 17 163 ...} \cr
\code{  ..$ CONT_D: int [1:47] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ CONT_G: int [1:47] 0 0 0 0 0 0 1 1 0 0 ...} \cr
\code{  ..$ CONT_N: int [1:47] 1 1 1 1 1 1 0 0 1 1 ...} \cr
\code{ $ q.matrix3:'data.frame':      47 obs. of  4 variables:} \cr
\code{  ..$ item  : Factor w/ 174 levels "M031004","M031009",..: 29 30 31 32 33 25 8 5 17 163 ...} \cr
\code{  ..$ COGN_A: int [1:47] 1 0 0 0 0 1 0 0 0 1 ...} \cr
\code{  ..$ COGN_K: int [1:47] 0 0 0 0 0 0 1 1 0 0 ...} \cr
\code{  ..$ COGN_R: int [1:47] 0 1 1 1 1 0 0 0 1 0 ...} \cr

\item The dataset \code{data.timss11.G4.sa} contains the Q-matrix
used in Sedat and Arican (2015).

\code{List of 2} \cr
\code{ $ q.matrix:'data.frame':       31 obs. of  13 variables:} \cr
\code{  ..$ N1 : num [1:31] 1 0 0 1 1 0 0 0 0 0 ...} \cr
\code{  ..$ N2 : num [1:31] 1 1 0 0 1 0 0 0 0 0 ...} \cr
\code{  ..$ N3 : num [1:31] 0 0 0 0 1 0 0 0 0 0 ...} \cr
\code{  ..$ A4 : num [1:31] 0 0 1 0 0 1 1 1 0 0 ...} \cr
\code{  ..$ A5 : num [1:31] 0 0 0 0 0 1 0 1 0 0 ...} \cr
\code{  ..$ A6 : num [1:31] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ A7 : num [1:31] 0 0 1 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ G8 : num [1:31] 0 0 0 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ G9 : num [1:31] 0 0 0 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ G10: num [1:31] 0 0 0 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ G11: num [1:31] 0 0 0 0 0 1 0 0 0 0 ...} \cr
\code{  ..$ D12: num [1:31] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ D13: num [1:31] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ skills  : Named chr [1:13] "Possesses understanding of"  __truncated__ ...} \cr
\code{  ..- attr(*, "names")=chr [1:13] "N1" "N2" "N3" "A4" ...} \cr


}

}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}


%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

\references{
George, A. C., & Robitzsch, A. (2014). Multiple group cognitive diagnosis models,
with an emphasis on differential item functioning.
\emph{Psychological Test and Assessment Modeling, 56}(4), 405-432.

George, A. C., & Robitzsch, A. (2015) Cognitive diagnosis models in R: A didactic.
\emph{The Quantitative Methods for Psychology, 11}, 189-205.

George, A. C., & Robitzsch, A. (2018). Focusing on interactions between content
and cognition: A new perspective on gender differences in mathematical
sub-competencies. \emph{Applied Measurement in Education, 31}(1), 79-97.

Sedat, S. E. N., & Arican, M. (2015). A diagnostic comparison of Turkish and
Korean students' Mathematics performances on the TIMSS 2011 assessment.
\emph{Journal of Measurement and Evaluation in Education and Psychology, 6}(2),
238-253.
}

%\examples{
%data(data.timss11.G4.AUT)
%## maybe str(data.timss11.G4.AUT) ; plot(data.timss11.G4.AUT) ...
%}


\keyword{datasets}
