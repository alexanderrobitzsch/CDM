%% File Name: data.ecpe.Rd
%% File Version: 0.19

\name{data.ecpe}
\alias{data.ecpe}
\docType{data}
\title{
Dataset ECPE
}
\description{
ECPE dataset from the Templin and Hoffman (2013) tutorial of
specifying cognitive diagnostic models in Mplus.
}

\usage{
data(data.ecpe)
}

\format{
The format of the data is a list containing the dichotomous item
response data \code{data} (2922 persons at 28 items) 
and the Q-matrix \code{q.matrix} (28 items and 3 skills):

\code{List of 2} \cr
\code{ $ data    :'data.frame':} \cr
\code{  ..$ id : int [1:2922] 1 2 3 4 5 6 7 8 9 10 ...} \cr
\code{  ..$ E1 : int [1:2922] 1 1 1 1 1 1 1 0 1 1 ...} \cr
\code{  ..$ E2 : int [1:2922] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ E3 : int [1:2922] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ E4 : int [1:2922] 0 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  [...] } \cr
\code{  ..$ E27: int [1:2922] 1 1 1 1 1 1 1 0 1 1 ...} \cr
\code{  ..$ E28: int [1:2922] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ q.matrix:'data.frame':} \cr
\code{  ..$ skill1: int [1:28] 1 0 1 0 0 0 1 0 0 1 ...} \cr
\code{  ..$ skill2: int [1:28] 1 1 0 0 0 0 0 1 0 0 ...} \cr
\code{  ..$ skill3: int [1:28] 0 0 1 1 1 1 1 0 1 0 ...} \cr

The skills are

\code{skill1}: Morphosyntactic rules

\code{skill2}: Cohesive rules

\code{skill3}: Lexical rules.

}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
The dataset is used in Templin and Hoffman (2013) and
Templin and Bradshaw (2014).

The dataset was downloaded from
\url{http://psych.unl.edu/jtemplin/teaching/dcm/dcm12ncme/}.
}


\references{
Templin, J., & Bradshaw, L. (2014). Hierarchical diagnostic classification
models: A family of models for estimating and testing attribute
hierarchies. \emph{Psychometrika}, \bold{79}, 317-339.
	
Templin, J., & Hoffman, L. (2013). 
Obtaining diagnostic classification model estimates using Mplus. 
\emph{Educational Measurement: Issues and Practice}, \bold{32}, 37-50.
}

\seealso{
\code{\link[GDINA:ecpe]{GDINA::ecpe}}
}

\examples{
\dontrun{
data(data.ecpe, package="CDM")
	
dat <- data.ecpe$data[,-1]
Q <- data.ecpe$q.matrix
	
#*** Model 1: LCDM model 
mod1 <- CDM::gdina( dat, q.matrix=Q, link="logit")
summary(mod1)

#*** Model 2: DINA model 
mod2 <- CDM::gdina( dat, q.matrix=Q, rule="DINA")
summary(mod2)

# Model comparison using likelihood ratio test
anova(mod1,mod2)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   2 Model 2 -42841.61 85683.23    63 85809.23 86185.97 206.0359 18  0
  ##   1 Model 1 -42738.60 85477.19    81 85639.19 86123.57       NA NA NA

#*** Model 3: Hierarchical LCDM (HLCDM) | Templin and Bradshaw (2014)
#      Testing a linear hierarchy
hier <- "skill3 > skill2 > skill1"
skill.names <- colnames(Q)
# define skill space with hierarchy
skillspace <- CDM::skillspace.hierarchy( hier, skill.names= skill.names )
skillspace$skillspace.reduced
  ##        skill1 skill2 skill3
  ##   A000      0      0      0
  ##   A001      0      0      1
  ##   A011      0      1      1
  ##   A111      1      1      1
zeroprob.skillclasses <- skillspace$zeroprob.skillclasses

# define user-defined parameters in LCDM: hierarchical LCDM (HLCDM)
Mj.user <- mod1$Mj
# select items with require two attributes
items <- which( rowSums(Q) > 1 )
# modify design matrix for item parameters
for (ii in items){
    m1 <- Mj.user[[ii]]
    Mj.user[[ii]][[1]] <- (m1[[1]])[,-2]
    Mj.user[[ii]][[2]] <- (m1[[2]])[-2]
}

# estimate model
#    note that avoid.zeroprobs is set to TRUE to avoid algorithmic instabilities
mod3 <- CDM::gdina( dat, q.matrix=Q, link="logit", 
            zeroprob.skillclasses=zeroprob.skillclasses, Mj=Mj.user, 
            avoid.zeroprobs=TRUE )
summary(mod3)

#*****************************************
#** estimate further models

#*** Model 4: RRUM model
mod4 <- CDM::gdina( dat, q.matrix=Q, rule="RRUM")
summary(mod4)
# compare some models
IRT.compareModels(mod1 , mod2, mod3, mod4 )

#*** Model 5a: GDINA model with identity link
mod5a <- CDM::gdina( dat, q.matrix=Q, link="identity")
summary(mod5a)
#*** Model 5b: GDINA model with logit link
mod5b <- CDM::gdina( dat, q.matrix=Q, link="logit")
summary(mod5b)
#*** Model 5c: GDINA model with log link
mod5c <- CDM::gdina( dat, q.matrix=Q, link="log")
summary(mod5c)
# compare models
IRT.compareModels(mod5a, mod5b, mod5c)
}
}
\keyword{datasets}
