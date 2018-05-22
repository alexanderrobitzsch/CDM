%% File Name: data.dcm.Rd
%% File Version: 0.28

\name{data.dcm}
\alias{data.dcm}
\docType{data}
\title{
Dataset from Book 'Diagnostic Measurement' of Rupp, Templin and
Henson (2010)
}


\description{
Dataset from Chapter 9 of the book 'Diagnostic Measurement'
(Rupp, Templin & Henson, 2010).
}


\usage{
data(data.dcm)
}


\format{
The format of the data is a list containing the dichotomous item
response data \code{data} (10000 persons at 7 items)
and the Q-matrix \code{q.matrix} (7 items and 3 skills):

\code{List of 2} \cr
\code{ $ data    :'data.frame':} \cr
\code{  ..$ id: int [1:10000] 1 2 3 4 5 6 7 8 9 10 ...} \cr
\code{  ..$ D1: num [1:10000] 0 0 0 0 1 0 1 0 0 1 ...} \cr
\code{  ..$ D2: num [1:10000] 0 0 0 0 0 1 1 1 0 1 ...} \cr
\code{  ..$ D3: num [1:10000] 1 0 1 0 1 1 0 0 0 1 ...} \cr
\code{  ..$ D4: num [1:10000] 0 0 1 0 0 1 1 1 0 0 ...} \cr
\code{  ..$ D5: num [1:10000] 1 0 0 0 1 1 1 0 1 0 ...} \cr
\code{  ..$ D6: num [1:10000] 0 0 0 0 1 1 1 0 0 1 ...} \cr
\code{  ..$ D7: num [1:10000] 0 0 0 0 0 1 1 0 1 1 ...} \cr
\code{ $ q.matrix: num [1:7, 1:3] 1 0 0 1 1 0 1 0 1 0 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:7] "D1" "D2" "D3" "D4" ...} \cr
\code{  .. ..$ : chr [1:3] "skill1" "skill2" "skill3"} \cr
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
For supplementary material of the Rupp, Templin and Henson book (2010)
see \url{http://dcm.coe.uga.edu/}.

The dataset was downloaded from
\url{http://dcm.coe.uga.edu/supplemental/chapter9.html}.
}

\references{
Rupp, A. A., Templin, J., & Henson, R. A. (2010). \emph{Diagnostic
Measurement: Theory, Methods, and Applications}. New York: The Guilford
Press.
}

\examples{
\dontrun{
data(data.dcm, package="CDM")

dat <- data.dcm$data[,-1]
Q <- data.dcm$q.matrix

#*****************************************************
# Model 1: DINA model
#*****************************************************
mod1 <- CDM::din( dat, q.matrix=Q )
summary(mod1)

#--------
# Model 1m: estimate model in mirt package
library(mirt)
library(sirt)

  #** define theta grid of skills
  # use the function skillspace.hierarchy just for convenience
hier <- "skill1 > skill2"
skillspace <- CDM::skillspace.hierarchy( hier, skill.names=colnames(Q) )
Theta <- as.matrix(skillspace$skillspace.complete)
  #** create mirt model
mirtmodel <- mirt::mirt.model("
      skill1=1
      skill2=2
      skill3=3
      (skill1*skill2)=4
      (skill1*skill3)=5
      (skill2*skill3)=6
      (skill1*skill2*skill3)=7
          " )
  #** mirt parameter table
mod.pars <- mirt::mirt( dat, mirtmodel, pars="values")
  # use starting values of .20 for guessing parameter
ind <- which( mod.pars$name=="d" )
mod.pars[ind,"value"] <- stats::qlogis(.20) # guessing parameter on the logit metric
  # use starting values of .80 for anti-slipping parameter
ind <- which( ( mod.pars$name \%in\% paste0("a",1:20 ) ) & (mod.pars$est) )
mod.pars[ind,"value"] <- stats::qlogis(.80) - stats::qlogis(.20)
mod.pars
  #** prior for the skill space distribution
I <- ncol(dat)
lca_prior <- function(Theta,Etable){
  TP <- nrow(Theta)
  if ( is.null(Etable) ){ prior <- rep( 1/TP, TP ) }
  if ( ! is.null(Etable) ){
    prior <- ( rowSums(Etable[, seq(1,2*I,2)]) + rowSums(Etable[,seq(2,2*I,2)]) )/I
  }
  prior <- prior / sum(prior)
  return(prior)
 }

 #** estimate model in mirt
mod1m <- mirt::mirt(dat, mirtmodel, pars=mod.pars, verbose=TRUE,
            technical=list( customTheta=Theta, customPriorFun=lca_prior) )
  # The number of estimated parameters is incorrect because mirt does not correctly count
  # estimated parameters from the user customized  prior distribution.
mod1m@nest <- as.integer(sum(mod.pars$est) + nrow(Theta) - 1)
  # extract log-likelihood
mod1m@logLik
  # compute AIC and BIC
( AIC <- -2*mod1m@logLik+2*mod1m@nest )
( BIC <- -2*mod1m@logLik+log(mod1m@Data$N)*mod1m@nest )
  #** extract item parameters
cmod1m <- sirt::mirt.wrapper.coef(mod1m)$coef
# compare estimated guessing and slipping parameters
dfr <- data.frame(    "din.guess"=mod1$guess$est,
                  "mirt.guess"=plogis(cmod1m$d), "din.slip"=mod1$slip$est,
                  "mirt.slip"=1-plogis( rowSums( cmod1m[, c("d", paste0("a",1:7) ) ] ) )
                    )
round(t(dfr),3)
  ##               [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]
  ##   din.guess  0.217 0.193 0.189 0.135 0.143 0.135 0.162
  ##   mirt.guess 0.226 0.189 0.184 0.132 0.142 0.132 0.158
  ##   din.slip   0.338 0.331 0.334 0.220 0.222 0.211 0.042
  ##   mirt.slip  0.339 0.333 0.336 0.223 0.225 0.214 0.044

# compare estimated skill class distribution
dfr <- data.frame("din"=mod1$attribute.patt$class.prob,
                    "mirt"=mod1m@Prior[[1]] )
round(t(dfr),3)
  ##         [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]
  ##   din  0.113 0.083 0.094 0.092 0.064 0.059 0.065 0.429
  ##   mirt 0.116 0.074 0.095 0.064 0.095 0.058 0.066 0.433

#** extract estimated classifications
fsc1m <- sirt::mirt.wrapper.fscores( mod1m )
#- estimated reliabilities
fsc1m$EAP.rel
  ##      skill1    skill2    skill3
  ##   0.5479942 0.5362595 0.5357961
#- estimated classfications: EAPs, MLEs and MAPs
head( round(fsc1m$person,3) )
  ##     case     M EAP.skill1 SE.EAP.skill1 EAP.skill2 SE.EAP.skill2 EAP.skill3 SE.EAP.skill3
  ##   1    1 0.286      0.508         0.500      0.067         0.251      0.820         0.384
  ##   2    2 0.000      0.162         0.369      0.191         0.393      0.190         0.392
  ##   3    3 0.286      0.200         0.400      0.211         0.408      0.607         0.489
  ##   4    4 0.000      0.162         0.369      0.191         0.393      0.190         0.392
  ##   5    5 0.571      0.802         0.398      0.267         0.443      0.928         0.258
  ##   6    6 0.857      0.998         0.045      1.000         0.019      1.000         0.020
  ##     MLE.skill1 MLE.skill2 MLE.skill3 MAP.skill1 MAP.skill2 MAP.skill3
  ##   1          1          0          1          1          0          1
  ##   2          0          0          0          0          0          0
  ##   3          0          0          1          0          0          1
  ##   4          0          0          0          0          0          0
  ##   5          1          0          1          1          0          1
  ##   6          1          1          1          1          1          1

#** estimate model fit in mirt
( fit1m <- mirt::M2( mod1m ) )

#*****************************************************
# Model 2: DINO model
#*****************************************************
mod2 <- CDM::din( dat, q.matrix=Q, rule="DINO")
summary(mod2)

#*****************************************************
# Model 3: log-linear model (LCDM): this model is the GDINA model with the
#    logit link function
#*****************************************************
mod3 <- CDM::gdina( dat, q.matrix=Q, link="logit")
summary(mod3)

#*****************************************************
# Model 4: GDINA model with identity link function
#*****************************************************
mod4 <- CDM::gdina( dat, q.matrix=Q )
summary(mod4)

#*****************************************************
# Model 5: GDINA additive model identity link function
#*****************************************************
mod5 <- CDM::gdina( dat, q.matrix=Q, rule="ACDM")
summary(mod5)

#*****************************************************
# Model 6: GDINA additive model logit link function
#*****************************************************
mod6 <- CDM::gdina( dat, q.matrix=Q, link="logit", rule="ACDM")
summary(mod6)

#--------
# Model 6m: GDINA additive model in mirt package
# use data specifications from Model 1m)
  #** create mirt model
mirtmodel <- mirt::mirt.model("
      skill1=1,4,5,7
      skill2=2,4,6,7
      skill3=3,5,6,7
          " )
  #** mirt parameter table
mod.pars <- mirt::mirt( dat, mirtmodel, pars="values")
 #** estimate model in mirt
 # Theta and lca_prior as defined as in Model 1m
mod6m <- mirt::mirt(dat, mirtmodel, pars=mod.pars, verbose=TRUE,
            technical=list( customTheta=Theta, customPriorFun=lca_prior) )
mod6m@nest <- as.integer(sum(mod.pars$est) + nrow(Theta) - 1)
  # extract log-likelihood
mod6m@logLik
  # compute AIC and BIC
( AIC <- -2*mod6m@logLik+2*mod6m@nest )
( BIC <- -2*mod6m@logLik+log(mod6m@Data$N)*mod6m@nest )
  #** skill distribution
  mod6m@Prior[[1]]
  #** extract item parameters
cmod6m <- mirt.wrapper.coef(mod6m)$coef
print(cmod6m,digits=4)
  ##     item    a1    a2    a3       d g u
  ##   1   D1 1.882 0.000 0.000 -0.9330 0 1
  ##   2   D2 0.000 2.049 0.000 -1.0430 0 1
  ##   3   D3 0.000 0.000 2.028 -0.9915 0 1
  ##   4   D4 2.697 2.525 0.000 -2.9925 0 1
  ##   5   D5 2.524 0.000 2.478 -2.7863 0 1
  ##   6   D6 0.000 2.818 2.791 -3.1324 0 1
  ##   7   D7 3.113 2.918 2.785 -4.2794 0 1

#*****************************************************
# Model 7: Reduced RUM model
#*****************************************************
mod7 <- CDM::gdina( dat, q.matrix=Q, rule="RRUM")
summary(mod7)

#*****************************************************
# Model 8: latent class model with 3 classes and 4 sets of starting values
#*****************************************************

#-- Model 8a: randomLCA package
library(randomLCA)
mod8a <- randomLCA::randomLCA( dat, nclass=3, verbose=TRUE, notrials=4)

#-- Model8b: rasch.mirtlc function in sirt package
library(sirt)
mod8b <- sirt::rasch.mirtlc( dat, Nclasses=3, nstarts=4 )
summary(mod8a)
summary(mod8b)
}
}
\keyword{datasets}
