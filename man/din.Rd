%% File Name: din.Rd
%% File Version: 2.35

\name{din}

\alias{din}
\alias{print.din}

\concept{diagnosis models}
\concept{binary response data}

\title{
  Parameter Estimation for Mixed DINA/DINO Model
}

\description{
\code{din} provides parameter estimation for cognitive
diagnosis models of the types ``DINA'', ``DINO'' and ``mixed DINA 
and DINO''.
}

\usage{
din(data, q.matrix, skillclasses = NULL , 
      conv.crit = 0.001, dev.crit= 10^(-5) , maxit = 500, 
      constraint.guess = NULL, constraint.slip = NULL,
      guess.init = rep(0.2, ncol(data)), slip.init = guess.init,
      guess.equal = FALSE, slip.equal = FALSE, zeroprob.skillclasses = NULL,  
      weights = rep(1, nrow(data)), rule = "DINA", 
      wgt.overrelax = 0 , wgtest.overrelax = FALSE , param.history=FALSE , 
      seed=0 , progress = TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

\method{print}{din}(x , \dots) 
}


\arguments{
  \item{data}{A required \eqn{N \times J} data matrix 
    containing the binary responses, 0 or 1, of \eqn{N} respondents to \eqn{J}
    test items, where 1 denotes a correct anwer and 0 an incorrect one. The 
    nth row of the matrix represents the binary response pattern of respondant
    n.  \code{NA} values are allowed.  }
    
  \item{q.matrix}{A required binary \eqn{J \times K} containing the 
    attributes not required or required, 0 or 1, to master the items.  The
    jth row of the matrix is a binary indicator vector indicating which 
    attributes are not required (coded by 0) and which attributes are 
    required (coded by 1) to master item \eqn{j}.  }

  \item{skillclasses}{An optional matrix for determining the skill space.
    The argument can be used if a user wants less than \eqn{2^K} skill classes.    
  }
   
    
  \item{conv.crit}{A numeric which defines the termination criterion 
    of iterations in the parameter 
    estimation process.  Iteration ends if the maximal change in parameter
    estimates is below this value.  }

  \item{dev.crit}{A numeric value which defines the termination criterion 
  	of iterations in relative change in deviance.}


  \item{maxit}{An integer which defines the maximum number 
  of iterations in the estimation process.  }
    
  \item{constraint.guess}{An optional matrix of fixed guessing
    parameters.  The first column of this matrix indicates the numbers of the
    items whose guessing parameters are fixed and the second 
    column the values the guessing parameters are fixed to.  }
    
  \item{constraint.slip}{An optional matrix of fixed slipping
    parameters.  The first column of this matrix indicates the numbers of the 
    items whose slipping parameters are fixed and the second column 
    the values the slipping parameters are fixed to.  }
    
  \item{guess.init}{An optional initial vector of guessing parameters.  
    Guessing parameters are bounded between 0 and 1.  }
    
  \item{slip.init}{An optional initial vector of slipping parameters.  
    Slipping parameters are bounded between 0 and 1.  }
    
  \item{guess.equal}{An optional logical indicating if all guessing parameters 
  are equal to each other. Default is \code{FALSE}.}

 \item{slip.equal}{An optional logical indicating if all slipping parameters 
 are equal to each other. Default is \code{FALSE}.}
  
\item{zeroprob.skillclasses}{An optional vector of integers which indicates 
which skill classes should have zero probability. Default is \code{NULL} 
    (no skill classes with zero probability).}
 
     \item{weights}{An optional vector of weights for the response pattern. 
  	Non-integer weights allow for different sampling schemes.  }
    
  \item{rule}{An optional character string or vector of character strings 
  	specifying the model rule that is used. The character strings must be 
  	of \code{"DINA"} or \code{"DINO"}.  If a vector of character strings is 
  	specified, implying an item wise condensation rule, the vector must 
  	be of length \eqn{J}, which is the number of items.  The default is 
    the condensation rule \code{"DINA"} for all items.  }
         

\item{wgt.overrelax}{A parameter which is relevant when an overrelaxation
    algorithm is used}

\item{wgtest.overrelax}{A logical which indicates if the overrelexation 
    parameter being estimated during iterations}

\item{param.history}{A logical which indicates if the parameter history during
iterations should be saved. The default is \code{FALSE}.}

\item{seed}{Simulation seed for initial parameters. A value of zero corresponds
to deterministic starting values, an integer value different from
zero to random initial values with \code{set.seed(seed)}.}
\item{progress}{An optional logical indicating whether the function
    should print the progress of iteration in the estimation process.  }
\item{guess.min}{Minimum value of guessing parameters to be estimated.}    
\item{slip.min}{Minimum value of slipping parameters to be estimated.}
\item{guess.max}{Maximum value of guessing parameters to be estimated.}    
\item{slip.max}{Maximum value of slipping parameters to be estimated.}   
   \item{x}{Object of class \code{din}}
   \item{\dots}{Further arguments to be passed}
}

\details{
  In the CDM DINA (deterministic-input, noisy-and-gate; de la Torre &
  Douglas, 2004) and DINO (deterministic-input, noisy-or-gate; Templin &
  Henson, 2006) models endorsement probabilities are modeled 
  based on guessing and slipping parameters, given the different skill
  classes.  The probability of respondent \eqn{n} (or corresponding respondents class \eqn{n}) 
  for solving item \eqn{j} is calculated as a function of the 
  respondent's latent response \eqn{\eta_{nj}}{eta_{nj}}
  and the guessing and slipping rates \eqn{g_j} and \eqn{s_j} for item
  \eqn{j} conditional on the respondent's skill class \eqn{\alpha_n}{alpha_n}:
   \deqn{ P(X_{nj} = 1 | \alpha_n) = g_j^{(1- \eta_{nj})}(1 - s_j) ^{\eta_{nj}}.  }
  The respondent's latent response (class) \eqn{\eta_{nj}}{eta_{nj}} is a binary number,
  0 or 1, indicating absence or presence of all (\code{rule = "DINO"}) 
  or at least one (\code{rule = "DINO"}) required skill(s) for 
  item \eqn{j}, respectively.  
  
  DINA and DINO parameter estimation is performed by
  maximization of the marginal likelihood of the data.  The
  a priori distribution of the skill vectors is a uniform distribution.  
  The implementation follows the EM algorithm by de la Torre (2009).  
  
  The function \code{din} returns an object of the class
  \code{din} (see \sQuote{Value}), for which \code{plot},
  \code{print}, and \code{summary} methods are provided;
  \code{\link{plot.din}}, \code{\link{print.din}}, and
  \code{\link{summary.din}}, respectively.  
}

\value{
  \item{coef}{Estimated model parameters. Note that only freely
    estimated parameters are included.}		
  \item{item}{A data frame giving for each item condensation rule, the 
    estimated guessing and slipping parameters and their standard errors.  
    All entries are rounded to 3 digits.  }
  \item{guess}{A data frame giving the estimated guessing parameters
    and their standard errors for each item.  }
  \item{slip}{A data frame giving the estimated slipping parameters
    and their standard errors for each item.  }
  \item{IDI}{A matrix giving the item discrimination
    index (IDI; Lee, de la Torre & Park, 2012) for each item \eqn{j}
    \deqn{ IDI_j = 1 - s_j - g_j , }{ IDI_j = 1 - s_j - g_j , }
    where a high IDI corresponds to good test items
    which have both low guessing and slipping rates.  Note that
    a negative IDI indicates violation of the monotonicity condition 
    \eqn{g_j < 1 - s_j}.  See \code{\link{din}} for help.  }
\item{itemfit.rmsea}{The RMSEA item fit index (see \code{\link{itemfit.rmsea}}).
            }
\item{mean.rmsea}{Mean of RMSEA item fit indexes.
            }            
  \item{loglike}{A numeric giving the value of the maximized
    log likelihood.  }
  \item{AIC}{A numeric giving the AIC value of the model.  }
  \item{BIC}{A numeric giving the BIC value of the model.  }
  \item{Npars}{Number of estimated parameters}  
  \item{posterior}{A matrix given the posterior skill distribution
    for all respondents.  The nth row of the matrix gives the probabilities for 
    respondent n to possess any of the \eqn{2^K} skill classes.  }
  \item{like}{A matrix giving the values of the maximized likelihood
    for all respondents.  }
  \item{data}{The input matrix of binary response data.}
  \item{q.matrix}{The input matrix of the required attributes.  }
  \item{pattern}{A matrix giving the skill classes leading to highest endorsement 
    probability for the respective response pattern (\code{mle.est}) with the 
    corresponding posterior class probability (\code{mle.post}), the attribute 
    classes having the highest occurrence posterior probability given the 
    response pattern (\code{map.est}) with the corresponding posterior class 
    probability (\code{map.post}), and the estimated posterior for each 
    response pattern (\code{pattern}).  }
  \item{attribute.patt}{A data frame giving the estimated occurrence
    probabilities of the skill classes and the expected frequency of the
    attribute classes given the model.  }
  \item{skill.patt}{A matrix given the population prevalences of the
    skills.  }
  \item{subj.pattern}{A vector of strings indicating the item response
    pattern for each subject.  }
  \item{attribute.patt.splitted}{A dataframe giving the skill class 
    of the respondents.  }
  \item{display}{A character giving the model specified under 
    \code{rule}.  }
  \item{item.patt.split}{A matrix giving the splitted response pattern.  }
  \item{item.patt.freq}{A numeric vector given the frequencies of the response
    pattern in \code{item.patt.split}.  }
   \item{seed}{Used simulation seed for initial parameters}
   \item{partable}{Parameter table which is used for \code{coef} and \code{vcov}.}
   \item{vcov.derived}{Design matrix for extended set of parameters in
   \code{vcov}.}
   \item{converged}{Logical indicating whether convergence was achieved.}
  \item{control}{Optimization parameters used in estimation}
}

\references{
  de la Torre, J. (2009). DINA model parameter estimation:
  A didactic. \emph{Journal of Educational and Behavioral
  Statistics}, \bold{34}, 115--130.
  
  de la Torre, J., & Douglas, J. (2004). Higher-order latent trait models 
  for cognitive diagnosis. \emph{Psychometrika}, \bold{69}, 333--353.    

  Lee, Y.-S., de la Torre, J., & Park, Y. S. (2012). 
  Relationships between cognitive diagnosis, CTT, and IRT indices: 
  An empirical investigation. \emph{Asia Pacific Educational Research}, \bold{13}, 333-345.

  Rupp, A. A., Templin, J., & Henson, R. A. (2010). \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}.  New York: The Guilford
  Press.
  
  Templin, J., & Henson, R. (2006). Measurement of
  psychological disorders using cognitive diagnosis
  models. \emph{Psychological Methods}, \bold{11}, 287--305.
}

%\author{
%  Alexander Robitzsch \email{a.robitzsch@bifie.at}
%  Based on EM Algorithm by J. de la Torre.
%  
%  Thomas Kiefer, Ann Cathrice George, Ali Uenlue 
%}

\note{
  The calculation of standard errors using sampling weights which
  represent multistage sampling schemes is not correct.  Please use
  replication methods (like Jackknife) instead.  
}

\seealso{
  \code{\link{plot.din}}, the S3 method for plotting objects of
  the class \code{din}; \code{\link{print.din}}, the S3 method
  for printing objects of the class \code{din};
  \code{\link{summary.din}}, the S3 method for summarizing objects
  of the class \code{din}, which creates objects of the class
  \code{summary.din}; \code{\link{din}}, the main function for
  DINA and DINO parameter estimation, which creates objects of the class
  \code{din}.  
  
  See the \code{\link{gdina}} function for the estimation of
  the generalized DINA (GDINA) model.
  
  For assessment of model fit see \code{\link{modelfit.cor.din}} and 
  \code{\link{anova.din}}.
  
  See \code{\link{itemfit.sx2}} for item fit statistics.  
  
  See also \code{\link{CDM-package}} for general
  information about this package.  
  
  See the \code{NPCD::JMLE} function in the \pkg{NPCD} package for 
  joint maximum likelihood estimation
  of the DINA, DINO and NIDA model.
  
  See the \code{dina::DINA_Gibbs} function in the \pkg{dina}
  package for MCMC based estimation of the DINA model.
}

\examples{
#############################################################################
# EXAMPLE 1: Examples based on dataset fractions.subtraction.data
#############################################################################

## dataset fractions.subtraction.data and corresponding Q-Matrix
head(fraction.subtraction.data)
fraction.subtraction.qmatrix

## Misspecification in parameter specification for method CDM::din()
## leads to warnings and terminates estimation procedure. E.g.,

# See Q-Matrix specification
fractions.dina.warning1 <- CDM::din(data = fraction.subtraction.data,
  q.matrix = t(fraction.subtraction.qmatrix)) 
  
# See guess.init specification
fractions.dina.warning2 <- CDM::din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, guess.init = rep(1.2,
  ncol(fraction.subtraction.data)))
  
# See rule specification   
fractions.dina.warning3 <- CDM::din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, rule = c(rep("DINA",
  10), rep("DINO", 9)))

## Parameter estimation of DINA model
# rule = "DINA" is default
fractions.dina <- CDM::din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, rule = "DINA")
attributes(fractions.dina)
str(fractions.dina)	  

## For instance assessing the guessing parameters through
## assignment
fractions.dina$guess

## corresponding summaries, including IDI,
## most frequent skill classes and information 
## criteria AIC and BIC
summary(fractions.dina)

## In particular, assessing detailed summary through assignment
detailed.summary.fs <- summary(fractions.dina)
str(detailed.summary.fs)

## Item discrimination index of item 8 is too low. This is also
## visualized in the first plot 
plot(fractions.dina)

## The reason therefore is a high guessing parameter
round(fractions.dina$guess[,1], 2)

## Estimate DINA model with different random initial parameters using seed=1345
fractions.dina1 <- CDM::din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, rule = "DINA" , seed=1345)
  
## Fix the guessing parameters of items 5, 8 and 9 equal to .20
# define a constraint.guess matrix
constraint.guess <-  matrix(c(5,8,9, rep(0.2, 3)), ncol = 2)
fractions.dina.fixed <- CDM::din(data = fraction.subtraction.data, 
  q.matrix = fraction.subtraction.qmatrix, 
  constraint.guess=constraint.guess)

## The second plot shows the expected (MAP) and observed skill 
## probabilities. The third plot visualizes the skill class
## occurrence probabilities; Only the 'top.n.skill.classes' most frequent 
## skill classes are labeled; it is obvious that the skill class '11111111' 
## (all skills are mastered) is the most probable in this population. 
## The fourth plot shows the skill probabilities conditional on response
## patterns; in this population the skills 3 and 6 seem to be
## mastered easier than the others. The fourth plot shows the
## skill probabilities conditional on a specified response
## pattern; it is shown whether a skill is mastered (above 
## .5+'uncertainty') unclassifiable (within the boundaries) or
## not mastered (below .5-'uncertainty'). In this case, the
## 527th respondent was chosen; if no response pattern is 
## specified, the plot will not be shown (of course)
pattern <- paste(fraction.subtraction.data[527, ], collapse = "")
plot(fractions.dina, pattern = pattern, display.nr = 4)

#uncertainty = 0.1, top.n.skill.classes = 6 are default
plot(fractions.dina.fixed, uncertainty = 0.1, top.n.skill.classes = 6, 
  pattern = pattern)

\dontrun{  
#############################################################################
# EXAMPLE 2: Examples based on dataset sim.dina
#############################################################################

# DINA Model
d1 <- CDM::din(sim.dina, q.matr = sim.qmatrix, rule = "DINA",
  conv.crit = 0.01, maxit = 500, progress = TRUE)
summary(d1)

# DINA model with hierarchical skill classes (Hierarchical DINA model)
# 1st step:  estimate an initial full model to look at the indexing
#    of skill classes
d0 <- CDM::din(sim.dina, q.matr = sim.qmatrix, maxit=1)
d0$attribute.patt.splitted
#      [,1] [,2] [,3]
# [1,]    0    0    0
# [2,]    1    0    0
# [3,]    0    1    0
# [4,]    0    0    1
# [5,]    1    1    0
# [6,]    1    0    1
# [7,]    0    1    1
# [8,]    1    1    1
#
# In this example, following hierarchical skill classes are only allowed:
# 000, 001, 011, 111
# We define therefore a vector of indices for skill classes with
# zero probabilities (see entries in the rows of the matrix 
# d0$attribute.patt.splitted above)
zeroprob.skillclasses <- c(2,3,5,6)     # classes 100, 010, 110, 101
# estimate the hierarchical DINA model
d1a <- CDM::din(sim.dina, q.matr = sim.qmatrix, 
          zeroprob.skillclasses =zeroprob.skillclasses )
summary(d1a)

# Mixed DINA and DINO Model
d1b <- CDM::din(sim.dina, q.matr = sim.qmatrix, rule = 
          c(rep("DINA", 7), rep("DINO", 2)), conv.crit = 0.01,
          maxit = 500, progress = FALSE)
summary(d1b)

# DINO Model
d2 <- CDM::din(sim.dina, q.matr = sim.qmatrix, rule = "DINO",
  conv.crit = 0.01, maxit = 500, progress = FALSE)
summary(d2)

# Comparison of DINA and DINO estimates
lapply(list("guessing" = rbind("DINA" = d1$guess[,1],
  "DINO" = d2$guess[,1]), "slipping" = rbind("DINA" = 
  d1$slip[,1], "DINO" = d2$slip[,1])), round, 2)

# Comparison of the information criteria
c("DINA"=d1$AIC, "MIXED"=d1b$AIC, "DINO"=d2$AIC)

# following estimates:
d1$coef            # guessing and slipping parameter
d1$guess           # guessing parameter
d1$slip            # slipping parameter
d1$skill.patt      # probabilities for skills
d1$attribute.patt  # skill classes with probabilities
d1$subj.pattern    # pattern per subject

# posterior probabilities for every response pattern
d1$posterior       

# Equal guessing parameters
d2a <- CDM::din( data = sim.dina , q.matrix = sim.qmatrix ,  
            guess.equal = TRUE , slip.equal = FALSE )
d2a$coef

# Equal guessing and slipping parameters
d2b <- CDM::din( data = sim.dina , q.matrix = sim.qmatrix ,  
            guess.equal = TRUE , slip.equal = TRUE )
d2b$coef

#############################################################################
# EXAMPLE 3: Examples based on dataset sim.dino
#############################################################################

# DINO Estimation
d3 <- CDM::din(sim.dino, q.matr = sim.qmatrix, rule = "DINO",
        conv.crit = 0.005, progress = FALSE)

# Mixed DINA and DINO Model
d3b <- CDM::din(sim.dino, q.matr = sim.qmatrix, 
          rule = c(rep("DINA", 4), rep("DINO", 5)), conv.crit = 0.001, 
          progress = FALSE)
                        
# DINA Estimation
d4 <- CDM::din(sim.dino, q.matr = sim.qmatrix, rule = "DINA",
  conv.crit = 0.005, progress = FALSE)
            
# Comparison of DINA and DINO estimates
lapply(list("guessing" = rbind("DINO" = d3$guess[,1],  "DINA" = d4$guess[,1]), 
       "slipping" = rbind("DINO" =d3$slip[,1], "DINA" = d4$slip[,1])), round, 2)

# Comparison of the information criteria
c("DINO"=d3$AIC, "MIXED"=d3b$AIC, "DINA"=d4$AIC)

#############################################################################
# EXAMPLE 4: Example estimation with weights based on dataset sim.dina
#############################################################################

# Here, a weighted maximum likelihood estimation is used 
# This could be useful for survey data.

# i.e. first 200 persons have weight 2, the other have weight 1
(weights <- c(rep(2, 200), rep(1, 200)))

d5 <- CDM::din(sim.dina, sim.qmatrix, rule = "DINA", conv.crit = 
  0.005, weights = weights, progress = FALSE)
        
# Comparison of the information criteria
c("DINA"=d1$AIC, "WEIGHTS"=d5$AIC)

#############################################################################
# EXAMPLE 5: Example estimation within a balanced incomplete 
##           block (BIB) design generated on dataset sim.dina
#############################################################################

# generate BIB data

# The next example shows that the din function works for 
# (relatively arbitrary) missing value pattern

# Here, a missing by design is generated in the dataset dinadat.bib
sim.dina.bib <- sim.dina
sim.dina.bib[1:100, 1:3] <- NA
sim.dina.bib[101:300, 4:8] <- NA
sim.dina.bib[301:400, c(1,2,9)] <- NA

d6 <- CDM::din(sim.dina.bib, sim.qmatrix, rule = "DINA", 
         conv.crit = 0.0005, weights = weights, maxit=200)

d7 <- CDM::din(sim.dina.bib, sim.qmatrix, rule = "DINO",
         conv.crit = 0.005, weights = weights)

# Comparison of DINA and DINO estimates
lapply(list("guessing" = rbind("DINA" = d6$guess[,1],
  "DINO" = d7$guess[,1]), "slipping" = rbind("DINA" =
  d6$slip[,1], "DINO" = d7$slip[,1])), round, 2) 

#############################################################################
# EXAMPLE 6: DINA model with attribute hierarchy
#############################################################################

set.seed(987)
# assumed skill distribution: P(000)=P(100)=P(110)=P(111)=.245 and 
#     "deviant pattern": P(010)=.02
K <- 3 # number of skills

# define alpha
alpha <- scan()
    0 0 0
    1 0 0
    1 1 0
    1 1 1
    0 1 0
    
alpha <- matrix( alpha , length(alpha)/K , K  , byrow=TRUE )
alpha <- alpha[ c( rep(1:4,each=245) , rep(5,20) ),  ]

# define Q-matrix
q.matrix <- scan()
    1 0 0   1 0 0   1 0 0
    0 1 0   0 1 0   0 1 0
    0 0 1   0 1 0   0 0 1
    1 1 0   1 0 1   0 1 1

q.matrix <- matrix( q.matrix , nrow=length(q.matrix)/K , ncol=K , byrow=TRUE )

# simulate DINA data
dat <- sim.din( alpha=alpha , q.matrix=q.matrix )$dat

#*** Model 1: estimate DINA model | no skill space restriction
mod1 <- CDM::din( dat , q.matrix )

#*** Model 2: DINA model | hierarchy A2 > A3
B <- "A2 > A3"
skill.names <- paste0("A",1:3)
skillspace <- CDM::skillspace.hierarchy( B , skill.names )$skillspace.reduced
mod2 <- CDM::din( dat , q.matrix , skillclasses = skillspace )

#*** Model 3: DINA model | linear hierarchy A1 > A2 > A3
#   This is a misspecied model because due to P(010)=.02 the relation A1>A2
#   does not hold.
B <- "A1 > A2
      A2 > A3"
skill.names <- paste0("A",1:3)
skillspace <- CDM::skillspace.hierarchy( B , skill.names )$skillspace.reduced
mod3 <- CDM::din( dat , q.matrix , skillclasses = skillspace )

#*** Model 4: 2PL model in gdm
mod4 <- CDM::gdm( dat , theta.k=seq(-5,5,len=21) , 
           decrease.increments= TRUE , skillspace="normal" )
summary(mod4)

anova(mod1,mod2)
  ##       Model   loglike Deviance Npars      AIC      BIC  Chisq df       p
  ##   2 Model 2 -7052.460 14104.92    29 14162.92 14305.24 0.9174  2 0.63211
  ##   1 Model 1 -7052.001 14104.00    31 14166.00 14318.14     NA NA      NA

anova(mod2,mod3)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df       p
  ##   2 Model 2 -7059.058 14118.12    27 14172.12 14304.63 13.19618  2 0.00136
  ##   1 Model 1 -7052.460 14104.92    29 14162.92 14305.24       NA NA      NA

anova(mod2,mod4)
  ##       Model  loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   2 Model 2 -7220.05 14440.10    24 14488.10 14605.89 335.1805  5  0
  ##   1 Model 1 -7052.46 14104.92    29 14162.92 14305.24       NA NA NA

# compare fit statistics
summary( CDM::modelfit.cor.din( mod2 ) )
summary( CDM::modelfit.cor.din( mod4 ) )

#############################################################################
# EXAMPLE 7: Fitting the basic local independence model (BLIM) with din
#############################################################################

library(pks)
data(DoignonFalmagne7 , package="pks")
  ##  str(DoignonFalmagne7)
  ##    $ K  : int [1:9, 1:5] 0 1 0 1 1 1 1 1 1 0 ...
  ##     ..- attr(*, "dimnames")=List of 2
  ##     .. ..$ : chr [1:9] "00000" "10000" "01000" "11000" ...
  ##     .. ..$ : chr [1:5] "a" "b" "c" "d" ...
  ##    $ N.R: Named int [1:32] 80 92 89 3 2 1 89 16 18 10 ...
  ##     ..- attr(*, "names")= chr [1:32] "00000" "10000" "01000" "00100" ...

# The idea is to fit the local independence model with the din function.
# This can be accomplished by specifying a DINO model with
# prespecified skill classes.

# extract dataset
dat <- as.numeric( unlist( sapply( names(DoignonFalmagne7$N.R)  , 
    FUN = function( ll){ strsplit( ll , split="") } ) ) )
dat <- matrix( dat , ncol=5 , byrow=TRUE )
colnames(dat) <- colnames(DoignonFalmagne7$K)
rownames(dat) <- names(DoignonFalmagne7$N.R)

# sample weights
weights <- DoignonFalmagne7$N.R

# define Q-matrix
q.matrix <- t(DoignonFalmagne7$K)
v1 <- colnames(q.matrix) <- paste0("S" , colnames(q.matrix))
q.matrix <- q.matrix[ , - 1] # remove S00000

# define skill classes
SC <- ncol(q.matrix)
skillclasses <- matrix( 0 , nrow=SC+1 , ncol=SC)
colnames(skillclasses) <- colnames(q.matrix)
rownames(skillclasses) <- v1
skillclasses[ cbind( 2:(SC+1) , 1:SC ) ] <- 1

# estimate BLIM with din function
mod1 <- CDM::din(data= dat, q.matrix=q.matrix, skillclasses = skillclasses , 
            rule="DINO" , weights=weights   )
summary(mod1)
  ##   Item parameters
  ##     item guess  slip   IDI rmsea
  ##   a    a 0.158 0.162 0.680 0.011
  ##   b    b 0.145 0.159 0.696 0.009
  ##   c    c 0.008 0.181 0.811 0.001
  ##   d    d 0.012 0.129 0.859 0.001
  ##   e    e 0.025 0.146 0.828 0.007

# estimate basic local independence model with pks package
mod2 <- pks::blim(K, N.R, method="ML") # maximum likelihood estimation by EM algorithm
mod2
  ##   Error and guessing parameters
  ##         beta      eta
  ##   a 0.164871 0.103065
  ##   b 0.163113 0.095074
  ##   c 0.188839 0.000004
  ##   d 0.079835 0.000003
  ##   e 0.088648 0.019910
}
}

\keyword{Cognitive Diagnosis Models}
\keyword{DINA}
\keyword{DINO}
\keyword{print}
