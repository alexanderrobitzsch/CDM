%% File Name: data.dtmr.Rd
%% File Version: 0.25

\name{data.dtmr}
\alias{data.dtmr}
\docType{data}

\title{
DTMR Fraction Data (Bradshaw et al., 2014)
}

\description{
This is a simulated dataset of the DTMR fraction data described
in Bradshaw, Izsak, Templin and Jacobson (2014).
}


\usage{
data(data.dtmr)
}


\format{
  The format is:

\code{List of 5} \cr
\code{ $ data              : num [1:5000, 1:27] 0 0 0 0 0 1 0 0 1 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:27] "M1" "M2" "M3" "M4" ...} \cr
\code{ $ q.matrix          :'data.frame':     27 obs. of  4 variables:} \cr
\code{  ..$ RU : int [1:27] 1 0 0 1 1 0 1 0 0 0 ...} \cr
\code{  ..$ PI : int [1:27] 0 0 1 0 0 1 0 0 0 0 ...} \cr
\code{  ..$ APP: int [1:27] 0 1 0 0 0 0 0 1 1 1 ...} \cr
\code{  ..$ MC : int [1:27] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ skill.distribution:'data.frame':     16 obs. of  5 variables:} \cr
\code{  ..$ RU  : int [1:16] 0 0 0 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ PI  : int [1:16] 0 0 0 0 1 1 1 1 0 0 ...} \cr
\code{  ..$ APP : int [1:16] 0 0 1 1 0 0 1 1 0 0 ...} \cr
\code{  ..$ MC  : int [1:16] 0 1 0 1 0 1 0 1 0 1 ...} \cr
\code{  ..$ freq: int [1:16] 1064 350 280 406 196 126 238 770 14 28 ...} \cr
\code{ $ itempars          :'data.frame':     27 obs. of  7 variables:} \cr
\code{  ..$ item : chr [1:27] "M1" "M2" "M3" "M4" ...} \cr
\code{  ..$ lam0 : num [1:27] -1.12 0.59 -2.07 -1.19 -1.67 -3.81 -0.73 -0.62 -0.09 0.28 ...} \cr
\code{  ..$ RU   : num [1:27] 2.24 0 0 0.65 1.52 0 1.2 0 0 0 ...} \cr
\code{  ..$ PI   : num [1:27] 0 0 1.7 0 0 2.08 0 0 0 0 ...} \cr
\code{  ..$ APP  : num [1:27] 0 1.27 0 0 0 0 0 4.25 2.16 0.87 ...} \cr
\code{  ..$ MC   : num [1:27] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ RU.PI: num [1:27] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ sim_data          :function (N, skill.distribution, itempars)  } \cr
\code{  ..- attr(*, "srcref")='srcref' int [1:8] 1 13 20 1 13 1 1 20} \cr
\code{  .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000298a8ed0> } \cr


The attribute definition are as follows

\code{RU}: {Referent units}

\code{PI}: {Partitioning and iterating attribute}

\code{APP}: {Appropriateness attribute}

\code{MC}: {Multiplicative Comparison attribute}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
Simulated dataset according to Bradshaw et al. (2014).
}

\references{
Bradshaw, L., Izsak, A., Templin, J., & Jacobson, E. (2014).
Diagnosing teachers' understandings of rational numbers: Building a
multidimensional test within the diagnostic classification framework.
\emph{Educational Measurement: Issues and Practice, 33}, 2-14.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Model comparisons data.dtmr
#############################################################################

data(data.dtmr, package="CDM")
data <- data.dtmr$data
q.matrix <- data.dtmr$q.matrix
I <- ncol(data)

#*** Model 1: LCDM
# define item wise rules
rule <- rep( "ACDM", I )
names(rule) <- colnames(data)
rule[ c("M14","M17") ] <- "GDINA2"
# estimate model
mod1 <- CDM::gdina( data, q.matrix, linkfct="logit", rule=rule)
summary(mod1)

#*** Model 2: DINA model
mod2 <- CDM::gdina( data, q.matrix, rule="DINA" )
summary(mod2)

#*** Model 3: RRUM model
mod3 <- CDM::gdina( data, q.matrix, rule="RRUM" )
summary(mod3)

#--- model comparisons

# LCDM vs. DINA
anova(mod1,mod2)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   2 Model 2 -76570.89 153141.8    69 153279.8 153729.5 1726.645 10  0
  ##   1 Model 1 -75707.57 151415.1    79 151573.1 152088.0       NA NA NA

# LCDM vs. RRUM
anova(mod1,mod3)
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   2 Model 2 -75746.13 151492.3    77 151646.3 152148.1 77.10994  2  0
  ##   1 Model 1 -75707.57 151415.1    79 151573.1 152088.0       NA NA NA

#--- model fit
summary( CDM::modelfit.cor.din( mod1 ) )
  ##   Test of Global Model Fit
  ##          type   value       p
  ##   1   max(X2) 7.74382 1.00000
  ##   2 abs(fcor) 0.04056 0.72707
  ##
  ##   Fit Statistics
  ##                       est
  ##   MADcor          0.00959
  ##   SRMSR           0.01217
  ##   MX2             0.75696
  ##   100*MADRESIDCOV 0.20283
  ##   MADQ3           0.02220

#############################################################################
# EXAMPLE 2: Simulating data of structure data.dtmr
#############################################################################

data(data.dtmr, package="CDM")

# draw sample of N=200
set.seed(87)
data.dtmr$sim_data(N=200, skill.distribution=data.dtmr$skill.distribution,
             itempars=data.dtmr$itempars)
}
}
%% \keyword{datasets}
