%% File Name: CDM-package.Rd
%% File Version: 2.69

\name{CDM-package}

\alias{CDM-package}
\concept{cognitive diagnosis modeling}
\docType{package}

\title{
  Cognitive Diagnosis Modeling: The \R Package \pkg{CDM}
}

\description{
    Functions for cognitive diagnosis modeling
    and multidimensional item response modeling for
    dichotomous and polytomous data. This package
    enables the estimation of the DINA and DINO model,
    the multiple group (polytomous) GDINA model,
    the multiple choice DINA model, the general diagnostic
    model (GDM), the multidimensional linear compensatory
    item response model and the structured latent class
    model (SLCA). See George, Robitzsch, Kiefer, Gross, and
    Uenlue (2017) <DOI:10.18637/jss.v074.i02> for further
    details.
}

\details{

  Cognitive diagnosis models (CDMs) are restricted latent class models.  
  They represent model-based classification approaches, which aim at
  assigning respondents to different attribute profile groups.  The latent
  classes correspond to the possible attribute profiles, and the
  conditional item parameters model atypical response behavior in the sense
  of slipping and guessing errors.  The core CDMs in particular differ in
  the utilized condensation rule, conjunctive / non-compensatory versus
  disjunctive / compensatory, where in the model structure these two
  types of response error parameters enter and what restrictions are
  imposed on them.  The confirmatory character of CDMs is apparent in the
  Q-matrix, which can be seen as an operationalization of the latent
  concepts of an underlying theory.  The Q-matrix allows incorporating
  qualitative prior knowledge and typically has as its rows the items and
  as the columns the attributes, with entries 1 or 0, depending on whether
  an attribute is measured by an item or not, respectively.  

  CDMs as compared to common psychometric models (e.g., IRT) contain
  categorical instead of continuous latent variables.  The results of
  analyses using CDMs differ from the results obtained under continuous
  latent variable models.  CDMs estimate in a direct manner the
  probabilistic attribute profile of a respondent, that is, the
  multivariate vector of the conditional probabilities for possessing the
  individual attributes, given her / his response pattern.  Based on these
  probabilities, simplified deterministic attribute profiles can be
  derived, showing whether an individual attribute is essentially possessed
  or not by a respondent.  As compared to alternative two-step
  discretization approaches, which estimate continuous scores and discretize
  the continua based on cut scores, with CDMs the classification error can
  generally be reduced.  

  The package \code{CDM} implements parameter estimation procedures for the 
  DINA and DINO model (e.g.,de la Torre &
  Douglas, 2004; Junker & Sijtsma, 2001; Templin &
  Henson, 2006; the generalized DINA model for dichotomous attributes 
  (GDINA, de la Torre, 2011) and for polytomous attributes
  (pGDINA, Chen & de la Torre, 2013); 
  the general diagnostic model (GDM, von Davier, 2008) and its extension
  to the multidimensional latent class IRT model (Bartolucci, 2007),
  the structure latent class model (Formann, 1992),
  and tools for analyzing data under the models. 
  These and related concepts are explained in detail in the
  book about diagnostic measurement and CDMs by
  Rupp, Templin and Henson (2010), and in such survey articles as
  DiBello, Roussos and Stout (2007) and
  Rupp and Templin (2008).  

  The package \code{CDM} is implemented based on the S3 system.  It comes
  with a namespace and consists of several external functions (functions the
  package exports). 
  The package contains a utility method for the simulation of artificial data based
  on a CDM model (\code{\link{sim.din}}).  It also contains seven internal functions 
  (functions not exported by the package): this are \code{plot}, \code{print}, and 
  \code{summary} methods for objects of the class \code{din} (\code{\link{plot.din}},
  \code{\link{print.din}}, \code{\link{summary.din}}), a \code{print} method for 
  objects of the class \code{summary.din} (\code{\link{print.summary.din}}), 
  and three functions for checking the input format and computing intermediate 
  information.  The features of the package \code{CDM} are 
  illustrated with an accompanying real dataset and Q-matrix 
  (\code{\link{fraction.subtraction.data}} and \code{\link{fraction.subtraction.qmatrix}}) 
  and artificial examples (\code{\link{Data-sim}}).  
  
  See George et al. (2016) for an overview and some computational details
  of the \pkg{CDM} package.
}

\author{
  Alexander Robitzsch, Thomas Kiefer, Ann Cathrice George, Ali Uenlue 

  Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de>
}

\seealso{
    See the \pkg{GDINA} package for comprehensive functions for the 
    GDINA model.  

    See also the \pkg{ACTCD} and \pkg{NPCD} packages for nonparametric cognitive 
    diagnostic models.     
    
    See \pkg{dcmr} for MCMC based estimation methods. See \pkg{dina}
    for estimating the DINA model with a Gibbs sampler.
        }

%    In \pkg{NPCD} joint maximum likelihood estimation 
%    of several cognitive diagnostic models are implemented.        
        
\references{
  Bartolucci, F. (2007). A class of multidimensional IRT models for testing 
  unidimensionality and clustering items. \emph{Psychometrika}, \bold{72}, 141-157.

  Chen, J., & de la Torre, J. (2013). 
  A general cognitive diagnosis model for expert-defined polytomous attributes.
  \emph{Applied Psychological Measurement}, \bold{37}, 419-437.  
  
  de la Torre, J., & Douglas, J. (2004). Higher-order latent trait models 
  for cognitive diagnosis. \emph{Psychometrika}, \bold{69}, 333--353.

  de la Torre, J. (2011). The generalized {DINA} model framework. 
  \emph{Psychometrika}, \bold{76}, 179--199.    

  DiBello, L. V., Roussos, L. A., & Stout, W. F. (2007). Review of
  cognitively diagnostic assessment and a summary of psychometric models.
  In C. R. Rao and S. Sinharay (Eds.), \emph{Handbook of Statistics},
  Vol. 26 (pp. 979--1030). Amsterdam: Elsevier.

Formann, A. K. (1992). Linear logistic latent class analysis for polytomous data. 
\emph{Journal of the American Statistical Association}, \bold{87}, 476-486.  
  
George, A. C., Robitzsch, A., Kiefer, T., Gross, J., & Uenlue, A. (2016).
The R package CDM for cognitive diagnosis models.
\emph{Journal of Statistical Software, 74}(2), 1-24.
  
Junker, B. W., & Sijtsma, K. (2001). Cognitive assessment models with few  
  assumptions, and connections with nonparametric item response theory.
  \emph{Applied Psychological Measurement}, \bold{25}, 258--272.
    
  Rupp, A. A., & Templin, J. (2008). Unique characteristics of
  diagnostic classification models: A comprehensive review of the current
  state-of-the-art. \emph{Measurement: Interdisciplinary Research and
  Perspectives}, \bold{6}, 219--262.
  
  Rupp, A. A., Templin, J., & Henson, R. A. (2010). \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}. New York: The Guilford
  Press.
  
  Templin, J., & Henson, R. (2006). Measurement of
  psychological disorders using cognitive diagnosis
  models. \emph{Psychological Methods}, \bold{11}, 287--305.

  von Davier, M. (2008). A general diagnostic model applied to 
  language testing data. \emph{British Journal
  of Mathematical and Statistical Psychology}, \bold{61}, 287-307.
}

\examples{
##   
##   **********************************
##   ** CDM 2.5-16 (2013-11-29)      **
##   ** Cognitive Diagnostic Models  **
##   **********************************
##   
}

\keyword{package}
