%% File Name: gdina.Rd
%% File Version: 3.404

\name{gdina}
\alias{gdina}
\alias{summary.gdina}
\alias{plot.gdina}
\alias{print.gdina}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimating the Generalized DINA (GDINA) Model
}
\description{
    This function implements the generalized DINA model for dichotomous
    attributes (GDINA; de la Torre, 2011) and polytomous attributes
    (pGDINA; Chen & de la Torre, 2013). 
    See the papers for details about estimable cognitive diagnosis models. 
    In addition, multiple group estimation 
    is also possible using the \code{gdina} function. This function also
    allows for the estimation of a higher order GDINA model
    (de la Torre & Douglas, 2004).
    Polytomous item responses are treated by specifying a sequential
    GDINA model (Ma & de la Torre, 2016; Tutz, 1997).
    
    The estimation can also be conducted by posing monotonocity 
    constraints using the argument \code{mono.constr}. Moreover,
    regularization methods SCAD, lasso, ridge, SCAD-L2 and
    truncated \eqn{L_1} penalty (TLP) for item parameters 
    can be employed (Xu & Shang, xxxx).
    
    Normally distributed priors can be specified for item parameters 
    (item intercepts and item slopes). Note that (for convenience) the
    prior specification holds simultaneously for all items.
}
\usage{
gdina(data, q.matrix, skillclasses=NULL, conv.crit=0.0001, dev.crit=.1 ,  maxit=1000, 
    linkfct="identity", Mj=NULL, group=NULL , invariance = TRUE ,method=NULL , 
    delta.init = NULL , delta.fixed = NULL , delta.designmatrix=NULL, 
    delta.basispar.lower=NULL, delta.basispar.upper=NULL, delta.basispar.init=NULL,
    zeroprob.skillclasses=NULL, attr.prob.init = NULL , reduced.skillspace=NULL , 
    reduced.skillspace.method=2 , HOGDINA=-1, Z.skillspace=NULL, 
    weights=rep(1, nrow(data)), rule="GDINA", regular_lam = 0, regular_type = "none", 
    regular_alpha=NA, regular_tau=NA, mono.constr = FALSE, 
    prior_intercepts = NULL, prior_slopes = NULL, progress=TRUE, 
    progress.item=FALSE, mstep_iter = 10, mstep_conv = 1E-4, increment.factor=1.01, 
    fac.oldxsi=0, max.increment = .3, avoid.zeroprobs = FALSE, seed=0, 
    save.devmin=TRUE, calc.se=TRUE, se_version=1, PEM = TRUE, PEM_itermax=maxit, 
    cd = FALSE, cd_steps = 1 , mono_maxiter = 10, freq_weights=FALSE, ...)

\method{summary}{gdina}(object, digits = 4 , file = NULL ,  \dots)

\method{plot}{gdina}(x , ask=FALSE ,  \dots)

\method{print}{gdina}(x ,  \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A required \eqn{N \times J} data matrix 
    containing integer responses, 0, 1, ..., K. Polytomous
    item responses are treated by the sequential GDINA model.
  \code{NA} values are allowed.  
  }
    
  \item{q.matrix}{A required integer \eqn{J \times K} matrix containing
    attributes not required or required, 0 or 1, to master the items in case
    of dichotomous attributes or integers in case of polytomous attributes. 
    For polytomous item responses the Q-matrix must also include the
    item name and item category, see Example 11.
  }

  \item{skillclasses}{An optional matrix for determining the skill space.
    The argument can be used if a user wants less than \eqn{2^K} skill classes.    
  }
  
% The jth row of the matrix is a binary indicator vector indicating which 
%    attributes are not required (coded by 0) and which attributes are 
%    required (coded by 1) to master item \eqn{j}.  
  
\item{conv.crit}{
    Convergence criterion for maximum absolut change in item parameters
        }
\item{dev.crit}{
    Convergence criterion for maximum absolut change in deviance
        }        
\item{maxit}{
    Maximum number of iterations
    }
\item{linkfct}{
    A string which indicates the link function for the GDINA model. 
    Options are \code{"identity"} (identity link),
    \code{"logit"} (logit link) and \code{"log"} (log link). The default is the 
    \code{"identity"} link. Note that the link function is chosen
    for the whole model (i.e. for all items).
}
\item{Mj}{
A list of design matrices and labels for each item. 
The definition of \code{Mj} follows the
definition of \eqn{M_j} in de la Torre (2011). 
Please study the value \code{Mj} of the function in 
default analysis. See Example 3.
}
\item{group}{A vector of group identifiers for multiple group 
    estimation. Default is \code{NULL} (no multiple group estimation).
        }
\item{invariance}{Logical indicating whether invariance of item parameters
is assumed for multiple group models. If a subset of items should
be treated as nonivariant, then \code{invariance} can be a vector
of item names.
}     
\item{method}{Estimation method for item parameters (see) 
    (de la Torre, 2011). The default \code{"WLS"}
    weights probabilities attribute classes by a weighting 
    matrix \eqn{W_j} of expected frequencies,
    whereas the method \code{"ULS"} perform unweighted least squares
    estimation on expected frequencies.
    The method \code{"ML"} directly maximizes the log-likelihood
    function.  The \code{"ML"} method is a bit slower but can be
    much more stable, especially in the case of the RRUM model.
    Only for the RRUM model, the default is changed to
    \code{method="ML"} if not specified otherwise.
    }
\item{delta.init}{List with initial \eqn{\delta} parameters}
\item{delta.fixed}{List with fixed \eqn{\delta} parameters.
For free estimated parameters \code{NA} must be declared.}
\item{delta.designmatrix}{
    A design matrix for restrictions on delta. See Example 4.
    }
\item{delta.basispar.lower}{
    Lower bounds for delta basis parameters.
}
  \item{delta.basispar.upper}{
    Upper bounds for delta basis parameters.
}
  \item{delta.basispar.init}{
    An optional vector of starting values for the basis parameters of delta. 
    This argument only applies when using a designmatrix for delta, 
    i.e. \code{delta.designmatrix} is not \code{NULL}.
}

\item{zeroprob.skillclasses}{An optional vector of integers which indicates which skill
  classes should have zero probability. Default is NULL 
    (no skill classes with zero probability).}
\item{attr.prob.init}{Initial probabilities of skill distribution.}    

\item{reduced.skillspace}{A logical which indicates if the latent class skill space should be
        dimensionally reduced (see Xu & von Davier, 2008). The default is \code{NULL}
        which applies skill space reduction for more than four skills.
        The dimensional reduction is only well defined for more than three skills.
        If the argument \code{zeroprob.skillclasses} is not \code{NULL}, then
        \code{reduced.skillspace} is set to \code{FALSE}.}

\item{reduced.skillspace.method}{Computation method for skill space reduction
	in case of \code{reduced.skillspace=TRUE}. The default is \code{2}
	which is computationally more efficient but introduced in \pkg{CDM} 2.6.
	For reasons of compatibility of former \pkg{CDM} versions (\eqn{\le} 2.5),
	\code{reduced.skillspace.method=1} uses the older implemented
	method. In case of non-convergence with the new method, please try
	the older method.
}        
        
\item{HOGDINA}{Values of -1, 0 or 1 indicating if a higher order GDINA
 	model (see Details) should be estimated. 
 	The default value of -1 corresponds to the case that no higher order
 	factor is assumed to exist. A value of 0 corresponds to independent
 	attributes. A value of 1 assumes the existence of a higher order
	factor. }

\item{Z.skillspace}{A user specified design matrix for the skill space reduction
        as described in Xu and von Davier (2008). See in the Examples section for 
        applications. See Example 6.
                }

\item{weights}{
An optional vector of sample weights.
}
\item{rule}{
    A string or a vector of itemwise condensation rules. Allowed entries are 
    \code{GDINA}, \code{DINA}, \code{DINO}, \code{ACDM} (additive 
    cognitive diagnostic model) and \code{RRUM} (reduced reparametrized
    unified model, RRUM, see Details). 
    The rule \code{GDINA1} applies
    only main effects in the GDINA model which is equivalent to \code{ACDM}. 
    The rule \code{GDINA2} applies to all main effects and second-order 
    interactions of the attributes. If some item is specified as \code{RRUM},
    then for all the items the reduced RUM will be estimated which means
    that the log link function and the \code{ACDM} condensation rule is used.
    In the output, the entry \code{rrum.params} contains the parameters
    transformed in the RUM parametrization.
    If \code{rule} is a string, the condensation rule applies to all items. 
    If \code{rule} is a vector, condensation rules can be specified itemwise. 
    The default is \code{GDINA} for all items.
}
\item{regular_lam}{Regularization parameter \eqn{\lambda}}
\item{regular_type}{Type of regularization. Can be \code{scad} (SCAD penalty),
\code{lasso} (lasso penalty), \code{ridge} (ridge penalty),
\code{elnet} (elastic net), \code{scadL2} (SCAD-\eqn{L_2};
Zeng & Xie, 2014), \code{tlp} (truncated \eqn{L_1} penalty; Xu & Shang, xxxx;
Shen, Pan, & Zhu, 2012), \code{mcp} (MCP penalty; Zhang, 2010)
or \code{none} (no regularization).}
\item{regular_alpha}{Regularization parameter \eqn{\alpha} 
(applicable for elastic net or SCAD-L2.}
\item{regular_tau}{Regularization paramezer \eqn{\tau} for truncated
\eqn{L_1} penalty.}
\item{mono.constr}{Logical indicating whether monotonicity constraints
should be fulfilled in estimation.}
\item{prior_intercepts}{Vector with mean and standard deviation for prior
of random intercepts (applies to all items)}
\item{prior_slopes}{Vector with mean and standard deviation for prior
of random slopes (applies to all items and all parameters)}
\item{progress}{An optional logical indicating whether the function
    should print the progress of iteration in the estimation process.  }     
  \item{progress.item}{
    An optional logical indicating whether item wise progress should
    be displayed
        }
\item{mstep_iter}{Number of iterations in M-step if \code{method="ML"}.}        
\item{mstep_conv}{Convergence criterion in M-step if \code{method="ML"}.}        
 \item{increment.factor}{A factor larger than 1 (say 1.1) to control
 	 maximum increments in item parameters. This parameter 
 	 can be used in case of nonconvergence. 	 
 }
 \item{fac.oldxsi}{A convergence acceleration factor between 0 and 1 which 
 	 defines the weight of previously estimated values in
 	current parameter updates.}

\item{max.increment}{Maximum size of change in increments in M steps
of EM algorithm when \code{method="ML"} is used.} 	
 	
\item{avoid.zeroprobs}{An optional logical indicating whether for estimating
	item parameters probabilities occur. Especially if
	not a skill classes are used, it is recommended to switch
	the argument to \code{TRUE}.
		}

\item{seed}{Simulation seed for initial parameters. A value of zero corresponds
to deterministic starting values, an integer value different from
zero to random initial values with \code{set.seed(seed)}.}

\item{save.devmin}{An optional logical indicating whether intermediate
estimates should be saved corresponding to minimal deviance. 
Setting the argument to \code{FALSE} could help for preventing
working memory overflow.}
\item{calc.se}{Optional logical indicating whether standard
errors should be calculated.}
\item{se_version}{Integer for calculation method of standard errors.
	\code{se_version=1} is based on the observed log likelihood and included
	since \pkg{CDM} 5.1 and is the default. Comparability with 
    previous \pkg{CDM} versions can be obtained with \code{se_version=0}. 
}
\item{PEM}{Logical indicating whether the P-EM acceleration should be
applied (Berlinet & Roland, 2012).}
\item{PEM_itermax}{Number of iterations in which the P-EM method should be
applied.} 
\item{cd}{Logical indicating whether coordinate descent algorithm should be used.}
\item{cd_steps}{Number of steps for each parameter in coordinate descent algorithm}
\item{mono_maxiter}{Maximum number of iterations for fulfilling the
monotonicity constraint}
\item{freq_weights}{Logical indicating whether frequency weights should
be used. Default is \code{FALSE}.}
  \item{object}{A required object of class \code{gdina}, obtained
    from a call to the function \code{\link{gdina}}.  }
  \item{digits}{Number of digits after decimal separator to display.  }
\item{file}{Optional file name for a file in which \code{summary}
should be sinked.}      
  \item{x}{A required object of class \code{gdina} }
  \item{ask}{A logical indicating whether every separate item should
  be displayed in \code{plot.gdina}} 
  \item{\dots}{Optional parameters to be passed to or from other
    methods will be ignored.  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DETAILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\details{
    The estimation is based on an EM algorithm as described in de la Torre (2011). 
    Item parameters are contained in the \code{delta} vector which is a list where
    the \eqn{j}th entry corresponds to item parameters of the \eqn{j}th item. 
    
    The following description refers to the case of dichotomous attributes.
    For using polytomous attributes see Chen and de la Torre (2013) and 
    Example 7 for a definition of the Q-matrix. In this case, \eqn{Q_{ik}=l}
    means that the \eqn{i}th item requires the mastery (at least) of level 
    \eqn{l} of attribute \eqn{k}. 
    
    Assume that two skills \eqn{\alpha_1} and \eqn{\alpha_2} are required for 
    mastering item \eqn{j}. Then the GDINA model can be written as
    \deqn{ g [ P( X_{nj} = 1 | \alpha_n ) ] = \delta_{j0} + \delta_{j1} \alpha_{n1} +
            \delta_{j2} \alpha_{n2} + \delta_{j12} \alpha_{n1} \alpha_{n2}
        }
which is a two-way GDINA-model (the \code{rule="GDINA2"} specification) with a 
link function \eqn{g} (which can be the identity, logit or logarithmic link). 
If the specification
\code{ACDM} is chosen, then \eqn{\delta_{j12}=0}. 
The DINA model (\code{rule="DINA"}) assumes \eqn{ \delta_{j1} = \delta_{j2} = 0}.

For the reduced RUM model (\code{rule="RRUM"}), the item response model is
\deqn{P(X_{nj}=1 | \alpha_n ) = \pi_i^\ast \cdot r_{i1}^{1-\alpha_{i1} } 
	\cdot r_{i2}^{1-\alpha_{i2} } } From this equation, it is obvious, that
this model is equivalent to an additive model (\code{rule="ACDM"}) with
a logarithmic link function (\code{linkfct="log"}).

If a reduced skillspace (\code{reduced.skillspace=TRUE}) is employed, then the
logarithm of probability distribution of the attributes is modelled as a
log-linear model:
\deqn{ \log P[ ( \alpha_{n1} , \alpha_{n2} , \ldots , \alpha_{nK} ) ]
	= \gamma_0 + \sum_k \gamma_k \alpha_{nk} +
	  \sum_{k < l} \gamma_{kl} \alpha_{nk} \alpha_{nl} }

If a higher order DINA model is assumed (\code{HOGDINA=1}), then a higher order
factor \eqn{\theta_n} for the attributes is assumed:
\deqn{P( \alpha_{nk} = 1 | \theta_n ) = \Phi ( a_k \theta_n + b_k ) }

For \code{HOGDINA=0}, all attributes \eqn{\alpha_{nk}} are assumed to be
independent of each other:
\deqn{ P[ ( \alpha_{n1} , \alpha_{n2} , \ldots , \alpha_{nK} ) ]
	= \prod_k P( \alpha_{nk} ) }


Note that the noncompensatory reduced RUM (NC-RRUM) according 
to Rupp and Templin (2008) is the GDINA model with the arguments
\code{rule="ACDM"} and \code{linkfct="log"}. NC-RRUM can also be
obtained by choosing \code{rule="RRUM"}.

The compensatory RUM (C-RRUM) can be obtained by using the arguments
\code{rule="ACDM"} and \code{linkfct="logit"}.

    }
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VALUES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    
    
\value{
An object of class \code{gdina} with following entries  
\item{coef}{Data frame of item parameters}
\item{delta}{List with basis item parameters}
\item{se.delta}{Standard errors of basis item parameters}
\item{probitem}{Data frame with model implied conditional item probabilities
	\eqn{P(X_i=1 | \bold{\alpha})}. These probabilities are displayed
	in \code{plot.gdina}.
}
\item{itemfit.rmsea}{The RMSEA item fit index (see \code{\link{itemfit.rmsea}}).
            }
\item{mean.rmsea}{Mean of RMSEA item fit indexes.
            }            
\item{loglike}{Log-likelihood}
\item{deviance}{Deviance}
\item{G}{Number of groups}
\item{N}{Sample size}
\item{AIC}{AIC}
\item{BIC}{BIC}
\item{CAIC}{CAIC}
\item{Npars}{Total number of parameters}
\item{Nipar}{Number of item parameters}
\item{Nskillpar}{Number of parameters for skill class distribution}
\item{Nskillclasses}{Number of skill classes}
\item{varmat.delta}{Covariance matrix of \eqn{\delta} item parameters}
\item{posterior}{Individual posterior distribution}
\item{like}{Individual likelihood}
\item{data}{Original data}
\item{q.matrix}{Used Q-matrix}
\item{pattern}{Individual patterns, individual MLE and MAP classifications
    and their corresponding probabilities}
\item{attribute.patt}{Probabilities of skill classes}
\item{skill.patt}{Marginal skill probabilities}
\item{subj.pattern}{Individual subject pattern}
\item{attribute.patt.splitted}{Splitted attribute pattern}
\item{pjk}{Array of item response probabilities}
\item{Mj}{Design matrix \eqn{M_j} in GDINA algorithm (see de la Torre, 2011)}  
\item{Aj}{Design matrix \eqn{A_j} in GDINA algorithm (see de la Torre, 2011)}
\item{rule}{Used condensation rules}
\item{linkfct}{Used link function}
\item{delta.designmatrix}{Designmatrix for item parameters}
\item{reduced.skillspace}{A logical if skillspace reduction was performed}
\item{Z.skillspace}{Design matrix for skillspace reduction}
\item{beta}{Parameters \eqn{\delta} for skill class representation}
\item{covbeta}{Standard errors of \eqn{\delta} parameters}
\item{iter}{Number of iterations}
\item{rrum.params}{Parameters in the parametrization of the reduced RUM model
	if \code{rule="RRUM"}.}
\item{group.stat}{Group statistics (sample sizes, group labels)}                
\item{HOGDINA}{The used value of \code{HOGDINA}}
\item{mono.constr}{Monotonocity constraint}
\item{regularization}{Logical indicating whether regularization is used}
\item{regular_lam}{Regularization parameter}
\item{numb_bound_mono}{Number of items with parameters at boundary of
monotonicity constraints}
\item{numb_regular_pars}{Number of regularized item parameters}
\item{cd_algorithm}{Logical indicating whether coordinate descent algorithm is
used}
\item{cd_steps}{Number of steps for each parameter in coordinate descent algorithm} 
\item{seed}{Used simulation seed}
\item{a.attr}{Attribute parameters \eqn{a_k} in case of \code{HOGDINA>=0}}
\item{b.attr}{Attribute parameters \eqn{b_k} in case of \code{HOGDINA>=0}}
\item{attr.rf}{Attribute response functions. This matrix contains all 
	\eqn{a_k} and \eqn{b_k} parameters}
\item{converged}{Logical indicating whether convergence was achieved.}
\item{control}{Optimization parameters used in estimation}
\item{partable}{Parameter table for \code{gdina} function}
\item{polychor}{Group-wise matrices with polychoric correlations}
\item{sequential}{Logical indicating whether a sequential GDINA model
is applied for polytomous item responses}
\item{\dots}{Further values}
    }
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REFERENCES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
\references{
Berlinet, A. F., & Roland, C. (2012). 
Acceleration of the EM algorithm: P-EM versus epsilon algorithm.
\emph{Computational Statistics & Data Analysis}, \bold{56}(12), 4122-4137.

  Chen, J., & de la Torre, J. (2013). 
  A general cognitive diagnosis model for expert-defined polytomous attributes.
  \emph{Applied Psychological Measurement}, \bold{37}, 419-437.
  
  de la Torre, J., & Douglas, J. A. (2004). Higher-order latent trait models 
  for cognitive diagnosis. \emph{Psychometrika}, \bold{69}, 333-353.
  
  de la Torre, J. (2011). The generalized {DINA} model framework. 
  \emph{Psychometrika}, \bold{76}, 179-199.


Ma, W., & de la Torre, J. (2016).
A sequential cognitive diagnosis model for polytomous responses.
\emph{British Journal of Mathematical and Statistical Psychology, 69}(3),
253-275.
  
  Rupp, A. A., & Templin, J. (2008). Unique characteristics of
  diagnostic classification models: A comprehensive review of the current
  state-of-the-art. \emph{Measurement: Interdisciplinary Research and
  Perspectives}, \bold{6}, 219-262.

Shen, X., Pan, W., & Zhu, Y. (2012). Likelihood-based selection and sharp
parameter estimation.
\emph{Journal of the American Statistical Association}, \bold{107}, 223-232.
  
Tutz, G. (1997). Sequential models for ordered responses. 
In W. van der Linden & R. K. Hambleton.
\emph{Handbook of modern item response theory} (pp. 139-152). 
New York: Springer.  

Xu, G., & Shang, Z. (xxxx). Identying latent structures in 
restricted latent class models.
\emph{Journal of the American Statistical Association}, \bold{xxx}, xxx-xxx.

Xu, X., & von Davier, M. (2008). \emph{Fitting the structured general diagnostic
model to NAEP data}. ETS Research Report ETS RR-08-27. Princeton, ETS.
  
Zeng, L., & Xie, J. (2014). Group variable selection via
SCAD-\eqn{L_2}. \emph{Statistics}, \bold{48}, 49-66.

Zhang, C.-H. (2010). Nearly unbiased variable selection under minimax concave 
penalty. \emph{Annals of Statistics}, \bold{38}, 894-942.
}
    
        
    
\note{    
    The function \code{\link{din}} does not allow for multiple group estimation.
    Use this \code{gdina} function instead and choose the appropriate \code{rule="DINA"} 
    as an argument.
    
    Standard error calculation in analyses which use sample weights or 
    designmatrix for delta parameters (\code{delta.designmatrix!=NULL}) is not yet 
    correctly implemented. Please use replication methods instead.    
    }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the \code{\link{din}} function (for DINA and DINO estimation).

For assessment of model fit see \code{\link{modelfit.cor.din}} and 
\code{\link{anova.gdina}}.

See \code{\link{itemfit.sx2}} for item fit statistics.  

See \code{\link{sim.gdina}} for simulating the GDINA model.

See \code{\link{gdina.wald}} for a Wald test for testing the DINA and ACDM
rules at the item-level.

See \code{\link{gdina.dif}} for assessing differential item
functioning.

See the \code{\link[GDINA:GDINA]{GDINA::GDINA}} function in the
\pkg{GDINA} package for similar functionality.
}


\examples{
#############################################################################
# EXAMPLE 1: Simulated DINA data | different condensation rules 
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

dat <- sim.dina
Q <- sim.qmatrix

#***
# Model 1: estimation of the GDINA model (identity link)
mod1 <- CDM::gdina( data = dat,  q.matrix = Q)
summary(mod1)
plot(mod1) # apply plot function

\dontrun{
# Model 1a: estimate model with different simulation seed
mod1a <- CDM::gdina( data = dat ,  q.matrix = Q , seed=9089)
summary(mod1a)

# Model 1b: estimate model with some fixed delta parameters
delta.fixed <- as.list( rep(NA,9) )		# List for parameters of 9 items
delta.fixed[[2]] <- c( 0 , .15 , .15 , .45 )
delta.fixed[[6]] <- c( .25 , .25 )
mod1b <- CDM::gdina( data = dat ,  q.matrix = Q , delta.fixed=delta.fixed)
summary(mod1b)

# Model 1c: fix all delta parameters to previously fitted model
mod1c <- CDM::gdina( data = dat ,  q.matrix = Q , delta.fixed=mod1$delta)
summary(mod1c)

# Model 1d: estimate GDINA model with GDINA package
mod1d <- GDINA::GDINA( dat = dat , Q = Q , model = "GDINA" )
summary(mod1d)
# extract item parameters
GDINA::itemparm(mod1d)
GDINA::itemparm(mod1d, what="delta")
# compare likelihood
logLik(mod1)
logLik(mod1d)

#***
# Model 2: estimation of the DINA model with gdina function
mod2 <- CDM::gdina( data = dat ,  q.matrix = Q , rule="DINA")
summary(mod2)
plot(mod2)

#***
# Model 2b: compare results with din function
mod2b <- CDM::din( data = dat ,  q.matrix = Q , rule="DINA")
summary(mod2b)

# Model 2: estimation of the DINO model with gdina function
mod3 <- CDM::gdina( data = dat ,  q.matrix = Q , rule="DINO")
summary(mod3)

#***
# Model 4: DINA model with logit link
mod4 <- CDM::gdina( data = dat, q.matrix = Q, rule="DINA", linkfct = "logit" )
summary(mod4)

#***
# Model 5: DINA model log link
mod5 <- CDM::gdina( data = dat, q.matrix = Q, rule="DINA", linkfct = "log")
summary(mod5)

#***
# Model 6: RRUM model
mod6 <- CDM::gdina( data = dat, q.matrix = Q, rule="RRUM")
summary(mod6)

#***
# Model 7: Higher order GDINA model
mod7 <- CDM::gdina( data = dat, q.matrix = Q, HOGDINA=1)
summary(mod7)

#***
# Model 8: GDINA model with independent attributes
mod8 <- CDM::gdina( data = dat, q.matrix = Q, HOGDINA=0)
summary(mod8)

#***
# Model 9: Estimating the GDINA model with monotonocity constraints
mod9 <- CDM::gdina( data = dat, q.matrix = Q, rule="GDINA", 
              mono.constr=TRUE, linkfct="logit")
summary(mod9)
              
#***
# Model 10: Estimating the ACDM model with SCAD penalty and regularization
#           parameter of .05
mod10 <- CDM::gdina( data = dat, q.matrix = Q, rule="ACDM",
                linkfct="logit", regular_type="scad", regular_lam=.05 )
summary(mod10)                

#***
# Model 11: Estimation of GDINA model with prior distributions

# N(0,10^2) prior for item intercepts
prior_intercepts <- c(0,10)
# N(0,1^2) prior for item slopes
prior_slopes <- c(0,1)
# estimate model
mod11 <- CDM::gdina( data = dat, q.matrix = Q, rule="GDINA", 
              prior_intercepts=prior_intercepts, prior_slopes = prior_slopes)
summary(mod11)

#############################################################################
# EXAMPLE 2: Simulated DINO data
#    additive cognitive diagnosis model with different link functions
#############################################################################

data(sim.dino, package="CDM")
data(sim.matrix, package="CDM")

dat <- sim.dino
Q <- sim.qmatrix

#***
# Model 1: additive cognitive diagnosis model (ACDM; identity link)
mod1 <- CDM::gdina( data=dat, q.matrix=Q,  rule="ACDM")
summary(mod1)

#***
# Model 2: ACDM logit link
mod2 <- CDM::gdina( data=dat, q.matrix=Q, rule="ACDM", linkfct="logit")
summary(mod2)

#***
# Model 3: ACDM log link
mod3 <- CDM::gdina( data=dat, q.matrix=Q, rule="ACDM", linkfct="log")
summary(mod3)

#***
# Model 4: Different condensation rules per item
I <- 9      # number of items
rule <- rep( "GDINA", I )
rule[1] <- "DINO"   # 1st item: DINO model
rule[7] <- "GDINA2" # 7th item: GDINA model with first- and second-order interactions
rule[8] <- "ACDM"   # 8ht item: additive CDM
rule[9] <- "DINA"   # 9th item: DINA model
mod4 <- CDM::gdina( data=dat, q.matrix=Q, rule=rule )
summary(mod4)

#############################################################################
# EXAMPLE 3: Model with user-specified design matrices
#############################################################################

data(sim.dino, package="CDM")
data(sim.qmatrix, package="CDM")

dat <- sim.dino
Q <- sim.qmatrix

# do a preliminary analysis and modify obtained design matrices
mod0 <- CDM::gdina( data = dat ,  q.matrix = Q ,  maxit=1)

# extract default design matrices
Mj <- mod0$Mj
Mj.user <- Mj   # these user defined design matrices are modified.
#~~~  For the second item, the following model should hold
#     X1 ~ V2 + V2*V3
mj <- Mj[[2]][[1]]
mj.lab <- Mj[[2]][[2]]
mj <- mj[,-3]
mj.lab <- mj.lab[-3]
Mj.user[[2]] <- list( mj , mj.lab )
#    [[1]]
#        [,1] [,2] [,3]
#    [1,]    1    0    0
#    [2,]    1    1    0
#    [3,]    1    0    0
#    [4,]    1    1    1
#    [[2]]
#    [1] "0"   "1"   "1-2"    
#~~~  For the eight item an equality constraint should hold
#     X8 ~ a*V2 + a*V3 + V2*V3
mj <- Mj[[8]][[1]]
mj.lab <- Mj[[8]][[2]]
mj[,2] <- mj[,2] + mj[,3]
mj <- mj[,-3]
mj.lab <- c("0" , "1=2" , "1-2" )
Mj.user[[8]] <- list( mj , mj.lab )
Mj.user[[8]]
  ##   [[1]]
  ##        [,1] [,2] [,3]
  ##   [1,]    1    0    0
  ##   [2,]    1    1    0
  ##   [3,]    1    1    0
  ##   [4,]    1    2    1
  ##   
  ##   [[2]]
  ##   [1] "0"   "1=2" "1-2"
mod <- CDM::gdina( data = dat ,  q.matrix = Q ,
                    Mj = Mj.user ,  maxit=200 )
summary(mod)

#############################################################################
# EXAMPLE 4: Design matrix for delta parameters
#############################################################################

data(sim.dino, package="CDM")
data(sim.qmatrix, package="CDM")

#~~~ estimate an initial model
mod0 <- CDM::gdina( data = dat ,  q.matrix = Q , rule="ACDM" , maxit=1)
# extract coefficients
c0 <- mod0$coef
I <- 9  # number of items
delta.designmatrix <- matrix( 0 , nrow= nrow(c0) , ncol = nrow(c0) )
diag( delta.designmatrix) <- 1
# set intercept of item 1 and item 3 equal to each other
delta.designmatrix[ 7 , 1 ] <- 1 ; delta.designmatrix[,7] <- 0
# set loading of V1 of item1 and item 3 equal
delta.designmatrix[ 8 , 2 ] <- 1 ; delta.designmatrix[,8] <- 0
delta.designmatrix <- delta.designmatrix[ , -c(7:8) ]       
                # exclude original parameters with indices 7 and 8

#***
# Model 1: ACDM with designmatrix
mod1 <- CDM::gdina( data = dat ,  q.matrix = Q ,  rule="ACDM" , 
            delta.designmatrix = delta.designmatrix )
summary(mod1)            

#***
# Model 2: Same model, but with logit link instead of identity link function
mod2 <- CDM::gdina( data=dat,  q.matrix=Q,  rule="ACDM",
            delta.designmatrix = delta.designmatrix, linkfct = "logit")
summary(mod2)            

#############################################################################
# EXAMPLE 5: Multiple group estimation
#############################################################################

# simulate data
set.seed(9279)
N1 <- 200 ; N2 <- 100   # group sizes
I <- 10                 # number of items
q.matrix <- matrix(0,I,2)   # create Q-matrix
q.matrix[1:7,1] <- 1 ; q.matrix[ 5:10,2] <- 1
# simulate first group
dat1 <- sim.din(N1, q.matrix=q.matrix , mean = c(0,0) )$dat
# simulate second group
dat2 <- sim.din(N2, q.matrix=q.matrix , mean = c(-.3 , -.7) )$dat
# merge data
dat <- rbind( dat1 , dat2 )
# group indicator 
group <- c( rep(1,N1) , rep(2,N2) )

# estimate GDINA model with multiple groups assuming invariant item parameters
mod1 <- CDM::gdina( data = dat , q.matrix = q.matrix ,  group= group)
summary(mod1)

# estimate DINA model with multiple groups assuming invariant item parameters
mod2 <- CDM::gdina( data = dat , q.matrix = q.matrix , group= group , rule="DINA")
summary(mod2) 

# estimate GDINA model with noninvariant item parameters
mod3 <- CDM::gdina( data = dat , q.matrix = q.matrix ,  group= group, invariance=FALSE)
summary(mod3)

# estimate GDINA model with some invariant item parameters (I001, I006, I008)
mod4 <- CDM::gdina( data = dat , q.matrix = q.matrix ,  group= group, 
            invariance= c("I001", "I006","I008") )

#--- model comparison
IRT.compareModels(mod1,mod2,mod3,mod4)                      

# estimate GDINA model with non-invariant item parameters except for the
# items I001, I006, I008
mod5 <- CDM::gdina( data = dat , q.matrix = q.matrix ,  group= group, 
            invariance= setdiff( colnames(dat) , c("I001", "I006","I008") ) )

#############################################################################
# EXAMPLE 6: User specified reduced skill space
#############################################################################

#   Some correlations between attributes should be set to zero.
q.matrix <- expand.grid( c(0,1) , c(0,1) , c(0,1) , c(0,1) )
colnames(q.matrix) <- colnames( paste("Attr" , 1:4 ,sep=""))
q.matrix <- q.matrix[ -1 , ]
Sigma <- matrix( .5 , nrow=4 , ncol=4 )
diag(Sigma) <- 1
Sigma[3,2] <- Sigma[2,3] <- 0 # set correlation of attribute A2 and A3 to zero
dat <- CDM::sim.din( N=1000 , q.matrix = q.matrix , Sigma = Sigma)$dat

#~~~ Step 1: initial estimation
mod1a <- CDM::gdina( data=dat , q.matrix = q.matrix , maxit= 1 , rule="DINA")
# estimate also "full" model
mod1 <- CDM::gdina( data=dat , q.matrix = q.matrix , rule="DINA")

#~~~ Step 2: modify designmatrix for reduced skillspace
Z.skillspace <- data.frame( mod1a$Z.skillspace )
# set correlations of A2/A4 and A3/A4 to zero
vars <- c("A2_A3","A2_A4") 
for (vv in vars){ Z.skillspace[,vv] <- NULL }

#~~~ Step 3: estimate model with reduced skillspace
mod2 <- CDM::gdina( data=dat, q.matrix = q.matrix, 
              Z.skillspace=Z.skillspace, rule="DINA")

#~~~ eliminate all covariances
Z.skillspace <- data.frame( mod1$Z.skillspace )
colnames(Z.skillspace)
Z.skillspace <- Z.skillspace[, -grep( "_" , colnames(Z.skillspace),fixed=TRUE)]
colnames(Z.skillspace)

mod3 <- CDM::gdina( data=dat , q.matrix = q.matrix , 
               Z.skillspace=Z.skillspace , rule="DINA")
summary(mod1)
summary(mod2)
summary(mod3)

#############################################################################
# EXAMPLE 7: Polytomous GDINA model (Chen & de la Torre, 2013)
#############################################################################

data(data.pgdina, package="CDM")

dat <- data.pgdina$dat
q.matrix <- data.pgdina$q.matrix

# pGDINA model with "DINA rule"
mod1 <- CDM::gdina( dat, q.matrix=q.matrix, rule="DINA")
summary(mod1)
# no reduced skill space
mod1a <- CDM::gdina( dat, q.matrix=q.matrix, rule="DINA",reduced.skillspace=FALSE)
summary(mod1)

# pGDINA model with "GDINA rule"
mod2 <- CDM::gdina( dat, q.matrix=q.matrix, rule="GDINA")
summary(mod2)

#############################################################################
# EXAMPLE 8: Fraction subtraction data: DINA and HO-DINA model
#############################################################################

data(fraction.subtraction.data, package="CDM")
data(fraction.subtraction.qmatrix, package="CDM")

dat <- fraction.subtraction.data
Q <- fraction.subtraction.qmatrix

# Model 1: DINA model
mod1 <- CDM::gdina( dat, q.matrix = Q, rule = "DINA")
summary(mod1)

# Model 2: HO-DINA model
mod2 <- CDM::gdina( dat, q.matrix = Q, HOGDINA = 1, rule = "DINA")
summary(mod2)

#############################################################################
# EXAMPLE 9: Skill space approximation data.jang
#############################################################################

data(data.jang, package="CDM")

data <- data.jang$data
q.matrix <- data.jang$q.matrix

#*** Model 1: Reduced RUM model
mod1 <- CDM::gdina( data, q.matrix, rule="RRUM"  , conv.crit=.001 , maxit=500 )

#*** Model 2: Reduced RUM model with skill space approximation
# use 300 instead of 2^9 = 512 skill classes
skillspace <- CDM::skillspace.approximation( L=300 , K=ncol(q.matrix) )
mod2 <- CDM::gdina( data , q.matrix , rule="RRUM"  , conv.crit=.001 ,
            skillclasses=skillspace )
  ##   > logLik(mod1)
  ##   'log Lik.' -30318.08 (df=153)
  ##   > logLik(mod2)
  ##   'log Lik.' -30326.52 (df=153)

#############################################################################
# EXAMPLE 10: CDM with a linear hierarchy
#############################################################################
# This model is equivalent to a unidimensional IRT model with an ordered
# ordinal latent trait and is actually a probabilistic Guttman model.
set.seed(789)

# define 3 competency levels
alpha <- scan()
   0 0 0   1 0 0   1 1 0   1 1 1

# define skill class distribution
K <- 3
skillspace <- alpha <- matrix( alpha, K + 1 ,  K , byrow= TRUE )
alpha <- alpha[ rep(  1:4 ,  c(300,300,200,200) ) , ]
# P(000)=P(100)=.3, P(110)=P(111)=.2
# define Q-matrix
Q <- scan()  
    1 0 0   1 1 0   1 1 1

Q <- matrix( Q , nrow=K ,  ncol=K , byrow= TRUE )
Q <- Q[ rep(1:K , each=4 ) , ]
colnames(skillspace) <- colnames(Q) <- paste0("A",1:K)
I <- nrow(Q)

# define guessing and slipping parameters
guess <- stats::runif( I , 0 , .3 )
slip <- stats::runif( I , 0 , .2 )
# simulate data
dat <- CDM::sim.din( q.matrix=Q, alpha=alpha, slip=slip, guess=guess )$dat

#*** Model 1: DINA model with linear hierarchy
mod1 <- CDM::din( dat, q.matrix=Q, rule="DINA",  skillclasses = skillspace )
summary(mod1)

#*** Model 2: pGDINA model with 3 levels
#    The multidimensional CDM with a linear hierarchy is a unidimensional
#    polytomous GDINA model.
Q2 <- matrix( rowSums(Q) , nrow=I , ncol=1 )
mod2 <- CDM::gdina( dat , q.matrix=Q2 , rule="DINA" )
summary(mod2)

#*** Model 3: Probabilistic Guttman Model (in sirt)
#    Proctor, C. H. (1970). A probabilistic formulation and statistical 
#    analysis for Guttman scaling. Psychometrika, 35, 73-78. 
library(sirt)
mod3 <- sirt::prob.guttman( dat , itemlevel=Q2[,1] )
summary(mod3)
# -> The three models result in nearly equivalent fit.

#############################################################################
# EXAMPLE 11: Sequential GDINA model (Ma & de la Torre, 2016)
#############################################################################

data(data.cdm04, package="CDM")

#** attach dataset
dat <- data.cdm04$data	# polytomous item responses
q.matrix1 <- data.cdm04$q.matrix1
q.matrix2 <- data.cdm04$q.matrix2

#-- DINA model with first Q-matrix
mod1 <- CDM::gdina( dat , q.matrix = q.matrix1 , rule="DINA")
summary(mod1)
#-- DINA model with second Q-matrix
mod2 <- CDM::gdina( dat , q.matrix = q.matrix2 , rule="DINA")
#-- GDINA model
mod3 <- CDM::gdina( dat , q.matrix = q.matrix2 , rule="GDINA")

#** model comparison
IRT.compareModels(mod1,mod2,mod3)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Cognitive Diagnosis Models}
\keyword{GDINA}
\keyword{DINA}
\keyword{Higher order GDINA model}
\keyword{summary}
\keyword{plot}
\keyword{print}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
