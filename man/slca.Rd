%% File Name: slca.Rd
%% File Version: 1.66
%% File Last Change: 2017-10-08 12:59:13

\name{slca}
\alias{slca}
\alias{summary.slca}
\alias{print.slca}
\alias{plot.slca}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Structured Latent Class Analysis (SLCA)
}


\description{
This function implements a structured latent class model for
polytomous item responses (Formann, 1992). Lasso estimation for the
item parameters is included (Chen et al., 2015; Sun et al., 2016).
}

\usage{
slca(data, group = NULL, weights = rep(1, nrow(data)), Xdes, 
  Xlambda.init = NULL, Xlambda.fixed = NULL, Xlambda.constr.V=NULL , 
  Xlambda.constr.c=NULL ,  delta.designmatrix = NULL, 
  delta.init = NULL, delta.fixed = NULL, delta.linkfct = "log" ,
  Xlambda_positive=NULL, regular_lam = 0, regular_w = NULL, regular_n = nrow(data),
  maxiter = 1000, conv = 1e-5, globconv = 1e-5, msteps = 10, convM = 5e-04, 
  decrease.increments = FALSE, oldfac=0, dampening_factor=1.01, seed=NULL, progress=TRUE,
  PEM=TRUE, PEM_itermax=maxiter, ...)
  
\method{summary}{slca}(object, file = NULL , \dots)

\method{print}{slca}(x, \dots)    

\method{plot}{slca}(x, group=1 , ... )
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{data}{
Matrix of polytomous item responses
}
  \item{group}{
Optional vector of group identifiers. For \code{plot.slca} it is
a single integer group identified.
}
  \item{weights}{
Optional vector of sample weights
}
  \item{Xdes}{
Design matrix for \eqn{x_{ijh}} with \eqn{ q_{ihjv}} entries. 
Therefore, it must be an array with four dimensions referring to
items (\eqn{i}), categories (\eqn{h}), latent classes (\eqn{j}) and
\eqn{\lambda} parameters (\eqn{v}).
}
  \item{Xlambda.init}{
Initial \eqn{\lambda_x} parameters 
}
  \item{Xlambda.fixed}{
Fixed \eqn{\lambda_x} parameters. These must be provided by a matrix
with two columns: 1st column -- Parameter index, 2nd column: 
Fixed value.
}
\item{Xlambda.constr.V}{A design matrix for linear restrictions of the 
	form \eqn{V_x \lambda_x = c_x} for the \eqn{\lambda_x} parameter.}
\item{Xlambda.constr.c}{A vector for the linear restriction
\eqn{V_x \lambda_x = c_x} of the \eqn{\lambda_x} parameter.}
  \item{delta.designmatrix}{
Design matrix for delta parameters \eqn{\delta} 
parametrizing the latent class distribution by log-linear smoothing
(Xu & von Davier, 2008)
}
  \item{delta.init}{
Initial \eqn{\delta} parameters
}
  \item{delta.fixed}{
Fixed \eqn{\delta} parameters. This must be a matrix with three columns:
1st column: Parameter index, 2nd column: Group index, 3rd column: Fixed value
}
\item{delta.linkfct}{Link function for skill space reduction.
	This can be the log-linear link (\code{log}) or the
	logistic link function (\code{logit}).
}
\item{Xlambda_positive}{Optional vector of logical indicating which
	elements of \eqn{\bold{\lambda}_x} should be constrained to be
positive.}
\item{regular_lam}{Numeric. Regularization parameter}
\item{regular_w}{Vector for weighting the regularization penalty}
\item{regular_n}{Vector of regularization factor. This will be 
typically the sample size.}
  \item{maxiter}{
Maximum number of iterations
}
  \item{conv}{
Convergence criterion for item parameters and
distribution parameters
}
  \item{globconv}{
Global deviance convergence criterion
}
\item{msteps}{
	Maximum number of M steps in estimating \eqn{b} and
	\eqn{a} item parameters. The default is to use 4 M steps.
}
\item{convM}{
	Convergence criterion in M step
		}
\item{decrease.increments}{Should in the M step the increments
    of \eqn{a} and \eqn{b} parameters decrease during iterations?
    The default is \code{FALSE}. If there is an increase in deviance
    during estimation, setting \code{decrease.increments} to \code{TRUE}
    is recommended.
}
\item{oldfac}{Factor \eqn{f} between 0 and 1 to control convergence behavior.
	If \eqn{x_t} denotes the estimated parameter in iteration \eqn{t},
	then the regularized estimate \eqn{x_t^{\ast}} is obtained by
	\eqn{x_t^{\ast} = f x_{t-1} + (1-f) x_t}. Therefore, values of
	\code{oldfac} near to one only allow for small changes in estimated
	parameters from in succeeding iterations.}
\item{dampening_factor}{Factor larger than one defining the specified decrease in
   decrements in iterations.}    	
\item{seed}{Simulation seed for initial parameters. The default
of \code{NULL} corresponds to a random seed.}
\item{progress}{An optional logical indicating whether the function
    should print the progress of iteration in the estimation process.  }
\item{PEM}{Logical indicating whether the P-EM acceleration should be
applied (Berlinet & Roland, 2012).}
\item{PEM_itermax}{Number of iterations in which the P-EM method should be
applied.}    
  \item{object}{A required object of class \code{slca}}
\item{file}{Optional file name for a file in which \code{summary}
should be sinked.}        
  \item{x}{A required object of class \code{slca}}
  \item{\dots}{Optional parameters to be passed to or from other
    methods will be ignored.  }  
}



\details{
The structured latent class model allows for general constraints of items
\eqn{i} in categories \eqn{h} and classes \eqn{j}. The item response model is
\deqn{P( X_{i} = h | j ) = \frac{ \exp( x_{ihj} ) }{ \sum_l \exp( x_{ilj} ) }}
with linear constraints on the class specific probabilities
\deqn{  x_{ihj} = \sum_v  q_{ihjv} \lambda_{xv} }

Linear restrictions on the \eqn{\lambda_x} parameter can be specified by
a matrix equation \eqn{V_x \lambda_x = c_x} (see \code{Xlambda.constr.V} and
\code{Xlambda.constr.c}; Neuhaus, 1996).

The latent class distribution can be smoothed by a log-linear
link function (Xu & von Davier, 2008) or a logistic link function
(Formann, 1992). For class \eqn{j}
in group \eqn{g} employing a link function \eqn{h}, it holds that
\deqn{ h [ P( j| g) ] \propto \sum_w   r_{jw}  \delta_{gw} }
where group-specific distributions are allowed. The values
\eqn{r_{jw}} are specified in the design matrix \code{delta.designmatrix}.

This model contains classical uni- and multidimensional latent trait models,
latent class analysis, located latent class analysis, cognitive diagnostic
models, the general diagnostic model and mixture item response models as 
special cases (see Formann & Kohlmann, 1998; Formann, 2007).

The function also allows for regularization of \eqn{\lambda_{xv}} parameters
using the lasso approach (Sun et al., 2016).
More formally, the penalty function can be written as 
\deqn{pen( \bold{\lambda}_x ) =  p_\lambda \sum_v n_v w_v | \lambda_{xv} }
where \eqn{p_\lambda} can be specified with \code{regular_lam},
\eqn{w_v} can be specified with \code{regular_w}, and
\eqn{n_v} can be specified with \code{regular_n}.
}




\value{
An object of class \code{slca}. The list contains the
following entries:
\item{item}{Data frame with conditional item probabilities}
\item{deviance}{Deviance}
\item{ic}{Information criteria, number of estimated parameters}
\item{Xlambda}{Estimated \eqn{\lambda_x} parameters }
\item{se.Xlambda}{Standard error of \eqn{\lambda_x} parameters 
}
\item{pi.k}{Trait distribution}
\item{pjk}{Item response probabilities evaluated for all classes}
\item{n.ik}{An array of expected counts \eqn{n_{cikg}} of ability class \eqn{c}
    at item \eqn{i} at category \eqn{k} in group \eqn{g}}
\item{G}{Number of groups}
\item{I}{Number of items}
\item{N}{Number of persons}
\item{delta}{Parameter estimates for skillspace representation}
\item{covdelta}{Covariance matrix of parameter estimates for
      skillspace representation}
\item{MLE.class}{Classified skills for each student (MLE)}
\item{MAP.class}{Classified skills for each student (MAP)}      
\item{data}{Original data frame}
\item{group.stat}{Group statistics (sample sizes, group labels)}
\item{p.xi.aj}{Individual likelihood}
\item{posterior}{Individual posterior distribution}
\item{K.item}{Maximal category per item}
\item{time}{Info about computation time}
\item{skillspace}{Used skillspace parametrization}
\item{iter}{Number of iterations}
\item{seed.used}{Used simulation seed}
\item{Xlambda.init}{Used initial lambda parameters}
\item{delta.init}{Used initial delta parameters}
\item{converged}{Logical indicating whether convergence was achieved.}
}

% \item{object}{Object of class \code{slca}}
% \item{\dots}{Optional parameters to be passed to or from other
%    methods will be ignored.  }  





\references{
Berlinet, A. F., & Roland, C. (2012). 
Acceleration of the EM algorithm: P-EM versus epsilon algorithm.
\emph{Computational Statistics & Data Analysis}, \bold{56}(12), 4122-4137.		

Chen, Y., Liu, J., Xu, G., & Ying, Z. (2015). 
Statistical analysis of Q-matrix based diagnostic classification models. 
\emph{Journal of the American Statistical Association}, \bold{110}, 850-866.

Formann, A. K. (1992). Linear logistic latent class analysis for polytomous data. 
\emph{Journal of the American Statistical Association}, \bold{87}, 476-486.	

Formann, A. K. (2007). (Almost) Equivalence between conditional and mixture maximum
likelihood estimates for some models of the Rasch type. In M. von Davier & C. H. Carstensen
(Eds.), \emph{Multivariate and mixture distribution Rasch models} (pp. 177-189). 
New York: Springer.

Formann, A. K., & Kohlmann, T. (1998). Structural latent class models. 
\emph{Sociological Methods & Research}, \bold{26}, 530-565.

Neuhaus, W. (1996). Optimal estimation under
linear constraints. \emph{Astin Bulletin}, \bold{26}, 233-245. 

Sun, J., Chen, Y., Liu, J., Ying, Z., & Xin, T. (2016).
Latent variable selection for multidimensional item response theory models 
via \eqn{L_1} regularization. \emph{Psychometrika}, \bold{81}(4), 921-939.

Xu, X., & von Davier, M. (2008). \emph{Fitting the structured general diagnostic
model to NAEP data}. ETS Research Report ETS RR-08-27. Princeton, ETS.
}


%\author{
%%  ~~who you are~~
%}
\note{
If some items have differing number of categories, appropriate
class probabilities in non-existing categories per items can be 
practically set to zero by loading an item for all skill classes
on a fixed \eqn{\lambda_x} parameter of a small number, e.g. \code{-999}.

The implementation of the model builds on pieces work of Anton Formann.
See \url{http://www.antonformann.at/} for more information.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For latent trait models with continuous latent variables see the
\pkg{mirt} or \pkg{TAM} packages. For a discrete trait distribution see
the \pkg{MultiLCIRT} package. 

For latent class models see the \pkg{poLCA}, \pkg{covLCA} or \pkg{randomLCA}
package.

For mixture Rasch or mixture IRT models see the \pkg{psychomix} or
\pkg{mRm} package.
}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
#############################################################################
# EXAMPLE 1: data.Students | (Generalized) Partial Credit Model
#############################################################################

data(data.Students)
dat <- data.Students[ , c("mj1","mj2","mj3","mj4","sc1" , "sc2") ]
# define discretized ability
theta.k <- seq( -6 , 6 , len=21 )

#*** Model 1: Partial credit model

# define design matrix for lambda
I <- ncol(dat)
maxK <- 4
TP <- length(theta.k)
NXlam <- I*(maxK-1) + 1       # number of estimated parameters
       # last parameter is joint slope parameter
Xdes <- array( 0 , dim=c(I , maxK , TP ,  NXlam ) )
# Item1Cat1 , ... , Item1Cat3, Item2Cat1, ..., 
dimnames(Xdes)[[1]] <- colnames(dat)
dimnames(Xdes)[[2]] <- paste0("Cat" , 1:(maxK) )
dimnames(Xdes)[[3]] <- paste0("Class" , 1:TP )
v2 <- unlist( sapply( 1:I , FUN = function(ii){ # ii
    paste0( paste0( colnames(dat)[ii]  , "_b"  ) , "Cat" , 1:(maxK-1) )
                } , simplify=FALSE) )                
dimnames(Xdes)[[4]] <- c( v2 , "a" )
# define theta design and item discriminations
for (ii in 1:I){
	for (hh in 1:(maxK-1) ){
	    Xdes[ii , hh + 1 , , NXlam ] <- hh * theta.k
	}
}
# item intercepts
for (ii in 1:I){
	for (hh in 1:(maxK-1) ){
	    # ii <- 1  # Item    # hh <- 1  # category
	    Xdes[ii,hh+1,, ( ii - 1)*(maxK-1) + hh] <- 1
	}
}
#****
# skill space designmatrix
TP <- length(theta.k)
w1 <- stats::dnorm(theta.k)
w1 <- w1 / sum(w1)  
delta.designmatrix <- matrix( 1 , nrow=TP , ncol=1 )
delta.designmatrix[,1] <- log(w1)

# initial lambda parameters
Xlambda.init <- c( stats::rnorm( dim(Xdes)[[4]] - 1 ) , 1 )
# fixed delta parameter
delta.fixed <- cbind( 1 , 1 ,1 )

# estimate model
mod1 <- CDM::slca( dat , Xdes=Xdes , delta.designmatrix=delta.designmatrix , 
            Xlambda.init=Xlambda.init , delta.fixed = delta.fixed, maxiter = 70 )
summary(mod1)
plot(mod1, cex.names=.7 )

\dontrun{
#*** Model 2: Partial credit model with some parameter constraints
# fixed lambda parameters
Xlambda.fixed <- cbind( c(1,19) , c(3.2,1.52 ) )
# 1st parameter = 3.2
# 19th parameter = 1.52 (joint item slope)
mod2 <- CDM::slca( dat , Xdes=Xdes , delta.designmatrix=delta.designmatrix , 
            delta.init=delta.init, Xlambda.init=Xlambda.init, delta.fixed=delta.fixed,
            Xlambda.fixed=Xlambda.fixed , maxiter = 70 )

#*** Model 3: Partial credit model with non-normal distribution
Xlambda.fixed <- cbind(  c(1,19) , c(3.2,1) ) # fix item slope to one
delta.designmatrix <- cbind( 1 , theta.k , theta.k^2 , theta.k^3 )
mod3 <- CDM::slca( dat , Xdes=Xdes,  delta.designmatrix=delta.designmatrix ,          
            Xlambda.fixed=Xlambda.fixed ,  maxiter = 200 )
summary(mod3)  

# non-normal distribution with convergence regularizing factor oldfac
mod3a <- CDM::slca( dat , Xdes=Xdes,  delta.designmatrix=delta.designmatrix ,          
            Xlambda.fixed=Xlambda.fixed, maxiter = 500 , oldfac=.95 )
summary(mod3a)  

#*** Model 4: Generalized Partial Credit Model

# estimate generalized partial credit model without restrictions on trait
# distribution and item parameters to ensure better convergence behavior
# Note that two parameters are not identifiable and information criteria
# have to be adapted.

#---
# define design matrix for lambda
I <- ncol(dat)
maxK <- 4
TP <- length(theta.k)
NXlam <- I*(maxK-1) + I       # number of estimated parameters
Xdes <- array( 0 , dim=c(I , maxK , TP ,  NXlam ) )
# Item1Cat1 , ... , Item1Cat3, Item2Cat1, ..., 
dimnames(Xdes)[[1]] <- colnames(dat)
dimnames(Xdes)[[2]] <- paste0("Cat" , 1:(maxK) )
dimnames(Xdes)[[3]] <- paste0("Class" , 1:TP )
v2 <- unlist( sapply( 1:I , FUN = function(ii){ # ii
    paste0( paste0( colnames(dat)[ii]  , "_b"  ) , "Cat" , 1:(maxK-1) )
                } , simplify=FALSE) )                
dimnames(Xdes)[[4]] <- c( v2 , paste0( colnames(dat),"_a") )
dimnames(Xdes)
# define theta design and item discriminations
for (ii in 1:I){
	for (hh in 1:(maxK-1) ){
	    Xdes[ii , hh + 1 , , I*(maxK-1) + ii ] <- hh * theta.k
	}
}
# item intercepts
for (ii in 1:I){
	for (hh in 1:(maxK-1) ){
	    Xdes[ii,hh+1,, ( ii - 1)*(maxK-1) + hh] <- 1
	}
}
#****
# skill space designmatrix
delta.designmatrix <- cbind( 1, theta.k,theta.k^2 )
# initial lambda parameters from partial credit model
Xlambda.init <- mod1$Xlambda
Xlambda.init <- c( mod1$Xlambda[ - length(Xlambda.init) ] , 
         rep( Xlambda.init[ length(Xlambda.init)  ] ,I) )

# estimate model
mod4 <- CDM::slca( dat , Xdes=Xdes , Xlambda.init=Xlambda.init , 
             delta.designmatrix=delta.designmatrix , decrease.increments=TRUE ,         
             maxiter = 300 )           

#############################################################################
# EXAMPLE 2: Latent class model with two classes
#############################################################################

set.seed(9876)
I <- 7	# number of items
# simulate response probabilities
a1 <- stats::runif(I, 0 , .4 )
a2 <- stats::runif(I, .6 , 1 )
N <- 1000	# sample size
# simulate data in two classes of proportions .3 and .7
N1 <- round(.3*N)
dat1 <- 1 * ( matrix(a1,N1,I,byrow=TRUE) > matrix( stats::runif( N1 * I) , N1 , I ) )
N2 <- round(.7*N)
dat2 <- 1 * ( matrix(a2,N2,I,byrow=TRUE) > matrix( stats::runif( N2 * I) , N2 , I ) )
dat <- rbind( dat1 , dat2 )
colnames(dat) <- paste0("I" , 1:I)

# define design matrices
TP <- 2     # two classes
# The idea is that latent classes refer to two different "dimensions".
# Items load on latent class indicators 1 and 2, see below.
Xdes <- array(0 , dim=c(I,2,2,2*I) )
items <- colnames(dat)
dimnames(Xdes)[[4]] <- c(paste0( colnames(dat) , "Class" , 1),
          paste0( colnames(dat) , "Class" , 2) )
    # items, categories , classes , parameters
# probabilities for correct solution
for (ii in 1:I){
    Xdes[ ii , 2 , 1 , ii ] <- 1    # probabilities class 1
    Xdes[ ii , 2 , 2 , ii+I ] <- 1  # probabilities class 2
}
# estimate model
mod1 <- CDM::slca( dat , Xdes=Xdes )            
summary(mod1)

#############################################################################
# EXAMPLE 3: Mixed Rasch model with two classes
#############################################################################

set.seed(987)
library(sirt)
# simulate two latent classes of Rasch populations
I <- 15  # 6 items
b1 <- seq( -1.5 , 1.5 , len=I)      # difficulties latent class 1
b2 <- b1    # difficulties latent class 2
b2[ c(4,7, 9 , 11 , 12, 13) ] <- c(1 , -.5 , -.5 , .33 , .33 , -.66 )
N <- 3000    # number of persons
wgt <- .25       # class probability for class 1
# class 1
dat1 <- sirt::sim.raschtype( stats::rnorm( wgt*N ) , b1 )
# class 2
dat2 <- sirt::sim.raschtype( stats::rnorm( (1-wgt)*N , mean= 1 , sd=1.7) , b2 )
dat <- rbind( dat1 , dat2 )
# theta grid
theta.k <- seq( -5 , 5 , len=9 )
TP <- length(theta.k)

#*** Model 1: Rasch model with normal distribution
maxK <- 2
NXlam <- I +1
Xdes <- array( 0 , dim=c(I , maxK , TP ,  NXlam ) )
dimnames(Xdes)[[1]] <- colnames(dat)
dimnames(Xdes)[[2]] <- paste0("Cat" , 1:(maxK) )
dimnames(Xdes)[[4]] <- c( paste0( "b_" , colnames(dat)[1:I] ) , "a" )
# define item difficulties
for (ii in 1:I){  
	Xdes[ii , 2 , , ii ] <- -1        
}
# theta design
for (tt in 1:TP){  
	Xdes[1:I , 2 , tt , I + 1] <- theta.k[tt]       
}

# skill space definition
delta.designmatrix <- cbind( 1 , theta.k^2 )
delta.fixed <- NULL
Xlambda.init <- c( stats::runif( I  , -.8 , .8 ) , 1 )
Xlambda.fixed <- cbind( I+1 , 1 )
# estimate model
mod1 <- CDM::slca( dat , Xdes=Xdes , delta.designmatrix=delta.designmatrix , 
            delta.fixed=delta.fixed , Xlambda.fixed=Xlambda.fixed , 
            Xlambda.init=Xlambda.init, decrease.increments=TRUE , maxiter = 200 )
summary(mod1)

#*** Model 1b: Constraint the sum of item difficulties to zero

# change skill space definition
delta.designmatrix <- cbind( 1 , theta.k , theta.k^2 )
delta.fixed <- NULL
# constrain sum of difficulties Xlambda parameters to zero
Xlambda.constr.V <- matrix( 1 , nrow=I+1 , ncol=1 )
Xlambda.constr.V[I+1,1] <- 0
Xlambda.constr.c <- c(0)
# estimate model
mod1b <- CDM::slca( dat , Xdes=Xdes , delta.designmatrix=delta.designmatrix ,         
            Xlambda.fixed=Xlambda.fixed , Xlambda.constr.V=Xlambda.constr.V ,
            Xlambda.constr.c=Xlambda.constr.c  )
summary(mod1b)

#*** Model 2: Mixed Rasch model with two latent classes
NXlam <- 2*I +2
Xdes <- array( 0 , dim=c(I , maxK , 2*TP ,  NXlam ) )
dimnames(Xdes)[[1]] <- colnames(dat)
dimnames(Xdes)[[2]] <- paste0("Cat" , 1:(maxK) )
dimnames(Xdes)[[4]] <- c( paste0( "bClass1_" , colnames(dat)[1:I] ) , 
        paste0( "bClass2_" , colnames(dat)[1:I] ) , "aClass1" , "aClass2" )
# define item difficulties
for (ii in 1:I){
    Xdes[ii , 2 , 1:TP , ii ] <- -1  # first class
    Xdes[ii , 2 , TP + 1:TP , I+ii ] <- -1  # second class    
}                                                                               
# theta design
for (tt in 1:TP){
    Xdes[1:I , 2 , tt , 2*I+1 ] <- theta.k[tt]
    Xdes[1:I , 2 , TP+tt , 2*I+2 ] <- theta.k[tt]
}
# skill space definition
delta.designmatrix <- matrix( 0 , nrow=2*TP , ncol=4 )
delta.designmatrix[1:TP,1] <- 1
delta.designmatrix[1:TP,2] <- theta.k^2
delta.designmatrix[TP + 1:TP,3] <- 1
delta.designmatrix[TP+ 1:TP,4] <- theta.k^2
b1 <- stats::qnorm( colMeans( dat ) )
Xlambda.init <- c( stats::runif( 2*I  , -1.8 , 1.8 ) , 1 ,1 )
Xlambda.fixed <- cbind( c(2*I+1 , 2*I+2) , 1 )
# estimate model
mod2 <- CDM::slca( dat, Xdes=Xdes,  delta.designmatrix=delta.designmatrix ,          
            Xlambda.fixed=Xlambda.fixed, decrease.increments=TRUE ,
            Xlambda.init=Xlambda.init, maxiter = 1000 )
summary(mod2)
summary(mod1)
# latent class proportions
stats::aggregate( mod2$pi.k , list( rep(1:2, each=TP)) , sum )

#*** Model 2b: Different parametrization with sum constraint on item difficulties

# skill space definition
delta.designmatrix <- matrix( 0 , nrow=2*TP , ncol=6 )
delta.designmatrix[1:TP,1] <- 1
delta.designmatrix[1:TP,2] <- theta.k
delta.designmatrix[1:TP,3] <- theta.k^2
delta.designmatrix[TP+ 1:TP,4] <- 1
delta.designmatrix[TP+ 1:TP,5] <- theta.k
delta.designmatrix[TP+ 1:TP,6] <- theta.k^2
Xlambda.fixed <- cbind( c(2*I+1,2*I+2) , c(1,1) )
b1 <- stats::qnorm( colMeans( dat ) )
Xlambda.init <- c( b1 , b1 + stats::runif(I, -1  , 1 ) , 1 , 1 )
# constraints on item difficulties
Xlambda.constr.V <- matrix( 0 , nrow=NXlam , ncol=2)
Xlambda.constr.V[1:I , 1 ] <- 1
Xlambda.constr.V[I + 1:I , 2 ] <- 1
Xlambda.constr.c <- c(0,0)
# estimate model
mod2b <- CDM::slca( dat, Xdes=Xdes, delta.designmatrix=delta.designmatrix ,          
            Xlambda.fixed=Xlambda.fixed ,  Xlambda.init=Xlambda.init ,  
            Xlambda.constr.V=Xlambda.constr.V , Xlambda.constr.c=Xlambda.constr.c , 
            decrease.increments=TRUE , maxiter = 1000 )
summary(mod2b)
stats::aggregate( mod2b$pi.k , list( rep(1:2, each=TP)) , sum )

#*** Model 2c: Estimation with mRm package
library(mRm)
mod2c <- mRm::mrm(data.matrix=dat, cl=2)
plot(mod2c)
print(mod2c)

#*** Model 2d: Estimation with psychomix package
library(psychomix)
mod2d <- psychomix::raschmix(data=dat, k=2 , verbose=TRUE )
summary(mod2d)
plot(mod2d)

#############################################################################
# EXAMPLE 4: Located latent class model, Rasch model
#############################################################################

set.seed(487)
library(sirt)
I <- 15  # I items
b1 <- seq( -2 , 2 , len=I)      # item difficulties
N <- 4000    # number of persons
# simulate 4 theta classes
theta0 <- c( -2.5 , -1 , 0.3 , 1.3 )  # skill classes
probs0 <- c( .1 , .4 , .2 , .3 )
TP <- length(theta0)
theta <- theta0[ rep(1:TP, round(probs0*N)  ) ]
dat <- sirt::sim.raschtype( theta , b1 )

#*** Model 1: Located latent class model with 4 classes
maxK <- 2
NXlam <- I + TP
Xdes <- array( 0 , dim=c(I , maxK , TP ,  NXlam ) )
dimnames(Xdes)[[1]] <- colnames(dat)
dimnames(Xdes)[[2]] <- paste0("Cat" , 1:(maxK) )
dimnames(Xdes)[[3]] <- paste0("Class" , 1:TP )
dimnames(Xdes)[[4]] <- c( paste0( "b_" , colnames(dat)[1:I] ) , paste0("theta" , 1:TP) )
# define item difficulties
for (ii in 1:I){  
	Xdes[ii , 2 , , ii ] <- -1        
}
# theta design
for (tt in 1:TP){  
	Xdes[1:I , 2 , tt , I + tt] <- 1       
}

# skill space definition
delta.designmatrix <- diag(TP)
Xlambda.init <- c( - stats::qnorm( colMeans(dat) ) , seq(-2,1,len=TP)  )
# constraint on item difficulties
Xlambda.constr.V <- matrix( 0 , nrow=NXlam , ncol=1)
Xlambda.constr.V[1:I,1] <- 1
Xlambda.constr.c <- c(0)
delta.init <- matrix( c(1,1,1,1) , TP , 1 )
# estimate model
mod1 <- CDM::slca( dat , Xdes=Xdes, delta.designmatrix=delta.designmatrix, 
            delta.init=delta.init, Xlambda.init=Xlambda.init , 
            Xlambda.constr.V=Xlambda.constr.V , Xlambda.constr.c=Xlambda.constr.c , 
            decrease.increments=TRUE,  maxiter = 400 )
summary(mod1)
# compare estimated and simulated theta class locations
cbind( mod1$Xlambda[ - c(1:I) ] , theta0 )
# compare estimated and simulated latent class proportions
cbind( mod1$pi.k , probs0 )

#############################################################################
# EXAMPLE 5: DINA model with two skills
#############################################################################

set.seed(487)
N <- 3000   # number of persons
# define Q-matrix
I <- 9  # 9 items
NS <- 2 # 2 skills 
TP <- 4 # number of skill classes
Q <- scan( nlines=3)
  1 0   1 0   1 0
  0 1   0 1   0 1
  1 1   1 1   1 1
Q <- matrix(Q , I, ncol=NS,byrow=TRUE)
# define skill distribution
alpha0 <- matrix( c(0,0,1,0,0,1,1,1) , nrow=4,ncol=2,byrow=TRUE)
prob0 <- c( .2 , .4 , .1 , .3 )
alpha <- alpha0[ rep( 1:TP , prob0*N) ,]
# define guessing and slipping parameters
guess <- round( stats::runif(I, 0 , .4 ) , 2 )
slip <- round( stats::runif(I , 0 , .3 ) , 2 )
# simulate data according to the DINA model
dat <- CDM::sim.din( q.matrix=Q , alpha=alpha , slip=slip , guess=guess )$dat

# define Xlambda design matrix
maxK <- 2
NXlam <- 2*I
Xdes <- array( 0 , dim=c(I , maxK , TP ,  NXlam ) )
dimnames(Xdes)[[1]] <- colnames(dat)
dimnames(Xdes)[[2]] <- paste0("Cat" , 1:(maxK) )
dimnames(Xdes)[[3]] <- c("S00","S10","S01","S11")
dimnames(Xdes)[[4]] <- c( paste0("guess",1:I ) , paste0( "antislip" , 1:I ) )
dimnames(Xdes)
# define item difficulties
for (ii in 1:I){  
        # define latent responses
        latresp <- 1*( alpha0 \%*\% Q[ii,]  == sum(Q[ii,]) )[,1]
        # model slipping parameters
        Xdes[ii , 2 , latresp == 1, I+ii ] <- 1
        # guessing parameters
        Xdes[ii , 2 , latresp == 0, ii ] <- 1        
}
Xdes[1,2,,] ; Xdes[7,2,,]
# skill space definition
delta.designmatrix <- diag(TP)
Xlambda.init <- c( rep( stats::qlogis( .2 ) , I ) , rep( stats::qlogis( .8 ) , I ) )

# estimate DINA model with slca function
mod1 <- CDM::slca( dat , Xdes=Xdes, delta.designmatrix=delta.designmatrix , 
            Xlambda.init=Xlambda.init, decrease.increments=TRUE, maxiter=400 )
summary(mod1)

# compare estimated and simulated latent class proportions
cbind( mod1$pi.k , probs0 )
# compare estimated and simulated guessing parameters
cbind( mod1$pjk[1,,2] , guess )
# compare estimated and simulated slipping parameters
cbind( 1 - mod1$pjk[4,,2] , slip )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Structured latent class analysis (SLCA)}
\keyword{summary}% __ONLY ONE__ keyword per line
\keyword{plot}
