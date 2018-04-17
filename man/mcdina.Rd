%% File Name: mcdina.Rd
%% File Version: 0.26

\name{mcdina}
\alias{mcdina}
\alias{summary.mcdina}
\alias{print.mcdina}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Multiple Choice DINA Model
}


\description{
The function \code{mcdina} implements the multiple choice DINA model 
(de la Torre, 2009) for multiple groups. Note that the dataset must contain 
integer values \eqn{1,\ldots , K_j} for each item. The multiple choice
DINA model assumes that each item category possesses some diagnostic capacity.
Using this modeling approach, different distractors of a 
multiple choice item can be of different diagnostic value. The Q-matrix can also
contain integer values which allows the definition of polytomous attributes.

}
\usage{
mcdina(dat, q.matrix, group = NULL, itempars = "gr", weights = NULL, 
    skillclasses = NULL, zeroprob.skillclasses = NULL, 
    reduced.skillspace=TRUE , conv.crit = 1e-04, 
    dev.crit = 0.1, maxit = 1000, progress = TRUE)

\method{summary}{mcdina}(object, digits=4 , file = NULL ,  \dots)

\method{print}{mcdina}(x, \dots)  
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A required \eqn{N \times J} data matrix containing the binary responses, 0 or 1, 
  of \eqn{N} respondents to \eqn{J} test items, where 1 denotes a correct response 
  and 0 an incorrect one. The nth row of the matrix represents the binary 
  response pattern of respondent \eqn{n}. \code{NA} values are allowed. 
}
  \item{q.matrix}{
A required matrix specifying which item category is intended to measure which skill.
The Q-matrix has \eqn{K+2} columns for a model with \eqn{K} skills.
In the first column should be the item index, in the second column the 
category integer and the rest of the columns contains the 'ordinary'
Q-matrix specification. See \code{data.cdm01$q.matrix} for the layout of such a 
Q-matrix. }
  \item{group}{
An optional vector of group identifiers for multiple group estimation.
}
  \item{itempars}{
A character or a character vector of length \eqn{J} indicating whether
item parameters should separately estimated within each group. The default
  is \code{"gr"}, for group-invariant item parameters choose \code{"jo"}.
}
  \item{weights}{
An optional vector of sample weights. 
}
  \item{skillclasses}{
An optional matrix for determining the skill space. The argument can be used 
if a user wants less than the prespecified number of \eqn{2^K} skill classes. 
}
  \item{zeroprob.skillclasses}{
An optional vector of integers which indicates which skill classes should have 
zero probability. Default is \code{NULL} (no skill classes with zero probability).
}
\item{reduced.skillspace}{An optional logical indicating whether the
	skill space should be reduced to cover only bivariate associations
among skills (see Xu & von Davier, 2008).}
  \item{conv.crit}{
Convergence criterion for change in item parameter values
}
  \item{dev.crit}{
Convergence criterion for change in deviance values
}
  \item{maxit}{
Maximum number of iterations.
}
  \item{progress}{
 	An optional logical indicating whether the function should print the 
 	progress of iteration in the estimation process. 
}
  \item{object}{
Object of class \code{mcdina}.
}
  \item{digits}{
Number of digits to display in \code{summary.mcdina}
}
\item{file}{Optional file name for a file in which \code{summary}
should be sinked.}      
\item{x}{Object of class \code{mcdina}}
\item{\dots}{Further arguments to be passed.}
}


\details{
The multiple choice DINA model defines for each item category \eqn{jc} the
necessary skills to master this attribute. Therefore, the vector of skills
\eqn{\bold{\alpha}} is transformed into item-specific latent responses
\eqn{\eta_{j}} which are functions of
\eqn{\bold{\alpha}} and Q-matrix entries \eqn{q_{jc}}
(just like in the DINA model). If there are \eqn{K_j} item categories for item \eqn{j},
then there exist at most \eqn{K_j} values of the latent response \eqn{\eta_j}.

The multiple choice DINA model estimates the item response function as
\deqn{ P( X_{nj} = k | \eta_{nj} = l ) = p_{jkl}  }
with the constraint \eqn{\sum_k p_{jkl} = 1 }.

}

\value{
A list with following entries

\item{item}{Data frame with item parameters}
\item{posterior}{Individual posterior distribution}
\item{likelihood}{Individual likelihood}
\item{ic}{List with information criteria}
\item{q.matrix}{Used Q-matrix}
\item{pik}{Array of item-category probabilities}
\item{delta}{Array of item parameters}
\item{se.delta}{Array of standard errors of item parameters}
\item{itemstat}{Data frame containing item definitions}
\item{n.ik}{Array of expected counts}
\item{deviance}{Deviance}
\item{attribute.patt}{Probabilities of latent classes}
\item{attribute.patt.splitted}{Splitted attribute pattern}
\item{skill.patt}{Marginal skill probabilities}
\item{MLE.class}{Classified skills for each student (MLE)}
\item{MAP.class}{Classified skills for each student (MAP)}
\item{EAP.class}{Classified skills for each student (EAP)}
\item{dat}{Used dataset}
\item{skillclasses}{Used skill classes}
\item{group}{Used group identifiers}
\item{lc}{Data frame containing definitions of each item category}
\item{lr}{Data frame containing the relation of each latent class and each item category}
\item{iter}{Number of iterations}
\item{itempars}{Used specification of item parameter estimation type}
\item{converged}{Logical indicating whether convergence was achieved.}
}

\references{
de la Torre, J. (2009). A cognitive diagnosis model for cognitively based 
multiple-choice options. \emph{Applied Psychological Measurement, 33}, 163-183.

  Xu, X., & von Davier, M. (2008). \emph{Fitting the structured general diagnostic
  model to NAEP data}. ETS Research Report ETS RR-08-27. Princeton, ETS.
}


%\author{
%%  ~~who you are~~
%}

\note{
If \code{dat} and \code{q.matrix} correspond to the 'ordinary format' which is used
in \code{gdina}, then the function \code{mcdina} will detect it and convert it
into the necessary format (see Example 2).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{din}} for estimating the DINA/DINO model and \code{\link{gdina}}
for estimating the GDINA model.
}


\examples{
#############################################################################
# EXAMPLE 1: Multiple choice DINA model for data.cdm01 dataset
#############################################################################

data(data.cdm01, package="CDM")

dat <- data.cdm01$data
group <- data.cdm01$group
q.matrix <- data.cdm01$q.matrix

#*** Model 1: Single group model
mod1 <- CDM::mcdina( dat=dat, q.matrix=q.matrix )
summary(mod1)

#*** Model 2: Multiple group model with group-invariant item parameters
mod2 <- CDM::mcdina( dat=dat, q.matrix=q.matrix, group=group, itempars="jo")
summary(mod2)

\dontrun{
#*** Model 3: Multiple group model with group-specific item parameters
mod3 <- CDM::mcdina( dat=dat, q.matrix=q.matrix, group=group, itempars="gr")
summary(mod3)

#*** Model 4: Multiple group model with some group-specific item parameters
itempars <- rep("jo" , ncol(dat))
itempars[ c( 2, 7, 9) ] <- "gr" # set items 2,7 and 9 group specific
mod4 <- CDM::mcdina( dat=dat, q.matrix=q.matrix, group=group, itempars=itempars)
summary(mod4)

#*** Model 5: Reduced skill space

# define skill classes
skillclasses <- scan(nlines=1)  # read only one line
    0 0 0    1 0 0    0 1 0    0 0 1    1 1 0     1 1 1    
skillclasses <- matrix( skillclasses  , ncol=3 , byrow=TRUE )
mod5 <- CDM::mcdina( dat, q.matrix=q.matrix, group=group0,  skillclasses=skillclasses )
summary(mod5)

#*** Model 6: Reduced skill space with setting zero probabilities
#             for some latent classes

# set probabilities of classes P101 P011 (6th and 7th class) to zero
zeroprob.skillclasses <- c(6,7)
mod6 <- CDM::mcdina( dat, q.matrix, group=group, zeroprob.skillclasses=zeroprob.skillclasses )
summary(mod6)

#############################################################################
# EXAMPLE 2: Using the mcdina function for estimating the DINA model
#############################################################################

data(sim.dina, package="CDM")
data(sim.qmatrix, package="CDM")

# estimate the DINA model
mod <- CDM::mcdina( sim.dina , q.matrix=sim.qmatrix )
summary(mod)

#############################################################################
# EXAMPLE 3: MCDINA model with polytomous attributes
#############################################################################

data(data.cdm02, package="CDM")
dat <- data.cdm02$data
q.matrix <- data.cdm02$q.matrix

# estimate model with polytomous attribute B1
mod1 <- CDM::mcdina( dat , q.matrix=q.matrix )
summary(mod1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Multiple choice}
\keyword{Multiple choice DINA model (MCDINA)}
\keyword{DINA}
\keyword{summary}
\keyword{print}
