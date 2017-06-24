\name{cdi.kli}
\alias{cdi.kli}
\alias{summary.cdi.kli}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cognitive Diagnostic Indices based on Kullback-Leibler Information
}
\description{
This function computes several cognitive diagnostic indices grounded
on the Kullback-Leibler information (Rupp, Henson
& Templin, 2009, Ch. 13) at the test, item, attribute and item-attribute level.
See Henson and Douglas (2005) and Henson et al. (2008) for more details.
}

\usage{
cdi.kli(object)

\method{summary}{cdi.kli}(object, digits = 2 ,  \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{\link{din}} or \code{\link{gdina}}.
For the \code{summary} method, it is the result of \code{cdi.kli}.
}
\item{digits}{Number of digits for rounding}
\item{\dots}{Further arguments to be passed}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with following entries

\item{test_disc}{Test discrimination which is the sum of all global
item discrimination indices}
\item{attr_disc}{Attribute discriminations}
\item{glob_item_disc}{Global item discriminations (Cognitive diagnostic
index)}
\item{attr_item_disc}{Attribute-specific item discrimination}
\item{KLI}{Array with Kullback-Leibler informations of all items (first dimension)
and skill classes (in the second and third dimension)}
\item{skillclasses}{Matrix containing all used skill classes in the model}
\item{hdist}{Matrix containing Hamming distance between skill classes}
\item{pjk}{Used probabilities}
\item{q.matrix}{Used Q-matrix}
\item{summary}{Data frame with test- and item-specific
discrimination statistics}
}

\references{
Henson, R., & Douglas, J. (2005). Test construction for cognitive diagnosis. 
\emph{Applied Psychological Measurement}, \bold{29}, 262-277.

Henson, R., Roussos, L., Douglas, J., & He, X. (2008). 
Cognitive diagnostic attribute-level discrimination indices. 
\emph{Applied Psychological Measurement}, \bold{32}, 275-288.

Rupp, A. A., Templin, J., & Henson, R. A. (2010). \emph{Diagnostic
Measurement: Theory, Methods, and Applications}.  New York: The Guilford
Press.
}

%\author{
%%  ~~who you are~~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
#############################################################################
# EXAMPLE 1: Examples based on sim.dina
#############################################################################	

data(sim.dina)
data(sim.qmatrix)

mod <- CDM::din( sim.dina , q.matrix = sim.qmatrix )
summary(mod)
  ##  Item parameters
  ##         item guess  slip   IDI rmsea
  ##  Item1 Item1 0.086 0.210 0.704 0.014
  ##  Item2 Item2 0.109 0.239 0.652 0.034
  ##  Item3 Item3 0.129 0.185 0.686 0.028
  ##  Item4 Item4 0.226 0.218 0.556 0.019
  ##  Item5 Item5 0.059 0.000 0.941 0.002
  ##  Item6 Item6 0.248 0.500 0.252 0.036
  ##  Item7 Item7 0.243 0.489 0.268 0.041
  ##  Item8 Item8 0.278 0.125 0.597 0.109
  ##  Item9 Item9 0.317 0.027 0.656 0.065

cmod <- CDM::cdi.kli( mod )

# attribute discrimination indices
round( cmod$attr_disc , 3 )
  ##      V1     V2     V3 
  ##   1.966  2.506 11.169 

# look at global item discrimination indices
round( cmod$glob_item_disc , 3 )
  ##  > round( cmod$glob_item_disc , 3 )
  ##  Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9 
  ##  0.594 0.486 0.533 0.465 5.913 0.093 0.040 0.397 0.656 

# correlation of IDI and global item discrimination
stats::cor( cmod$glob_item_disc , mod$IDI )
  ##  [1] 0.6927274

# attribute-specific item indices
round( cmod$attr_item_disc , 3 )            
  ##           V1    V2    V3
  ##  Item1 0.648 0.648 0.000
  ##  Item2 0.000 0.530 0.530
  ##  Item3 0.581 0.000 0.581
  ##  Item4 0.697 0.000 0.000
  ##  Item5 0.000 0.000 8.870
  ##  Item6 0.000 0.140 0.000
  ##  Item7 0.040 0.040 0.040
  ##  Item8 0.000 0.433 0.433
  ##  Item9 0.000 0.715 0.715
            
## Note that attributes with a zero entry for an item
## do not differ from zero for the attribute specific item index
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Item discrimination}
\keyword{Cognitive diagnostic discrimination}
\keyword{summary}
