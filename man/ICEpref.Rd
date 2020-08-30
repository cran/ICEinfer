\name{ICEpref}
\alias{ICEpref}
\title{Compute ICE Economic Preference Scores from Numeric Cost and Effe Measures}
\description{
  This function calculates Net Monetary Benefit (NMB) Scores that are expressed in Cost units. While
  these scores are of "linear" form, (lambda * Effe) - Cost, when eta == gamma / beta == 1, they are
  "Nonlinear" when eta is either > 1 or < 1.
}
\usage{
  ICEpref(x, y, lambda = 1, beta = 1, eta = 3 + 2 * sqrt(2)) 
}
\arguments{
  \item{x}{Given numeric values representing Effe measures observed for 1 or more experimental units.}
  \item{y}{Corresponding numeric values measuring Cost for the same experimental units. Thus, x and
   y must be of the same length.}
  \item{lambda}{Positive value for the fair, full-retail Shadow Price of Health. When x is
   measured in QALYs and y in U.S. Dollars, lambda = 50000 is traditionally assumed appropriate.}
  \item{beta}{Positive Returns-to-Scale Power parameter for ICE Preference calculations. beta = 1
   implies linear (constant) Returns-to-Scale.  A beta > 0 but < 1 implies Returns-to-Scale that
   are diminishing and tend to be highly realistic.  Any beta > 1 implies increasing Returns-to-Scale.}
  \item{eta}{Positive Power Parameter Ratio: gamma / beta.  Generalized "linear" maps result when
   eta = 1.  An eta ratio > 1 can yield a realistic "Nonlinear" map, but maps with eta > 3+2*sgrt(2)
   = 5.828 (Obenchain's "Omega") violate the traditional "Cartesian Monotonicity" Axiom.}
}
\value{
  Object of class "numeric" of the same length as the x and y input objects:
  \item{pref}{Computed ICE Economic Preference Scores.}
}
\references{
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
 
 Obenchain RL.  ICE Preference Maps: Nonlinear Generalizations of Net Benefit and Acceptability.
 \emph{Health Serv Outcomes Res Method}  2008; \bold{8}: 31-56. DOI 10.1007/s10742-007-0027-2.
 Open Access.
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{plot.ICEepmap}} and \code{\link{print.ICEepmap}}}
\examples{
  data(sepsis)
  icedr <- ICEpref(sepsis$qalypres, sepsis$totcost, lambda=50000, beta=0.1) # Diminishing Returns.
  summary(icedr)
}
\keyword{methods} 
