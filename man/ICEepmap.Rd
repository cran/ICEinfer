\name{ICEepmap}
\alias{ICEepmap}
\alias{ICEomega}
\title{Set Parameter values that define an ICE Economic Preference Map}
\description{
 ICEepmap() and ICEomega() define ICE Preference Map Parameter Values by defining an object, pm,
 of class ICEepmap for display using print(pm) or plot(pm, xygrid).
}
\usage{
  ICEepmap(lambda = 1, beta = 1, gamma = 3 + 2 * sqrt(2)) 
  ICEomega(lambda = 1, beta = 1, eta = 3 + 2 * sqrt(2))
}
\arguments{
  \item{lambda}{Optional; Positive value for the Shadow Price of Health.}
  \item{beta}{Optional; Positive Returns-to-Scale Power parameter for the ICE Preference Map.
   beta = 1 implies linear (constant) Returns to Scale.  A beta > 0 and < 1 implies diminishing
   Returns to Scale.  A beta > 1 implies increasing Returns to Scale.}
  \item{gamma}{Optional for ICEepmap(); Positive Directional Power parameter.  The smallest
   reasonable value for gamma is usually gamma = beta, which yields a (generalized) linear map.
   The largest reasonable value for gamma is usually gamma = beta*(3+2*sgrt(2)), which yields a
   map that satisfies Cartesian Monotonicity and also yields WTP and WTA values within [0, +Inf).}
  \item{eta}{Optional for ICEomega(); Positive Power Parameter Ratio.  Generalized linear maps
   result when eta = 1.  The eta for the more realistic Nonlinear maps is greater than one, but
   not greater than the Omega limit of (3+2*sgrt(2)), which is approximately 5.828.  This upper
   limit on eta is required for Cartesian Monotonicity to hold.}
}
\details{
 The ICEepmap() and ICEomega() functions specify numerical values for the Shadow Price of Health
 Parameter, lambda, for the Returns to Scale Power Parameter, beta, and for either the Directional
 Power Parameter, gamma, or else the Power Parameter Ratio, eta = gamma / beta.
}
\value{
  Object of class ICEepmap containing an output list with the following items:
  \item{lambda}{Saved positive value of Shadow Price of Health, lambda, read by the print and
   plot methods for objects of class ICEepmap.}
  \item{beta}{Saved Positive Returns-to-Scale Power parameter, beta, read by the print and
   plot methods for objects of class ICEepmap.}
  \item{gamma}{Saved Positive Directional Power parameter, gamma, read by the print and
   plot methods for objects of class ICEepmap.}
}
\references{
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
 
 Obenchain RL.  Incremental Cost-Effectiveness (ICE) Preference Maps.  2001 \emph{JSM Proceedings
 (Biopharmaceutical Section) on CD-ROM}. (10 pages.)  Alexandria, VA: American Statistical
 Association.  2002.
 
 Obenchain RL.  ICE Preference Maps: Nonlinear Generalizations of Net Benefit and Acceptability.
 \emph{Lilly US Health Outcomes White Paper}.  2007; 52 pages.
 
 Obenchain RL.  \bold{ICEinR.pdf}  ../R\_HOME/library/ICEinfer  2007; 30 pages.
}
\author{Bob Obenchain <softrx@iquest.net>}
\seealso{\code{\link{plot.ICEepmap}} and \code{\link{print.ICEepmap}}}
\examples{
 pm <- ICEomega(beta=0.8)
 require(lattice)
 plot(pm)
}
\keyword{methods}