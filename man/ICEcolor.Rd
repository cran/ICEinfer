\name{ICEcolor}
\alias{ICEcolor}
\title{
 Compute Preferences for Outcomes in a Bootstrap ICE Distribution that are within a Confidence Wedge
}
\description{
 Assuming ICEw is an object of class ICEwedge, ICEcolor uses the value of lambda given
 by lfact * (ICEw item lambda) and the ICE Preference Map with parameters beta and gamma to compute
 the Economic Preference value for each point within the Bootstrap Distribution of ICE Uncertainty
 that also lies within the ICEwedge.
}
\usage{
 ICEcolor(ICEw, lfact = 1, beta = 1, gamma = 3 + 2 * sqrt(2)) 
}
\arguments{
  \item{ICEw}{Required; Existing ICEwedge object.}
  \item{lfact}{Required; Strictly positive multiplier for ICEw item lambda.}
  \item{beta}{Required; Strictly positive Returns-to-Scale power parameter for the ICE
   Preference Map.  beta = 1 implies linear (constant) Returns to Scale.  beta > 0 and < 1
   implies diminishing Returns to Scale.  beta > 1 implies increasing Returns to Scale.}
  \item{gamma}{Required; Strictly positive Directional power parameter.  The smallest
   reasonable value for gamma is usually gamma = beta, which yields a (generalized) linear
   map.  The largest reasonable value for gamma is usually gamma = beta*(3+2*sgrt(2)), which
   yields a map that satisfies the Cartesian Monotonicity Axiom and also yields finite WTP and
   WTA values greater than or equat to 0 but less than +Inf.}
}
\details{Multiple calls to ICEcolor() are usually made for different lfact multipliers of
  x item lambda as well as different choices for the ICE Preference power parameters, beta and
  gamma.  Calls to plot(x, alibi) for these alternative ICEcolor x-objects can be used to
  illustrate that Economic Uncertainty can literally SWAMP the Statistical Uncertainty within
  patient level data on the relative cost and effectiveness of two treatments.  
}
\value{
  Object of class ICEcolor containing an output list with the following items:
  \item{df}{Saved value of the name of the data.frame input to ICEcolor.}
  \item{lambda}{Saved positive value of lambda input to ICEcolor.}
  \item{unit}{Saved value of unit, cost or effe, input to ICEcolor.}
  \item{R}{Saved integer value for number of bootstrap replications input to ICEcolor.}
  \item{trtm}{Saved name of the treatment indicator within the df data.frame.}
  \item{xeffe}{Saved name of the treatment effectiveness variable within the df data.frame.}
  \item{ycost}{Saved name of the treatment cost variable within the df data.frame.}
  \item{effcst}{Saved value of the sorted 3-variable (trtm,effe,cost) data.frame.}
  \item{t1}{Observed value of (DeltaEffe, DeltaCost) when each patient is sampled exactly once.}
  \item{t}{R x 2 matrix of values of (DeltaEffe, DeltaCost) computed from bootstrap resamples.}
  \item{seed}{Saved value of the seed used to start pseudo random number generation.}
}
\references{
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
 
 Obenchain RL.  ICE Preference Maps: Nonlinear Generalizations of Net Benefit and Acceptability.
 \emph{Lilly US Health Outcomes White Paper}.  2007; 52 pages.
 
 Obenchain RL.  \bold{ICEinR.pdf}  ../R\_HOME/library/ICEinfer  2007; 30 pages.
}
\author{Bob Obenchain <softrx@iquest.net>}
\seealso{\code{\link{ICEwedge}}, \code{\link{plot.ICEcolor}} and \code{\link{print.ICEcolor}}}
\examples{
  data(dpwdg)
  dpcol <- ICEcolor(dpwdg)
  dpcol
  plot(dpcol)
  dpcolX <- ICEcolor(dpwdg, lfact=10)
  dpcolX
  plot(dpcolX)
}
\keyword{methods}
