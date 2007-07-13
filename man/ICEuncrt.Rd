\name{ICEuncrt}
\alias{ICEuncrt}
\title{
 Compute the Bootstrap Distribution of ICE Uncertainty for a specified value of the Shadow
 Price of Health, lambda.
}
\description{
 ICEuncrt() uses bootstrap resampling (with replacement) to compute the distribution of
 uncertainty for 2-sample, 2-variable statistical inference.  The 2 variables must be measures
 of effectiveness (higher values are better) and cost (lower values are better).  The 2 samples
 are of patients receiving only 1 of the 2 possible treatments.  The treatment called new is
 the one with the higher numerical level for the specified treatment indicator variable, while
 the treatment called std corresponds to the lower numerical level.  The pivotal statistic for
 inference is (DeltaEffe, DeltaCost), which are the head-to-head mean differences for new
 treatment minus std treatment.  Each sample is assumed to provide unbiased estimates of the
 overall expected effectiveness and cost for that treatment.
 }
\usage{
ICEuncrt(df, trtm, xeffe, ycost, lambda = 1, unit = cost, R = 25000, seed = 0)
}
\arguments{
  \item{df}{Required; Existing data.frame object containing the trtm, xeffe and ycost variables.}
  \item{trtm}{Required; Name of the treatment indicator variable contained within the df
   data.frame that assumes one of only two different numerical values for each patient.}
  \item{xeffe}{Required; Name of the treatment effectiveness variable within the df data.frame.}
  \item{ycost}{Required; Name of the treatment cost variable within the df data.frame.}
  \item{lambda}{Optional; \code{lambda} strictly positive value for the Shadow Price of Health.}
  \item{unit}{Optional; \code{unit} character string containing either cost (default) or effe.}
  \item{R}{Optional; \code{R} positive integer value for the number of Bootstrap Replications
   desired.  Minimum allowed value is 50; default value is 25000.}
  \item{seed}{Optional; \code{seed} is an integer between 0 and 25000.  A seed value of 0 causes
   a random integer seed between 1 and 25000 to be generated.  To reproduce results from a
   previous invocation of ICEuncrt(), use the seed value saved in its output list object.}
}
\details{A single call to ICEuncrt() is usually made for a particular value of the Shadow
  Price of Health, lambda.  Alternative statistical choices for lambda can be suggested by
  making calls to ICEscale() with different values for lambda.  Because the bootstrap
  distribution of ICE uncertainty is equivariant under changes in lambda, it is much faster to
  transform an existing bootstrap distribution than to generate a new one for a different value
  of lambda.  The print.ICEuncrt() and plot.ICEuncrt() functions thus have 2 special parameters,
  lfact and swa, that can change lambda and switch the units of measurement, respectively, without
  actually generating a new bootstrap distribution via a call to ICEuncrt(). 
}
\value{
  Object of class ICEuncrt containing an output list with the following items:
  \item{df}{Saved value of the name of the data.frame input to ICEuncrt.}
  \item{lambda}{Saved positive value of lambda input to ICEuncrt.}
  \item{unit}{Saved value of unit, cost or effe, input to ICEuncrt.}
  \item{R}{Saved integer value for number of bootstrap replications input to ICEuncrt.}
  \item{trtm}{Saved name of the treatment indicator within the df data.frame.}
  \item{xeffe}{Saved name of the treatment effectiveness variable within the df data.frame.}
  \item{ycost}{Saved name of the treatment cost variable within the df data.frame.}
  \item{effcst}{Saved value of the sorted 3-variable (trtm,effe,cost) data.frame.}
  \item{t1}{Observed value of (DeltaEffe, DeltaCost) when each patient is included exactly once.}
  \item{t}{R x 2 matrix of values of (DeltaEffe, DeltaCost) computed from bootstrap resamples.}
  \item{seed}{Saved value of the seed used to start pseudo random number generation.}
}
\references{
 Obenchain RL.  ICEplane: a windows application for incremental cost-effectiveness (ICE)
 statistical inference. Copyright (c) \bold{Pharmaceutical Research and Manufacturers of America}
 (PhRMA.)   http://www.math.iupui.edu/~indyasa/bobodown.htm   1997--2007.

 Obenchain RL.  Resampling and multiplicity in cost-effectiveness inference.  \emph{Journal of
 Biopharmaceutical Statistics} 1999; \bold{9(4)}: 563--582.
 
 Obenchain RL.  \bold{ICEinR.pdf}  ../R\_HOME/library/ICEinfer  2007; 30 pages.
 
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
}
\author{Bob Obenchain <softrx@iquest.net>}
\seealso{\code{\link{ICEscale}}, \code{\link{plot.ICEuncrt}} and \code{\link{print.ICEuncrt}}}
\examples{
  data(dulxparx)
  # Generating a bootstrap ICE uncertainty distribution is time consuming.
  dpunc <- ICEuncrt(dulxparx, dulx, idb, ru, lambda=0.26)
  dpunc
  plot(dpunc)
  # Transforming an existing bootstrap ICE uncertainty distribution is fast.
  dpuncX <- plot(dpunc, lfact=10)
}
\keyword{methods}
\keyword{nonparametric}
\keyword{robust}