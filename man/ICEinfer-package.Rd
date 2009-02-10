\name{ICEinfer-package}
\alias{ICEinfer-package}
\docType{package}
\title{ICE Statistical Inference and Economic Preference Variation}
\description{
  Functions in the ICE Statistical Inference package make head-to-head comparisons
  between patients in two treatment cohorts (assumed to be unbiased samples) in two
  distinct dimensions, cost and effectiveness.
  
  Bootstrap resampling methods quantify the endogenous Distribution of ICE Uncertainty
  and define Wedge-Shaped Statistical Confidence Regions equivariant relative to
  exogenous choice for the numerical Shadow Price of Health, lambda.
  
  Preference maps with (linear or nonlinear) indiference curves can be viewed or
  superimposed upon endogenous confidence wedges to illustrate that considerable
  additional, potentially self-contradictory Economic Preference Uncertainty
  results from deliberately varying lambda.
}
\details{
  \tabular{ll}{
  Package: \tab ICEinfer\cr
  Type: \tab Package\cr
  Version: \tab 0.2\cr
  Date: \tab 2009-02-10\cr
  License: \tab GNU GENERAL PUBLIC LICENSE, Version 2, June 1991\cr
  }
Statistical inference using functions from the ICEinfer package usually start
with (possibly multiple) invocations of ICEscale() to help determine a reasonable
value for the Shadow Price of Health, lambda.  This is invariably followed by a
single call to ICEuncrt to generate the Bootstrap Distribution of ICE Uncertainty
corresponding to the chosen value of lambda.  However, the print() and plot()
functions for objects of type ICEuncrt do have optional arguments, lfact and swu,
to help the user quantify and visualize the consequences of changing lambda and
switching between cost and effe units.

Next, a single call to ICEwedge() yields the equivariant, wedge-shaped region
of specified statistical confidence within [.50, .99] ...by computing ICE Angle
Order Statistics around a circle centered at the ICE Origin, (DeltaEffe, DeltaCost)
= (0, 0).

Researchers wishing to view alternative ICE Acceptability Curves would then envoke
ICEalice().

Finally, multiple calls to ICEcolor for different values of lambda and/or different
forms of (linear or nonlinear) ICE Preference Maps are typically used to illustrate
the considerable additional Economic Preference Uncertainty that can be introduced.
This Economic Uncertainty is superimposed on top of the inherent Statistical
Uncertainty contained in unbiased, patient level data on the relative cost and
effectiveness of two treatments for the same disease / condition.
}
\author{Bob Obenchain <wizbob@att.net>}
\references{
 Black WC.  The CE plane: a graphic representation of cost-effectiveness.  \emph{Med
 Decis Making} 1990; \bold{10}: 212-214.

 Laupacis A, Feeny D, Detsky AS, Tugwell PX.  How attractive does a new technology have
 to be to warrant adoption and utilization?  Tentative guidelines for using clinical
 and economic evaluations.  \emph{Can Med Assoc J}  1992; \bold{146(4)}: 473-81.

 Stinnett AA, Mullahy J.  Net health benefits: a new framework for the analysis of
 uncertainty in cost-effectiveness analysis.  \emph{Medical Decision Making}, Special
 Issue on Pharmacoeconomics  1998; \bold{18}: S68-S80.

 O'Brien B, Gersten K, Willan A, Faulkner L.  Is there a kink in consumers' threshold
 value for cost-effectiveness in health care?  \emph{Health Econ}  2002; \bold{11}:
 175-180.

 Obenchain RL.  ICE Preference Maps:  Nonlinear Generalizations of Net Benefit and
 Acceptability. \emph{Health Serv Outcomes Res Method}  2008; \bold{8}: 31-56. DOI 
 10.1007/s10742-007-0027-2. Open Access.
 
 Obenchain RL.  \bold{ICEinR.pdf}  ../R\_HOME/library/ICEinfer  2009; 30 pages.
}
\keyword{package}
\examples{
  demo(dulxparx)
}