\name{ICEinfer-package}
\alias{ICEinfer-package}
\docType{package}
\title{
 Package for Incremental Cost-Effectiveness (ICE) Statistical Inference and
 quantification of ICE Economic Preference variation.
}
\description{
ICE Statistical Inference makes head-to-head comparisons between two treatments
in two dimensions, cost and effectiveness.  Bootstrap resampling from unbiased,
patient level data quantifies the Distribution of ICE Uncertainty and defines
Wedge-Shaped Statistical Confidence Regions equivariant relative to choice for the
numerical Shadow Price of Health, lambda.  Preference maps with (linear or
nonlinear) indiference curves then illustrate that considerable additional
Economic Preference Uncertainty can result from deliberately varying lambda.
}
\details{
\tabular{ll}{
Package: \tab ICEinfer\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2007-07-13\cr
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
\author{
Bob Obenchain
Maintainer: Bob Obenchain <softrx@iquest.net>
}
\references{
 Black WC.  The CE plane: a graphic representation of cost-effectiveness.  \emph{Med
 Decis Making} 1990; \bold{10}: 212--214.

 Laupacis A, Feeny D, Detsky AS, Tugwell PX.  How attractive does a new technology have
 to be to warrant adoption and utilization?  Tentative guidelines for using clinical
 and economic evaluations.  \emph{Can Med Assoc J}  1992; \bold{146(4)}: 473--81.

 Stinnett AA, Mullahy J.  Net health benefits: a new framework for the analysis of
 uncertainty in cost-effectiveness analysis.  \emph{Medical Decision Making}, Special
 Issue on Pharmacoeconomics  1998; \bold{18}: S68--S80.

 O'Brien B, Gersten K, Willan A, Faulkner L.  Is there a kink in consumers' threshold
 value for cost-effectiveness in health care?  \emph{Health Econ}  2002; \bold{11}:
 175--180.

 Obenchain RL.  ICE Preference Maps:  Nonlinear Generalizations of Net Benefit and
 Acceptability.  \emph{Eli Lilly White Paper}.  2007.
 
 Obenchain RL.  \bold{ICEinR.pdf}  ../R\_HOME/library/ICEinfer  2007; 30 pages.
}
\keyword{ package }
\examples{
  data(dulxparx)
  ICEscale(dulxparx, dulx, idb, ru)
  ICEscale(dulxparx, dulx, idb, ru, lambda=0.26)
  dpunc <- ICEuncrt(dulxparx, dulx, idb, ru, lambda=0.26)
  dpwdg <- ICEwedge(dpunc)
  plot(dpwdg)
  dpacc <- ICEalice(dpwdg)
  plot(dpacc)
  dpcol <- ICEcolor(dpwdg)
  dpcol
  plot(dpcol)
  dpcolX <- ICEcolor(dpwdg, lfact=10)
  dpcolX
  plot(dpcolX, alibi=TRUE)
}