\name{plot.ICEcolor}
\alias{plot.ICEcolor}
\title{Add Economic Preference Colors to Bootstrap Uncertainty Scatters within a Confidence Wedge}
\description{Assuming x is an object of class ICEcolor, the default invocation of plot(x)
 recolors the default alias display of the points within the bootstrap distribution of ICE
 uncertainty that are within its statistical confidence wedge.  An invocation of the form
 plot(x, alibi=TRUE) recolors the alibi display.  When ready, the user should click within
 this graphics window to display a Histogram of all the Economic Preference values falling
 within the ICE Statistical Confidence Wedge.  
}
\usage{
  \method{plot}{ICEcolor}(x, alibi = FALSE, \dots)
}
\arguments{
  \item{x}{Required; Output list object of class ICEcolor.}
  \item{alibi}{Optional; Logical value of TRUE or FALSE to control scaling of axes.  alibi = FALSE
   produces the default alias graphic in which points in the bootstrap uncertainty scatter are
   held fixed in space, and changes in lambda change the scaling (tick marks) along either the
   horizontal axis of a cost unit display or else along the vertical axis of an effe unit
   display.  alibi = TRUE produces an alibi graphic in which the scaling (and range) is the
   same along both axes, and changes in lambda cause the points in the bootstrap uncertainty
   scatter to move either left or right in a cost unit display or else up or dowm in an effe
   unit display.}
  \item{\dots}{Optional; Argument(s) passed on to plot().}
}
\details{
  To illustrate the sensitivity of Economic Preferences to choice of lambda, multiple calls are 
  usually made to ICEcolor() for different values of lambda as well as for different choices of
  the beta and gamma parameters that determine the shape of and spacing between the Indifference
  Curves of an ICE Preference Map.
  
  The plot() of an object of class ICEcolor displays the Bootstrap Distribution of ICE Uncertainty
  using small, circular, colored dots (pch = 20).  Outcomes outside the Confidence Wedge are
  displayed in black, while outcomes inside the Wedge are displayed in a rainbow of colors (within
  the red-tan-yellow-green range) that represent Economic Preferences.
  
  Upper and lower ICE Ray Limits are again displayed as solid black lines, while the Straight Line
  through the ICE origin that represents lambda is shown as a dashed black line.  In an Alias
  graphic, the slope of this dashed, black line will always be one; however, this dashed line
  usually does not appear to bisect the North-East and South-West ICE quadrants because DIFFERENT
  SCALINGS are being used along the horizontal and vertical axes. In an Alibi graphic where the
  scaling along both axes is the SAME, the slope of this dashed, black line will always be lambda;
  this dashed line will thus not bisect the North-East and South-West ICE quadrants unless lambda
  = 1.
}
\value{NULL}
\references{
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
 
 Obenchain RL.  Incremental Cost-Effectiveness (ICE) Preference Maps.  2001 \emph{JSM Proceedings
 (Biopharmaceutical Section) on CD-ROM}. (10 pages.)  Alexandria, VA: American Statistical
 Association.  2002.
 
 Obenchain RL.  ICE Preference Maps: Nonlinear Generalizations of Net Benefit and Acceptability.
 \emph{Health Serv Outcomes Res Method}  2008; \bold{8}: 31-56. DOI 10.1007/s10742-007-0027-2.
 Open Access.
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{ICEcolor}}, \code{\link{ICEscale}} and \code{\link{ICEwedge}}.}
\examples{
  data(dpwdg) 
  dpcol <- ICEcolor(dpwdg)
  plot(dpcol)
  plot(dpcol, alibi=TRUE)
}
\keyword{methods}
\keyword{hplot}