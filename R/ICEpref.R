"ICEpref" <- function (x, y, lambda = 1, beta = 1, eta = 3 + 2 * sqrt(2)) 
{
    if (lambda <= 0) 
        stop("The Lambda argument to ICEpref must be strictly positive.")
    if (beta <= 0) 
        stop("The Beta argument to ICEpref must be strictly positive.")
    if (eta <= 0) 
        stop("The Eta = Gamma/Beta ratio argument to ICEpref must be strictly positive.")
    if (eta > 3 + 2 * sqrt(2)) 
        cat("\nThese preferences violate the ICE Monotonicity Axiom.\n\n")
    gamma = eta * beta
    n <- length(x)
    pref <- rep(0, n)
    for (i in 1:n) {
      r <- sqrt(x[i]^2*lambda^2 + y[i]^2)
      gabs <- abs(x[i]*lambda - y[i])
      if (gabs > 0) gabs <- gabs^gamma
      if (r == 0) pref[i] <- 0
      else pref[i] <- r^(beta - gamma) * sign(x[i]*lambda-y[i]) * gabs
    }
    class(pref) <- "numeric"
    pref
}
