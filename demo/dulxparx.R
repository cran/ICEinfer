require(ICEinfer)
# input the dulxparx data of Obenchain et al. (2000).
data(dulxparx)
# Effectiveness = idb, Cost = ru, trtm = dulx where
# dulx = 1 ==> Duloxetine and dulx = 0 ==> Paroxetine

cat("\n Display of Lambda => Shadow Price Summary Statistics...\n")
ICEscale(dulxparx, dulx, idb, ru)
ICEscale(dulxparx, dulx, idb, ru, lambda=0.26)

cat("\n Bootstrap ICE Uncertainty calculations can be lengthy...\n")
dpunc <- ICEuncrt(dulxparx, dulx, idb, ru, lambda=0.26)
dpunc

cat("\n Display the Bootstrap ICE Uncertainty Distribution...")
plot(dpunc)

dpwdg <- ICEwedge(dpunc)
dpwdg
opar <- par(ask = dev.interactive(orNone = TRUE))
cat("\n Click within graphics window to display the Bootstrap 95% Confidence Wedge...\n")
plot(dpwdg)

cat("\n Computing VAGR Acceptability and ALICE Curves...\n")
dpacc <- ICEalice(dpwdg)
dpacc
plot(dpacc)

cat("\n Color Interior of Confidence Wedge with LINEAR Economic Preferences...\n")
dpcol <- ICEcolor(dpwdg, gamma=1)
dpcol
plot(dpcol)

cat("\n Increase Lambda and Recolor Confidence Wedge with NON-Linear Preferences...\n")
dpcol <- ICEcolor(dpwdg, lfact=10)
dpcol
plot(dpcol)

cat("\n Decrease Lambda and Recolor Confidence Wedge with LINEAR Preferences...\n")
dpcol <- ICEcolor(dpwdg, lfact=10, gamma=1)
dpcol
plot(dpcol)
par(opar)