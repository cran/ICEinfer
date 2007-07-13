require(ICEinfer)
# input the fluoxtca data of Obenchain et al. (1997).
data(fluoxtca)
# Effectiveness = stable, Cost = cost, Trtm = fluox where
# fluox = 1 ==> fluoxetine and fluox = 0 ==> TCA or HCA
cat("\nDisplay of Lambda => Shadow Price Summary Statistics...\n")
ICEscale(fluoxtca, fluox, stable, cost)
ICEscale(fluoxtca, fluox, stable, cost, lambda=10000)

cat("\nBootstrap ICE Uncertainty calculations can be lengthy...\n")
ftunc <- ICEuncrt(fluoxtca, fluox, stable, cost, lambda=10000)
ftunc

cat("\nDisplay the Bootstrap ICE Uncertainty Distribution...\n")
plot(ftunc)

ftwdg <- ICEwedge(ftunc)
ftwdg
opar <- par(ask = dev.interactive(orNone = TRUE))
cat("\nClick within graphics window to display the Bootstrap 95% Confidence Wedge...\n")
plot(ftwdg)

cat("\nComputing VAGR Acceptability and ALICE Curves...\n")
ftacc <- ICEalice(ftwdg)
ftacc
plot(ftacc)

cat("\nColor Interior of Confidence Wedge with LINEAR Economic Preferences...\n")
ftcol <- ICEcolor(ftwdg, gamma=1)
ftcol
plot(ftcol)

cat("\nIncrease Lambda and Recolor Confidence Wedge with NON-Linear Preferences...\n")
ftcol <- ICEcolor(ftwdg, lfact=10)
ftcol
plot(ftcol)

cat("\nDecrease Lambda and Recolor Confidence Wedge with LINEAR Preferences...\n")
ftcol <- ICEcolor(ftwdg, lfact=10, gamma=1)
ftcol
plot(ftcol)
par(opar)