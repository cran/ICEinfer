library(ICEinfer)

# input the sepsis data.frame...
data(sepsis)
str(sepsis)

# Create Non-Linear NMB y-Oucome variable with the default "eta" ratio = 3 + 2 * sqrt(2)
NLNMB <- ICEpref(sepsis$qalypres, sepsis$totcost, lambda=50000, beta=0.1)   
                                                           
summary(NLNMB) 

sepsis8 <- data.frame(cbind(NLNMB, sepsis))      # Augmented data.frame...

NLNMBsep1 <- subset(sepsis8, icu == 1, select = c(NLNMB, totcost, qalypres))
NLNMBsep0 <- subset(sepsis8, icu == 0, select = c(NLNMB, totcost, qalypres))
                      
par(mfrow=c(2,1))
hist(NLNMBsep1$NLNMB, breaks=35, xlim=c(-3,4), main="Patients in ICU_1", xlab = "NLNMB")
hist(NLNMBsep0$NLNMB, breaks=35, xlim=c(-3,4), main="Patients in ICU_0", xlab = "NLNMB")

par(mfrow=c(1,1))
plot(ICEomega(beta=.1))   
