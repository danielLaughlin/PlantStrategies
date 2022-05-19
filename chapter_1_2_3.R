#############################
### Plant Strategies      ###
### Daniel C. Laughlin    ###
#############################

### Chapter 1 ###

#Set working directory
#setwd("~/OneDrive - University of Wyoming/Data/Book/github_repo")
library(MetBrewer)
library(sn)

### Figure 1.6

#tiff(file="Fig1.6.fitness.tif", res=300, width=2000, height=1500)
par(mfrow=c(2,3),bty="o",xaxt='n',yaxt='n', mar=c(2,2,4,1), cex.lab=1.3)
x <- seq(-2, 2, 0.01)
cols <- met.brewer(name="Moreau",n=7,type="discrete")

plot(x, rev(dsn(x, xi = 0.2, omega = 0.3, alpha = 5)), type = "l", xlim=c(-1,0),
     col=cols[5], lwd=2, xlab="",ylab="", axes=FALSE, frame.plot=TRUE, ylim=c(0.09,2.5))
polygon(c(-0.325,-0.325,-0.3,-0.3), c(0,2.7,2.7,0), col = "grey80", border=NA)
lines(x, rev(dsn(x, xi = 0.2, omega = 0.3, alpha = 5)),col=cols[5], lwd=2)
title(xlab="Environment", ylab="Fitness", line=1, main="(a)")
title(main="No competition", line=2.5, cex.main=1.7)
#abline(h=2.4, lty=2)
#arrows(-0.33, 0.7, -0.33, 0, length=0.1)

plot(x, rev(dsn(x, xi = 0.2, omega = 0.3, alpha = 5)), type = "l", xlim=c(-1,0),
     col=cols[5], lwd=2, xlab="",ylab="", axes=FALSE, frame.plot=TRUE, ylim=c(0.09,2.5))
polygon(c(-0.325,-0.325,-0.3,-0.3), c(0,2.7,2.7,0), col = "grey80", border=NA)
title(xlab="Environment", line=1, main="(c)")
title(main="Stable coexistence", line=2.5, cex.main=1.7)
lines(x-0.02, rev(dsn(x, xi = 0.2, omega = 0.25, alpha = 5))-0.48,
      col=cols[3], lwd=2)
#abline(h=2.4, lty=2)
#arrows(-0.33, 0.7, -0.33, 0, length=0.1)
legend(-1.05,2.4,c("Resident","Invader"), lwd=2.5, col=cols[c(5,3)], bty="n",cex=1.2)

plot(x-0.35, rev(dsn(x, xi = 0.2, omega = 0.3, alpha = 5)), type = "l", xlim=c(-1,0),
     col=cols[5], lwd=2, xlab="",ylab="", axes=FALSE, frame.plot=TRUE, ylim=c(0.09,2.5))
polygon(c(-0.325,-0.325,-0.3,-0.3), c(0,2.7,2.7,0), col = "grey80", border=NA)
title(xlab="Environment", line=1, main="(e)")
title(main="Competitive exclusion", line=2.5, cex.main=1.7)
lines(x-0.02, rev(dsn(x, xi = 0.2, omega = 0.25, alpha = 5))-0.48,
      col=cols[3], lwd=2)
#abline(h=2.4, lty=2)
#arrows(-0.33, 0.7, -0.33, 0, length=0.1)

### alpha function, G-function, and K function
aij = function(v,u,siga=2,B=2){
  aij <- 1 + exp(-((v-u+B)^2)/(2*siga^2)) - exp(-(B^2)/(2*siga^2))
  return(aij)
}
K_v = function(Km=100, v, sigk=sqrt(12.5)) {
  K <- Km * exp(-(v^2)/(2*sigk^2))
  return(K)
}
G_f = function(v, r=0.25, x, u){
  G <- (r/K_v(v=v)) * (K_v(v=v) - aij(v=v,u=u[1])*x[1] - aij(v=v,u=u[2])*x[2])
  return(G)
}
v <- seq(-5,5,length.out=200)

plot(v, G_f(v=v, x=c(51,38), u=c(2.6,1.6)), type="l", lwd=2, main="(b)", ylim=c(-0.06,0.035))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
lines(v, G_f(v=v, x=c(51,38), u=c(2.6,1.6)), lwd=2)
title(xlab="Trait", ylab="Fitness",line=1)
#abline(h=0.013, lty=2)
points(3.8 ,0.013, pch=19, cex=3.5, col=cols[5])

plot(v, G_f(v=v, x=c(51.34,38.91), u=c(3.12,-0.24)),
     ylab="", type="l", lwd=2, main="(d)", ylim=c(-0.01,0.003), xlim=c(-1.5,4.2))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
lines(v, G_f(v=v, x=c(51.34,38.91), u=c(3.12,-0.24)),lwd=2)
#abline(h=0, lty=2)
title(xlab="Trait", line=1)
points(c(3.12,-0.24),c(0,0), pch=19, cex=3.5, col=cols[c(5,3)])
legend("top",c("Resident","Invader"), pch=19, col=cols[c(5,3)], bty="n",cex=1.2)

plot(v, G_f(v=v, x=c(51,38), u=c(2.6,1.6)), type="l", lwd=2, main="(f)", ylim=c(-0.06,0.065))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
lines(v, G_f(v=v, x=c(51,38), u=c(2.6,1.6)), lwd=2)
#abline(h=0.036, lty=2)
title(xlab="Trait", line=1) 
points(-3.5, 0.035, pch=19, cex=3.5, col=cols[3])
#dev.off()
#### End of figure


### Chapter 2 ###

library("Ternary")
library("viridis")

### Fig 2.11b
#tiff(file="Fig2.11b.grime.tif", res=300, width=1200, height=1200)
duration <- rnorm(1000,0,1)
product <- rnorm(1000,0,1)
par(mar=c(2,2,1,1))
plot(duration, product, xlim=c(-3,3), ylim=c(-3,3), pch=19, cex=0.8, col="white",
     xlab="",ylab="",xaxt='n', yaxt='n')
polygon(c(-3.3,-3.3,3.3),c(-3.3,3.3,-3.3), col="grey90")
title(ylab="Habitat productivity", line=0.5, cex.lab=1.2)
title(xlab="Habitat duration", line=0.5, cex.lab=1.2)
abline(0, -1)
text(-1.8,2.7,"R", cex=1.5)
text(2.7,2.7,"C", cex=1.5)
text(2.7,-1.8,"S", cex=1.5)
text(1,1,"Tenable", cex=1.3, font=3)
text(-1,-1,"Untenable", cex=1.3, font=3)
#dev.off()

### Fig 2.11c

#tiff(file="Fig2.11c.grime.tif", res=300, width=1200, height=1200)
par(mar=c(2,2,1,1))
TernaryPlot(lab.offset=0.15,alab="Importance of competition (Ic) %",
            blab="Importance of stress (Is) %", clab="Importance of disturbance (Id) %",
            grid.lines = 4, lab.cex=0.7, axis.cex=0.5,
            grid.minor.lines = 0)
TernaryText(c(75,12.5,12.5),"C", cex=0.8, font=2)
TernaryText(c(12.5,75,12.5),"S", cex=0.8, font=2)
TernaryText(c(12.5,12.5,75),"R", cex=0.8, font=2)
TernaryText(c(45,10,45),"C-R", cex=0.8, font=2)
TernaryText(c(45,45,10),"C-S", cex=0.8, font=2)
TernaryText(c(12.5,42.5,42.5),"S-R", cex=0.8, font=2)
TernaryText(c(33,33,33),"C-S-R", cex=0.8, font=2)
#dev.off()

### Fig 2.11d
#tiff(file="Fig2.11d.grime.tif", res=300, width=1200, height=1200)
par(mar=c(2,2,1,1))
duration <- rnorm(1000,0,1)
product <- rnorm(1000,0,1)
plot(duration, product, xlim=c(-3,3), ylim=c(-3,3), pch=19, cex=0.8, col="grey70",
     xlab="",ylab="",xaxt='n', yaxt='n')
polygon(c(-3.3,-3.3,3.3),c(-3.3,3.3,-3.3), col="grey90")
points(duration, product, pch=19, cex=0.8, col="grey70")
title(ylab="Habitat productivity", line=0.5, cex.lab=1.2)
title(xlab="Habitat duration", line=0.5, cex.lab=1.2)
abline(0, -1)
text(-1.8,2.7,"R", cex=1.5)
text(2.7,2.7,"C", cex=1.5)
text(2.7,-1.8,"S", cex=1.5)
text(1,1,"Tenable", cex=1.3, font=3)
text(-1,-1,"Untenable", cex=1.3, font=3)
text(0,-1.5,"A", cex=1.2)
points(0,-1.8,pch=19)
text(-1.5,0,"B", cex=1.2)
points(-1.5,-0.3,pch=19)
#dev.off()



### Fig 2.15 LHS model
library(plot3D)

### read in TRY data from Carmona et al. 2021 Nature
aboveTraits <- read.table("Above_traits.txt")
aboveTraitsCompleteRows <- complete.cases(aboveTraits)
aboveTraitsComplete <- aboveTraits[aboveTraitsCompleteRows, ] # Species with complete information aboveground (2630)

#tiff(file="Fig2.15.LHS.tif", res=300, width=1500, height=1500)
par(mar=c(2,2,2,2))
scatter3D(log(aboveTraitsComplete$sla),  log(aboveTraitsComplete$sm), log(aboveTraitsComplete$ph), colvar=aboveTraitsComplete$ssd,
          pch=19, cex=0.4, phi=18, theta=40, clab=c("Stem tissue","density"), main="Leaf-Height-Seed model",col=viridis_pal(begin=0, end=1,direction=-1)(100),
          xlab="Specific leaf area", ylab="Seed mass", zlab="Maximum height", colkey=list(length=0.5,cex.clab=0.6,width=0.7))
scatter3D( log(aboveTraitsComplete$sla),  log(aboveTraitsComplete$sm), rep(min(log(aboveTraitsComplete$ph)),length(aboveTraitsComplete$ph)),
           col="grey70" , pch=19, cex=0.1, add=TRUE)
scatter3D( log(aboveTraitsComplete$sla),  rep(max(log(aboveTraitsComplete$sm)),length(aboveTraitsComplete$sm)), log(aboveTraitsComplete$ph),
           col="grey70" , pch=19, cex=0.1, add=TRUE)
scatter3D( rep(min(log(aboveTraitsComplete$sla)),length(aboveTraitsComplete$sla)),  log(aboveTraitsComplete$sm), log(aboveTraitsComplete$ph),
           col="grey70" , pch=19, cex=0.1, add=TRUE)
#scatter3D( log(aboveTraitsComplete$sla),  log(aboveTraitsComplete$sm), log(aboveTraitsComplete$ph),
#           col=viridis_pal(begin=0, end=1,direction=-1)(100) , pch=19, cex=0.4, add=TRUE, colkey=list(plot=FALSE))
#dev.off()     



### Chapter 3 ###
library(popbio)
library(popdemo)
library(Rage)

### Fig 3.2 Butterfly plant population dynamics

### load data
counts.stream <- read.csv("~/OneDrive - University of Wyoming/Data/Book/Rscripts/counts.stream.csv", header=TRUE)[-c(1:2),]
counts.stream$total <- counts.stream$crow.total + counts.stream$diamond.total + counts.stream$unnamed.total
obs <- length(counts.stream$total)

### plot count data over time
#tiff(file="Fig3.2.COBP.tif", res=300, width=1500, height=1500)
par(mfrow=c(2,2), mar=c(3,3,1,1))
plot(crow.total~year, data=counts.stream, xlab="", ylab="", pch=19,
     main="(a) Crow creek", cex.lab=0.8, cex.main=0.8, yaxt="n", xaxt="n")
axis(2,cex.axis=0.7)
axis(1,cex.axis=0.7)
title(ylab="Flowering plant count (N)", line=2, cex.lab=0.8)
lines(crow.total~year, data=counts.stream)
plot(diamond.total~year, data=counts.stream, xlab="", ylab="", pch=19,
     main="(c) Diamond creek", cex.lab=0.8, cex.main=0.8, yaxt="n", xaxt="n")
axis(2,cex.axis=0.7)
axis(1,cex.axis=0.7)
lines(diamond.total~year, data=counts.stream)

### calculate logLambdas
counts.stream$crowlogLam <- c(diff(log(counts.stream$crow.total)), NA)
counts.stream$diamondlogLam <- c(diff(log(counts.stream$diamond.total)), NA)

### plot logLambdas
plot(crowlogLam~year, data=counts.stream, xlab="", ylab="", pch=19, ylim=c(-2.5,2),
     main="(b) Crow creek", cex.lab=0.8, cex.main=0.8, yaxt="n", xaxt="n")
axis(2,cex.axis=0.7)
axis(1,cex.axis=0.7)
title(ylab="log Lambda", line=2, cex.lab=0.8)
title(xlab="Year", line=2, cex.lab=0.8)
lines(crowlogLam~year, data=counts.stream)
abline(h=mean(counts.stream$crowlogLam, na.rm=TRUE),lty=2)
plot(diamondlogLam~year, data=counts.stream, xlab="", ylab="", pch=19, ylim=c(-2.5,2),
     main="(d) Diamond creek", cex.lab=0.8, cex.main=0.8, yaxt="n", xaxt="n")
axis(2,cex.axis=0.7)
axis(1,cex.axis=0.7)
title(xlab="Year", line=2, cex.lab=0.8)
lines(diamondlogLam~year, data=counts.stream)
abline(h=mean(counts.stream$diamondlogLam, na.rm=TRUE),lty=2)
#dev.off()


### Fig 3.7 Disturbance Dimensionality
disturb <- read.csv("disturbances.csv", header=TRUE)
disturb <- disturb[-c(1,22:27),] # remove human and volcano types

### Figure 3.7
#tiff(file="Fig3.7.FreqSev.tif", res=300, width=2400, height=1200)
par(mfrow=c(1,2), mar=c(4,4,1.5,0.5))
plot(disturb$Frequency, disturb$Severity, ylim=c(0.5, 5.5), xlim=c(0.5,5.5), col="white",
     xlab="Disturbance frequency", ylab="Disturbance severity", main="(a)")
text(disturb$Freq.Jitter, disturb$Severity.Jitter, disturb$Disturbance.Type, cex=0.7)
#text(jitter(disturb$Frequency,3), jitter(disturb$Severity,3), disturb$Disturbance.Type, cex=0.6)
row.names(disturb) <- disturb[,2]
#pca <- prcomp(na.omit(disturb[,c(6:8)]), scale.=TRUE)
pca <- princomp(na.omit(disturb[,c(6:8)]), cor=TRUE, scores=TRUE)
plot(pca$scores[,1], pca$scores[,2], col="white",xlab="PC1 (58%)", ylab="PC2 (35%)", xlim=c(-3,2),
     ylim=c(-1.6,1.6), main="(b)")
pca$scores[1,2] <- 0.8 #a'a
pca$scores[2,2] <- 1.1 #lahar
pca$scores[9,2] <- -0.8 #rock outcrop
pca$scores[12,2] <- -1.3 #tornado
pca$scores[8,2] <- -1.3 #uprooting
pca$scores[6,2] <- -1.2 #earthquake
text(pca$scores[,1], pca$scores[,2], disturb$Disturbance.Type, cex=0.7)
arrows(0,0,1.3,0.2, col="blue"); text(1.4,0.4, "Severity", col="blue")
arrows(0,0,0.2,1.2, col="blue"); text(0.2,1.4, "Extent", col="blue")
arrows(0,0,-2,0.5, col="blue"); text(-2.5,0.7, "Frequency", col="blue")
#dev.off()



