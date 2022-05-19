#############################
### Plant Strategies      ###
### Daniel C. Laughlin    ###
#############################

### Chapter 5 ###

setwd("~/OneDrive - University of Wyoming/Data/Book/github_repo")

# selection on lx and mx
#devtools::install_github("BlakeRMills/MetBrewer")
library(MetBrewer)
cols <- met.brewer(name="Moreau",n=7,type="discrete")

### Fig 5.1 Plot lx and mx
#tiff(file="Fig5.1.lxmx.tif", res=300, width=1900, height=2300)

par(mfcol=c(3,2), mar = c(3, 3, 3, 4))

L <- c(1,0.3,0.1,0.05,0.03,0.01,0.005)
m <- c(0,6,7,6,3,0,0) # earlier reproduction
x <- c(0:6)
Lm <- L*m
sum(Lm)
euler <- function(r) sum(L * m * exp(-r * x)) - 1
range=c(-100,100)
uniroot(f = euler, interval = range, tol = 1e-8, extendInt="yes")$root

plot(x,L, type ="l", ylab = "", cex.main=1,
     main = "(b) Ro = 2.89, r = 0.78", xlab = "",
     col=cols[5], lwd=3, lty=2, ylim=c(0,2.1))
lines(x,Lm, col=1, lty = 1, lwd=3)
par(new = TRUE)
plot(m, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col=cols[3], lty = 3, lwd=3)
axis(side = 4)
mtext("Age", side = 1, line = 2, cex=0.7)
mtext("L(x) and L(x)m(x)", side = 2, line = 2, cex=0.7)
mtext("m(x)", side = 4, line = 2, cex=0.7)
legend("topright", c("L(x)", "m(x)", "L(x)m(x)"), cex=1.2,
       col = c(cols[c(5,3)],1), lty = c(2, 3, 1), lwd=3, bty="n")

L <- c(1,0.3,0.1,0.05,0.03,0.01,0.005)
m <- c(0,0,6,7,6,3,0)
x <- c(0:6)
Lm <- L*m
sum(Lm)
uniroot(f = euler, interval = range, tol = 1e-8, extendInt="yes")$root

plot(x,L, type ="l", ylab = "",cex.main=1,
     main = "(a) Life table in Table 4.1, Ro = 1.2, r = 0.06", xlab = "",
     col=cols[5], lwd=3, lty=2, ylim=c(0,2.1))
lines(x, Lm, col=1, lty = 1, lwd=3)
par(new = TRUE)
plot(m, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col=cols[3], lty = 3, lwd=3)
axis(side = 4)
mtext("Age", side = 1, line = 2, cex=0.7)
mtext("L(x) and L(x)m(x)", side = 2, line = 2, cex=0.7)
mtext("m(x)", side = 4, line = 2, cex=0.7)

L <- c(1,0.3,0.1,0.05,0.03,0.01,0.005)
m <- c(0,0,0,6,7,6,3) # later reproduction
x <- c(0:6)
Lm <- L*m
sum(Lm)
uniroot(f = euler, interval = range, tol = 1e-8, extendInt="yes")$root

plot(x,L, type ="l", ylab = "",cex.main=1,
     main = "(c) Ro = 0.59, r = -0.15", xlab = "",
     col=cols[5], lwd=3, lty=2, ylim=c(0,2.1))
lines(x,Lm, col=1, lty = 1, lwd=3)
par(new = TRUE)
plot(m, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col=cols[3], lty = 3, lwd=3)
axis(side = 4)
mtext("Age", side = 1, line = 2, cex=0.7)
mtext("L(x) and L(x)m(x)", side = 2, line = 2, cex=0.7)
mtext("m(x)", side = 4, line = 2, cex=0.7)

L <- c(1,0.1,0.075,0.05,0.03,0.01,0.005) #steeper lx curve
m <- c(0,0,6,7,6,3,0)
x <- c(0:6)
Lm <- L*m
sum(Lm)
uniroot(f = euler, interval = range, tol = 1e-8, extendInt="yes")$root

plot(x,L, type ="l", ylab = "",cex.main=1,
     main = "(d) Ro = 1.01, r = 0.004", xlab = "",
     col=cols[5], lwd=3, lty=2,ylim=c(0,2.1))
lines(x,Lm, col=1, lty = 1, lwd=3)
par(new = TRUE)
plot(m, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col=cols[3], lty = 3, lwd=3)
axis(side = 4)
mtext("Age", side = 1, line = 2, cex=0.7)
mtext("L(x) and L(x)m(x)", side = 2, line = 2, cex=0.7)
mtext("m(x)", side = 4, line = 2, cex=0.7)

L <- c(1,0.3,0.1,0.05,0.03,0.01,0.005)
m <- c(0,0,6,7,6,3,0)
x <- c(0:6)
Lm <- L*m
sum(Lm)
uniroot(f = euler, interval = range, tol = 1e-8, extendInt="yes")$root

plot(x,L, type ="l", ylab = "",cex.main=1,
     main = "(b) Life table in Table 4.1, Ro = 1.2, r = 0.06", xlab = "",
     col=cols[5], lwd=3, lty=2,ylim=c(0,2.1))
lines(x,Lm, col=1, lty = 1, lwd=3)
par(new = TRUE)
plot(m, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col=cols[3], lty = 3, lwd=3)
axis(side = 4)
mtext("Age", side = 1, line = 2, cex=0.7)
mtext("L(x) and L(x)m(x)", side = 2, line = 2, cex=0.7)
mtext("m(x)", side = 4, line = 2, cex=0.7)

L <- c(1,0.7,0.35,0.2,0.1,0.05,0.01) # shallow lx curve
m <- c(0,0,6,7,6,3,0)
x <- c(0:6)
Lm <- L*m
sum(Lm)
uniroot(f = euler, interval = range, tol = 1e-8, extendInt="yes")$root

plot(x,L, type ="l", ylab = "",cex.main=1,
     main = "(e) Ro = 4.25, r = 0.56", xlab = "",
     col=cols[5], lwd=3, lty=2,ylim=c(0,2.1))
lines(x,Lm, col=1, lty = 1, lwd=3)
par(new = TRUE)
plot(m, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col=cols[3], lty = 3, lwd=3)
axis(side = 4)
mtext("Age", side = 1, line = 2, cex=0.7)
mtext("L(x) and L(x)m(x)", side = 2, line = 2, cex=0.7)
mtext("m(x)", side = 4, line = 2, cex=0.7)

#dev.off()

##### End of Figure 5.1



### Fig 5.5 Loehle 1988 appendix data
library(MetBrewer)
cols <- met.brewer(name="Moreau",n=7,type="discrete")
age <- read.csv("loehle1988.csv", header=TRUE)
attach(age)

#tiff("Fig5.5.AgeMaturity.tif", res=300, width=2000, height=1700)
plot(typmort[phylo=="gymnosperm"], averep[phylo=="gymnosperm"], col=cols[5], pch=19,log="xy",cex=1.3,
     xlab="Average longevity (yrs)", ylab="Average age to reproduction (yrs)", cex.lab=1.2)
points(typmort[phylo=="angiosperm"], averep[phylo=="angiosperm"], col=cols[3], pch=19, cex=1.3)
points(typmort[phylo=="gymnosperm"], averep[phylo=="gymnosperm"], col=cols[5], pch=19, cex=1.3)
legend("topleft", c("Gymnosperms","Angiosperms"), col=cols[c(5,3)], pch=19, cex=1.2, bty="n")
#dev.off()
### End Loehle 1988 appendix data


### Chapter 6 ###

#### Ch 6. Trait spectrums
library(viridis)

### Load data ###########################################################
dat <- read.csv("nzDrought.NewPhyt.data.csv", header=TRUE)

### Fig 6.14
#tiff(file="Fig6.14nzP50.tif", res=300, width=1600, height=1200)
par(mar=c(3,3,1,1))
con <- dat[dat$taxa=="conifer",]
dat[47,10] <- "PSECOLE"
dat[43,10] <- "PODLAE"
rownames(dat) <- dat$spcode
spp <- dat[c("AGAAUS","DACDAC","DACCUP","PRUFER","ELADEN","KNIEXC","PHYALP","FUSSOL",
             "LIBBID","WEIRAC","PODLAE","FUCEXC","HOHGLA","FUSFUS","QUISER","GRILIT",
             "PSECOL","LOPMEN","PITEUG","CARSER","SCHDIG","PENCOR"),]
spp <- spp[order(spp$P50),]
mpa <- seq(-12,0,0.01)
cols <- viridis_pal(option="D")(nrow(spp))
plot(mpa, 100/(1+exp((spp$slope[1]/25)*(mpa-spp$P50[1]))), col="white", ylim=c(0,100),
     ylab="Percent loss of conductance (%)", xlab="Xylem pressure (MPa)", xlim=c(-12.2,-0.2))
abline(50,0, lty=3, col="grey50")
for (i in 1:nrow(spp)){
  lines(mpa,100/(1+exp((spp$slope[i]/25)*(mpa-spp$P50[i]))), lwd=2.5, col= cols[i])
}
legend("bottomleft", legend=spp$species, col=cols, lwd=2.5, cex=0.485, bty="n", text.font=3)
title(xlab="Xylem pressure (MPa)", line=2)
title(ylab="Percent loss of conductance", line=2)
#dev.off()
### End pressure curves





## Fig 6.30-Phylogenetics
library(adephylo)
library(nlme)
library(ape)
library(phytools)
library(phylobase)
#tree <- phylomatic(species, get="POST", storedtree="zanne2014") #deprecated!

newick <- "((Bouteloua_gracilis:39.751062,(Koeleria_macrantha:23.849927,Oryzopsis_hymenoides:23.849925):15.901136):285.29895,((Pseudotsuga_menziesii:38.797572,Pinus_ponderosa:38.797571):2.897999,Abies_lasiocarpa:41.695571):283.35445)Spermatophyta;"
tree <- read.tree(text=newick) # tree taken from Brown
species <- c("Bouteloua gracilis","Koeleria macrantha","Oryzopsis hymenoides","Pinus ponderosa","Pseudotsuga menziesii","Abies lasiocarpa")
tree$tip.label <- Hmisc::capitalize(gsub("_"," ",tree$tip.label))
plot(tree)
sla <- c(12,15,14,5,9,7)
height <- c(0.3,0.5,1,40,45,30)
traits <- cbind(sla,height)
row.names(traits) <- species
p.dist.mat <- cophenetic(tree) # trick to ensure correct order
traits <- traits[row.names(p.dist.mat),] # trick to ensure correct order
traits <- as.data.frame(traits)
phylosig(tree, traits[,1], method="lambda", test=TRUE, nsim=999)
phylosig(tree, traits[,2], method="lambda", test=TRUE, nsim=999)
tree.traits <- phylo4d(tree, traits)
table.phylo4d(tree.traits,show.node.label=FALSE,cex.label=0.7,ratio.tree=0.5,cex.symbol=0.7,box=FALSE,
              symbol="circles",var.label=c("SLA","Height"))

# raw correlation
cor.test(sla,height)

# PIC calculation
pic.X <- pic(sla, tree)
pic.Y <- pic(height, tree)
cor.test(pic.X, pic.Y)

#PGLS calculation
fit.yx <- nlme::gls(sla~height, data=as.data.frame(traits[,1:2]), correlation=corBrownian(1,tree))
anova(fit.yx)

phylomorphospace(tree,traits[,c("sla","height")],node.size=c(0,1),xlim=c(2,20), ylim=c(-5,70))

### Fig 6.30 Phylogeny
#tiff(file="Fig6.30phylo.tif", res=300, width=3100, height=1000)
par(mfrow=c(1,3),mar=c(4,4,2,2))
plot.phylo(tree, cex=1)
table.phylo4d(tree.traits,show.node.label=FALSE,cex.label=0.7,ratio.tree=0.5,cex.symbol=0.5,box=FALSE,
              symbol="circles",var.label=c("SLA","Height"))
phylomorphospace(tree,traits[,c("sla","height")],node.size=c(0,1),xlim=c(-1,22), ylim=c(-27,70),
                 xlab="SLA",ylab="Height")
#dev.off()
### End Fig 6.30 Phylogeny



### Chapter 7 ###

## Figure 7.1
library(NLMR)
library(landscapetools)
landscape <- nlm_fbm(100, 100, fract_dim = 0.7)
show_landscape(landscape)

## Rest of chapter 7 figures
library(mclust)
library(MASS)
library(viridis)

t <- seq(-3,3,0.1)
f1 <- dnorm(t, mean=-1.5, sd=1)
f2 <- dnorm(t, mean=1.5, sd=1)
f3 <- 0.6*dnorm(t, mean=-1.5, sd=1) + 0.4*dnorm(t, mean=2, sd=1)
f4 <- 0.6*dnorm(t, mean=1.5, sd=1) + 0.4*dnorm(t, mean=-1.5, sd=1)

###Generate site data
N=100
Sigma <- matrix(c(2,-1,-1,2),2,2)
site1 <- mvrnorm(n = N, mu=c(-1.5,1.5), Sigma, tol = 1e-6)
site2a <- mvrnorm(n = N, mu=c(-1.5, 2), Sigma, tol = 1e-6)
site2b <- mvrnorm(n = N, mu=c(1.5, -1.5), Sigma, tol = 1e-6)
site2 <- rbind(site2a,site2b)

## Plot
#tiff("Fig7.2.Landscapes.tiff", res=300, width=1600, height=2000)
par(mfcol=c(3,2), mar=c(3,3,2,1))
plot(f1~t, type="l", xlab="", ylab="", cex.lab=1.5, col="darkblue", lwd=2)
text(2.9,0.36,"(a)")
title(ylab="Fitness", line=2, cex.lab=1.2)
title(xlab="Trait 1", line=2, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", cex.lab=1.5, col="darkblue", lwd=2)
text(2.9,0.36,"(b)")
title(ylab="Fitness", line=2, cex.lab=1.2)
title(xlab="Trait 2", line=2, cex.lab=1.2)
plot(densityMclust(site1, plot=FALSE),what="density",type="image",nlevels=10, xlim=c(-3,3),
     ylim=c(-3,3), col=viridis_pal()(10), lwd=2, xlab="", ylab="", cex.lab=1.5)
text(2.7,2.5,"(c)")
title(ylab="Trait 2", line=2, cex.lab=1.2)
title(xlab="Trait 1", line=2, cex.lab=1.2)
plot(f3~t, type="l", xlab="", ylab="", cex.lab=1.5, col="darkblue", lwd=2)
text(2.9,0.23,"(d)")
title(ylab="Fitness", line=2, cex.lab=1.2)
title(xlab="Trait 1", line=2, cex.lab=1.2)
plot(f4~t, type="l", xlab="", ylab="", cex.lab=1.5, col="darkblue", lwd=2)
text(2.9,0.23,"(e)")
title(ylab="Fitness", line=2, cex.lab=1.2)
title(xlab="Trait 2", line=2, cex.lab=1.2)
plot(densityMclust(site2, plot=FALSE),what="density",type="image",nlevels=10, xlim=c(-3,3),
     ylim=c(-3,3), col=viridis_pal()(10), lwd=2, xlab="", ylab="", cex.lab=1.5, main="(f)", font.main=1)
text(2.7,2.5,"(f)")
title(ylab="Trait 2", line=2, cex.lab=1.2)
title(xlab="Trait 1", line=2, cex.lab=1.2)
#dev.off()



## Fig five factors fitness landscapes
#tiff("Fig7.3.TraitLandscapes.tiff", res=300, width=2100, height=1900)
par(mfrow=c(4,5), mar=c(3,2,2,1), xaxt='n', yaxt='n')
#Productive
t <- seq(-3,3,0.1)
f1 <- dnorm(t, mean=2.5, sd=2)
f2 <- dnorm(t, mean=-2.5, sd=2)
f3 <- dnorm(t, mean=-2.5, sd=2)
#f4 <- 0.5*dnorm(t, mean=-2.5, sd=2.25) + 0.5*dnorm(t, mean=2.5, sd=2.25)
f4 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f5 <- dnorm(t, mean=2.5, sd=2)
plot(f1~t, type="l", xlab="", ylab="", lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
title(main="(a) High resource")
plot(f4~t, type="l", xlab="", ylab="", lwd=2)
plot(f5~t, type="l", xlab="", ylab="", lwd=2)

#Low Water
f1 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f2 <- dnorm(t, mean=-2.5, sd=2)
f3 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f4 <- dnorm(t, mean=2.5, sd=2)
f5 <- dnorm(t, mean=-2.5, sd=2)
plot(f1~t, type="l", xlab="", ylab="", lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
title(main="(b) Low water")
plot(f4~t, type="l", xlab="", ylab="", lwd=2)
plot(f5~t, type="l", xlab="", ylab="", lwd=2)

#Low Nutrient
f1 <- dnorm(t, mean=-2.5, sd=2)
f2 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f3 <- dnorm(t, mean=-2.5, sd=2)
f4 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f5 <- dnorm(t, mean=-2.5, sd=2)
plot(f1~t, type="l", xlab="", ylab="", lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
title(main="(c) Low nutrients")
plot(f4~t, type="l", xlab="", ylab="", lwd=2)
plot(f5~t, type="l", xlab="", ylab="", lwd=2)

#Low Light
f1 <- dnorm(t, mean=-2.5, sd=2)
f2 <- dnorm(t, mean=2.5, sd=2)
f3 <- dnorm(t, mean=-2.5, sd=2)
f4 <- dnorm(t, mean=-2.5, sd=2)
f5 <- dnorm(t, mean=2.5, sd=2)
plot(f1~t, type="l", xlab="", ylab="", lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
title(xlab="Economics
Slow --> Fast", line=2, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
title(xlab="Height
Short --> Tall", line=2, cex.lab=1.2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
title(xlab="Rooting depth
Shallow --> Deep", line=2, cex.lab=1.2)
title(main="(d) Low light")
plot(f4~t, type="l", xlab="", ylab="", lwd=2)
title(xlab="Collaboration
Thin --> Thick root", line=2, cex.lab=1.2)
plot(f5~t, type="l", xlab="", ylab="", lwd=2)
title(xlab="Seeder --> Resprout", line=0.5, cex.lab=1.2)
#dev.off()





## Life history strategy fitness landscapes
#tiff("Fig7.9.LifeHistoryLandscapes.tif", res=300, width=1500, height=2000)
par(mfrow=c(4,3), mar=c(3,2,2,1), xaxt='n', yaxt='n')
#High light
t <- seq(-3,3,0.1)
f1 <- dnorm(t, mean= 2.5, sd=2)
f2 <- dnorm(t, mean= -2.5, sd=2)
f3 <- dnorm(t, mean= -2.5, sd=2)
plot(f1~t, type="l", xlab="",  lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
title(main="(a) High resources", line=0.5, cex.lab=1.2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
#Low light
t <- seq(-3,3,0.1)
f1 <- dnorm(t, mean= -2.5, sd=2)
f2 <- dnorm(t, mean= 2.5, sd=2)
f3 <- dnorm(t, mean= 2.5, sd=2)
plot(f1~t, type="l", xlab="",  lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
title(main="(b) Low resources", line=0.5, cex.lab=1.2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
#Low nutrients
t <- seq(-3,3,0.1)
f1 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f2 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f3 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
plot(f1~t, type="l", xlab="",  lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
title(main="(c) Frequent disturbance", line=0.5, cex.lab=1.2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
#Low water
t <- seq(-3,3,0.1)
f1 <- dnorm(t, mean= 2.5, sd=2)
f2 <- dnorm(t, mean= -2.5, sd=2)
f3 <- dnorm(t, mean= -2.5, sd=2)
plot(f1~t, type="l", xlab="",  lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
title(xlab="Pace of life continuum
Slow --> Fast", line=2, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
title(main="(d) Large disturbances", line=0.5, cex.lab=1.2)
title(xlab="Reproduction
Semelparity->Iteroparity", line=2, cex.lab=1.2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
title(xlab="Lifespan
Short --> Long", line=2, cex.lab=1.2)
#dev.off()




## Fig five factors fitness landscapes Temperature
#tiff("Fig7.15.TraitLandscapes.tiff", res=300, width=2100, height=1000)
par(mfrow=c(2,5), mar=c(3,2,2,1), xaxt='n', yaxt='n')
#Cold
t <- seq(-3,3,0.1)
f1 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f2 <- dnorm(t, mean=-2.5, sd=2)
f3 <- dnorm(t, mean=-2.5, sd=2)
f4 <- dnorm(t, mean=-2.5, sd=2)
f5 <- dnorm(t, mean=2.5, sd=2)
plot(f1~t, type="l", xlab="", ylab="", lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
title(main="(a) Cold temperature")
plot(f4~t, type="l", xlab="", ylab="", lwd=2)
plot(f5~t, type="l", xlab="", ylab="", lwd=2)

#Hot
f1 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f2 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f3 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f4 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
f5 <- 0.25*dnorm(t, mean=-2.5, sd=0.73) + 0.25*dnorm(t, mean=-0.84, sd=0.76) +
  0.25*dnorm(t, mean=0.84, sd=0.76) + 0.25*dnorm(t, mean=2.5, sd=0.73)
plot(f1~t, type="l", xlab="", ylab="", lwd=2)
title(ylab="Fitness", line=0.5, cex.lab=1.2)
title(xlab="Economics
Slow --> Fast", line=2, cex.lab=1.2)
plot(f2~t, type="l", xlab="", ylab="", lwd=2)
title(xlab="Height
Short --> Tall", line=2, cex.lab=1.2)
plot(f3~t, type="l", xlab="", ylab="", lwd=2)
title(xlab="Rooting depth
Shallow --> Deep", line=2, cex.lab=1.2)
title(main="(b) Hot temperature")
plot(f4~t, type="l", xlab="", ylab="", lwd=2)
title(xlab="Collaboration
Thin --> Thick root", line=2, cex.lab=1.2)
plot(f5~t, type="l", xlab="", ylab="", lwd=2)
title(xlab="Seeder --> Resprout", line=0.5, cex.lab=1.2)
#dev.off()
