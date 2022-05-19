#############################
### Plant Strategies      ###
### Daniel C. Laughlin    ###
#############################

### Chapter 8 ###

#setwd("~/OneDrive - University of Wyoming/Data/Book/github_repo")
library(fields)
library(MetBrewer)

#tiff("Fig8.1.Fitness.tiff", res=300, width=1500, height=800)
set.seed(19240266)
env = seq(-2,2,length.out=5)
env.cont = seq(-2,2,length.out=100)
sigma=1.3
cols <- RColorBrewer::brewer.pal(5,"Set1")
par(mfrow=c(1,2),mar=c(3,3,2,1), xaxt='n', yaxt='n')
plot(env.cont, dnorm(env.cont,-1.5,0.5), type="l", col=cols[5], lwd=3,
     xlab="",ylab="", cex.lab=3, ylim=c(0.1,0.8), main="(a)", cex.main=1)
lines(env.cont, dnorm(env.cont,-0.75,0.5), col=cols[4], lwd=3)
lines(env.cont, dnorm(env.cont,0,0.5), col=cols[3], lwd=3)
lines(env.cont, dnorm(env.cont,0.75,0.5), col=cols[2], lwd=3)
lines(env.cont, dnorm(env.cont,1.5,0.5), col=cols[1], lwd=3)
title(ylab="Fitness", line=1, cex.lab=1)
title(xlab="Environment", line=1, cex.lab=1)

### Fitness Surface
s1 = rnorm(5,-1.5,0.05)
s1z = exp(-(s1 - (-env))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
s2 = rnorm(5,-0.75,0.05)
s2z = exp(-(s2 - (-env))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
s3 = rnorm(5,0,0.05)
s3z = exp(-(s3 - (-env))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
s4 = rnorm(5,0.75,0.05)
s4z = exp(-(s4 - (-env))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
s5 = rnorm(5,1.5,0.05)
s5z = exp(-(s5 - (-env))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
spdat <- data.frame(cbind(t = c(s1,s2,s3,s4,s5), e = c(rep(env,5)), f = c(s1z,s2z,s3z,s4z,s5z) ))
spdat$sp = c(rep(cols[1],5),rep(cols[2],5),rep(cols[3],5),rep(cols[4],5),rep(cols[5],5))

par(mar=c(1,2,2,1))
grid.l<- list( seq(-2,2,length=20), seq(-2,2,length=20)) 
te <- make.surface.grid(grid.l)
z.fit <- exp(-(te[,1] - (-te[,2]))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
te.norm <- as.surface(te, z.fit)
persp(te.norm$x,te.norm$y,te.norm$z, theta = 30, phi = 30,
      xlab="\nTrait",ylab="\nEnvironment",zlab="\nFitness",col=adjustcolor("white",alpha.f=.3),
      main="(b)", cex.main=1,
      cex.lab=1,lwd=0.5, border="grey40") -> pmat
lines(trans3d(rep(-1.5,100),env.cont,exp(-(rep(-1.5,100) - (-env.cont))^2/(2*sigma^2))/(sqrt(2*pi)*sigma),pmat), pch=19, cex=1, lwd=3, col=spdat$sp[1:5])
lines(trans3d(rep(-0.75,100),env.cont,exp(-(rep(-0.75,100) - (-env.cont))^2/(2*sigma^2))/(sqrt(2*pi)*sigma),pmat), pch=19, cex=1, lwd=3, col=spdat$sp[6:10])
lines(trans3d(rep(0,100),env.cont,exp(-(rep(0,100) - (-env.cont))^2/(2*sigma^2))/(sqrt(2*pi)*sigma),pmat), pch=19, cex=1, lwd=3, col=spdat$sp[11:15])
lines(trans3d(rep(0.75,100),env.cont,exp(-(rep(0.75,100) - (-env.cont))^2/(2*sigma^2))/(sqrt(2*pi)*sigma),pmat), pch=19, cex=1, lwd=3, col=spdat$sp[16:20])
lines(trans3d(rep(1.5,100),env.cont,exp(-(rep(1.5,100) - (-env.cont))^2/(2*sigma^2))/(sqrt(2*pi)*sigma),pmat), pch=19, cex=1, lwd=3, col=spdat$sp[21:25])
#dev.off()
### End of Fig


### Figure 8.4. log lambda figure
set.seed(29)
intra <- rnorm(100)
inter <- rnorm(100)
f <- -intra -inter +rnorm(100)
cols <- met.brewer(name="Moreau",n=7,type="discrete")

#tiff("Fig8.4LDGR.tiff", res=300, width=1500, height=800)
par(mfrow=c(1,2),mar=c(4,4,2,1), xaxt='n', yaxt='n')
plot(f[1:50], type="l", ylab="", xlab="", col="grey40", main="(a)", lwd=0.5)
points(f[1:50], cex=0.5, pch=19, col=1)
title(ylab="log(lambda)", line=0.5, cex.lab=0.7)
title(xlab="Time (years)", line=0.5, cex.lab=0.7)
grid.l<- list( seq(-2,2,length=20), seq(-2,2,length=20)) 
ae <- make.surface.grid(grid.l)
z.fit <- -ae[,1] -ae[,2]
ae.norm <- as.surface(ae, z.fit)
par(mar=c(2,2,2,1))
persp(ae.norm$x,ae.norm$y,ae.norm$z, theta = 40, phi = 30,
      xlab="\nConspecific density",ylab="\nHeterospecific density",zlab="\nlog(lambda)",col=adjustcolor("white",alpha.f=.3),
      main="(b)", cex.main=1.2,
      cex.lab=0.7,lwd=0.5, border="grey40") -> pmat
points(trans3d(intra,inter,f,pmat), pch=19, cex=0.5, col=1)
points(trans3d(min(intra)+0.9,min(inter)+0.4,max(f)+0.1,pmat), pch=19, cex=1.2, col=cols[5])
lines(trans3d(rep(0,100),seq(-3,-1.5,length.out=100),rep(max(f)-0.6,100),pmat), lty=2, lwd=1.5)
points(trans3d(min(intra)+0.9,min(inter)+0.4,max(f)-2,pmat), pch=19, cex=1.2, col=cols[3])
legend(trans3d(min(intra),mean(inter)+1.9,max(f)+0.3,pmat), c("Intrinsic growth rate","Invasion growth rate"),
       pch=19, col=c(cols[5], cols[3]),cex=0.45)
#dev.off()
### End figure


## Fig 8.5 Density dependece in vital rates
#tiff("Fig8.5.tiff", res=300, width=2000, height=1400)
cols <- met.brewer(name="Moreau",n=7,type="discrete")
par(mfrow=c(2,3), mar=c(1,1,1,1))
set.seed(29)
intra <- rnorm(100)
size_t <- rnorm(100)
size_tplus1 <- 2*size_t -2*intra +rnorm(100)
grid.l<- list( seq(-2,2,length=20), seq(-2,2,length=20)) 
ae <- make.surface.grid(grid.l)
z.fit <- 2*ae[,1] -2*ae[,2]
ae.norm <- as.surface(ae, z.fit)
persp(ae.norm$x,ae.norm$y,ae.norm$z, theta = 40, phi = 20,
      xlab="\nSize in t",ylab="\nConspecific density",zlab="\nSize in t+1",
      col=adjustcolor("white",alpha.f=.3),
      main="(a)", cex.lab=1,lwd=0.5, border="grey40", d=2) -> pmat
lines(trans3d(seq(-2,2,length.out=100), rep(-2,100), 4+2*seq(-2,2,length.out=100),pmat),
      lty=1, lwd=3, col=cols[5])
### survival
set.seed(15)
size_t = rnorm(100)           # some continuous variables 
intra = rnorm(100)
z = size_t - intra       # linear combination
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(100,1,pr)      # bernoulli response variable
df = data.frame(y=y, size_t=size_t, intra=intra) #now feed it to glm:
m1 <- glm( y~size_t+intra,data=df,family="binomial")
summary(m1)
grid.l <- list( seq(-2,2,length=50), seq(-2,2,length=50)) 
te <- make.surface.grid(grid.l)
te.linear.pred <- as.surface(te, 1/(1+exp(-(2*te[,1] - 2*te[,2] ))))
persp(te.linear.pred$x,te.linear.pred$y,te.linear.pred$z, theta = 20, phi = 20,
      xlab="\nSize in t",ylab="\nConspecific density",zlab="\nSurvival probability",
      col=adjustcolor("white",alpha.f=.3),
      main="(b)",cex.lab=1,lwd=0.5, border="grey40", d=2) -> pmat
lines(trans3d(seq(-2,2,length.out=100), rep(-2,100), 1/(1+exp(-(4+2*seq(-2,2,length.out=100)))),pmat),
      lty=1, lwd=3, col=cols[5])
### flowering
set.seed(15)
size_t = rnorm(100)           # some continuous variables 
intra = rnorm(100)
z = size_t - intra       # linear combination
y = exp(z)      # bernoulli response variable
df = data.frame(y=y, size_t=size_t, intra=intra) #now feed it to glm:
m1 <- glm( y~size_t+intra,data=df,family="poisson")
summary(m1)
grid.l <- list( seq(-2,2,length=50), seq(-2,2,length=50)) 
te <- make.surface.grid(grid.l)
te.linear.pred <- as.surface(te, exp((te[,1] - te[,2])) )
persp(te.linear.pred$x,te.linear.pred$y,te.linear.pred$z, theta = 30, phi = 20,
      xlab="\nSize in t",ylab="\nConspecific density",zlab="\nRecruitment rate",
      col=adjustcolor("white",alpha.f=.3), main="(c)",
      cex.lab=1,lwd=0.5, border="grey40", d=2) -> pmat
lines(trans3d(seq(-2,2,length.out=100), rep(-2,100),55*exp( seq(-2,2,length.out=100) + rep(-2,100)), pmat),
      lty=1, lwd=3, col=cols[5])
### Heterospecific
set.seed(29)
intra <- rnorm(100)
size_t <- rnorm(100)
size_tplus1 <- 2*size_t -2*intra +rnorm(100)
grid.l<- list( seq(-2,2,length=20), seq(-2,2,length=20)) 
ae <- make.surface.grid(grid.l)
z.fit <- 2*ae[,1] -2*ae[,2]
ae.norm <- as.surface(ae, z.fit)
persp(ae.norm$x,ae.norm$y,ae.norm$z, theta = 40, phi = 20,
      xlab="\nSize in t",ylab="\nHeterospecific density",zlab="\nSize in t+1",
      col=adjustcolor("white",alpha.f=.3),
      main="(d)", cex.lab=1,lwd=0.5, border="grey40", d=2) -> pmat
lines(trans3d(seq(-2,2,length.out=100), rep(0,100), 2*seq(-2,2,length.out=100),pmat),
      lty=1, lwd=3, col=cols[3])
### survival
set.seed(15)
size_t = rnorm(100)           # some continuous variables 
intra = rnorm(100)
z = size_t - intra       # linear combination
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(100,1,pr)      # bernoulli response variable
df = data.frame(y=y, size_t=size_t, intra=intra) #now feed it to glm:
m1 <- glm( y~size_t+intra,data=df,family="binomial")
summary(m1)
grid.l <- list( seq(-2,2,length=50), seq(-2,2,length=50)) 
te <- make.surface.grid(grid.l)
te.linear.pred <- as.surface(te, 1/(1+exp(-(2*te[,1] - 2*te[,2] ))))
persp(te.linear.pred$x,te.linear.pred$y,te.linear.pred$z, theta = 20, phi = 20,
      xlab="\nSize in t",ylab="\nHeterospecific density",zlab="\nSurvival probability",
      col=adjustcolor("white",alpha.f=.3),
      main="(e)",cex.lab=1,lwd=0.5, border="grey40", d=2) -> pmat
lines(trans3d(seq(-2,2,length.out=100), rep(0,100), 1/(1+exp(-(2*seq(-2,2,length.out=100)))),pmat),
      lty=1, lwd=3, col=cols[3])
### flowering
set.seed(15)
size_t = rnorm(100)           # some continuous variables 
intra = rnorm(100)
z = size_t - intra       # linear combination
y = exp(z)      # bernoulli response variable
df = data.frame(y=y, size_t=size_t, intra=intra) #now feed it to glm:
m1 <- glm( y~size_t+intra,data=df,family="poisson")
summary(m1)
grid.l <- list( seq(-2,2,length=50), seq(-2,2,length=50)) 
te <- make.surface.grid(grid.l)
te.linear.pred <- as.surface(te, exp((te[,1] - te[,2])) )
persp(te.linear.pred$x,te.linear.pred$y,te.linear.pred$z, theta = 30, phi = 20,
      xlab="\nSize in t",ylab="\nHeterospecific density",zlab="\nRecruitment rate",
      col=adjustcolor("white",alpha.f=.3), main="(f)",
      cex.lab=1,lwd=0.5, border="grey40", d=2) -> pmat
lines(trans3d(seq(-2,2,length.out=100), rep(0,100),
              exp( seq(-2,2,length.out=100) + rep(0,100)), pmat),
      lty=1, lwd=3, col=cols[3])
#dev.off()
### End figure



### Fig 8.6 common garden
#tiff("Fig8.6.Gardens.tiff", res=300, width=1000, height=1000)
env.cont = seq(-2,2,length.out=100)
cols <- met.brewer(name="Moreau",n=7,type="discrete")
par(mfrow=c(1,1),mar=c(3,3,2,1), xaxt='n', yaxt='n')
plot(env.cont, dnorm(env.cont,-1.5,1), type="l", lwd=3, col="darkgreen",
     xlab="",ylab="", cex.lab=3, ylim=c(0.1,0.5), main="", cex.main=1)
lines(env.cont, dnorm(env.cont,0,1), lwd=3, col="darkgoldenrod")
lines(env.cont, dnorm(env.cont,1.5,1), lwd=3, col=1)
abline(0.3,0, lty=2)
title(ylab="Intrinsic growth rate", line=1, cex.lab=1)
title(xlab="Elevation", line=1, cex.lab=1)
#dev.off()




## Fig 8.10 species traits and fitness and environment
library(fields)
library(plotrix)
library(vegan)
library(MetBrewer)
set.seed(19240266)
env = seq(-2,2,length.out=5)
env.cont = seq(-2,2,length.out=100)
sigma=1.3
blues <- c("#abc9c8", "#72aeb6", "#4692b0", "#2f70a1", "#134b73", "#0a3351")
reds <- c("#eab1c6", "#d35e17", "#e18a1f", "#e9b109", "#829d44")
cols=c(blues[2], blues[4], reds[2], reds[4])
traits <- data.frame(species=c("A", "B", "C", "D"), ldmc=c(1.25,1.25,-1.25,-1.25), tlp=c(-1.5,-1,1,1.5))

#tiff(file="Fig8.10example.tif", res=300, width=1200, height=1500)
par(mfcol=c(3,2),mar=c(2,2,2,1))

#panel A
plot(traits$ldmc, traits$tlp, xlab="", ylab="",
     pch=19,col="white", xlim=c(-2,2), ylim=c(-2,2), xaxt='n', yaxt='n',cex.lab=0.8, main="(a)", font.main=1)
text(traits$ldmc, traits$tlp, traits$species, cex=0.7)
draw.circle(-1.25,1.25,0.7, border=cols[2])
draw.circle(1.25,-1.25,0.7,border=cols[3])
text(-1.4,-0.3, "High water
     strategy", cex=0.7,col=cols[2])
text(1.4,0.3, "Low water
     strategy", cex=0.7, col=cols[3])
abline(0,-1,lty=2)
text(0,0,"Trait
     axis", srt=-43)
title(ylab="Turgor Loss Point", line=0.5, cex.lab=1)
title(xlab="Leaf dry matter content", line=0.5, cex.lab=1)

#panel B
rownames(traits) <- traits$species
d <- vegdist(traits[,2:3], method="euclidean")
res <- hclust(d, method = "aver")
par(mar=c(5,3,2,2))
plot(as.dendrogram(res),horiz=T, main="(b)", font.main=1, xlab="Distance")


#panel C
par(mar=c(4,4,2,1))
pca <- princomp(traits[,2:3])
plot(pca$scores[,1], pca$scores[,2], col="white",xlab="PC1: Trait axis", ylab="PC2", main="(c)", font.main=1,
     xlim=c(-2.2,2.2), ylim=c(-0.3,0.3))
text(pca$scores[,1], pca$scores[,2], traits$species, cex=1, col=c(cols[3], cols[3], cols[2], cols[2]))
arrows(0,0,1.1,0.02, col="black", length=0.1); text(1.6,0.02, "LDMC", col="black", cex=0.8)
arrows(0,0,-1.1,0.02, col="black", length=0.1); text(-1.6, 0.02, "TLP", col="black", cex=0.8)

#panel D
par(mar=c(2,2,2,1))
plot(env.cont, dnorm(env.cont,-1.5,0.5), type="l", col=cols[3], lwd=3, main="(d)", font.main=1,
     xlab="",ylab="", cex.lab=0.8, ylim=c(0.1,1), xaxt='n', yaxt='n')
lines(env.cont, dnorm(env.cont,-1,0.5), col=cols[4], lwd=3, lty=2)
lines(env.cont, dnorm(env.cont,1,0.5), col=cols[2], lwd=3, lty=3)
lines(env.cont, dnorm(env.cont,1.5,0.5), col=cols[1], lwd=3, lty=4)
text(c(-1.5,-1,1,1.5),c(0.9, 0.9, 0.9, 0.9),c("A", "B", "C", "D"))
title(ylab="Fitness", line=0.5, cex.lab=1)
title(xlab="Soil water", line=0.5, cex.lab=1)

#panel E
plot(env.cont, dnorm(env.cont,-1.25,0.75), type="l", col=cols[3], lwd=3,  main="(e)",font.main=1,
     xlab="",ylab="", cex.lab=0.8, ylim=c(0.1,0.6), xaxt='n', yaxt='n')
lines(env.cont, dnorm(env.cont,1.25,0.75), col=cols[2], lwd=3, lty=2)
text(-1.25,0.2, "Low water
strategy", cex=0.7, col=cols[3])
text(1.25,0.2, "High water
strategy", cex=0.7, col=cols[2])
title(ylab="Fitness", line=0.5, cex.lab=1)
title(xlab="Soil water", line=0.5, cex.lab=1)

#panel F
s1 = rnorm(5,-1.5,0.05)
s1z = exp(-(s1 - (-env))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
s2 = rnorm(5,-1,0.05)
s2z = exp(-(s2 - (-env))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
s3 = rnorm(5,1,0.05)
s3z = exp(-(s3 - (-env))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
s4 = rnorm(5,1.5,0.05)
s4z = exp(-(s4 - (-env))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
spdat <- data.frame(cbind(t = c(s1,s2,s3,s4), e = c(rep(env,4)), f = c(s1z,s2z,s3z,s4z) ))
spdat$sp = c(rep(cols[1],5),rep(cols[2],5),rep(cols[4],5),rep(cols[3],5))

par(mar=c(1,1,1,1))
grid.l<- list( seq(-2,2,length=20), seq(-2,2,length=20)) 
te <- make.surface.grid(grid.l)
z.fit <- exp(-(te[,1] - (-te[,2]))^2/(2*sigma^2))/(sqrt(2*pi)*sigma)
te.norm <- as.surface(te, z.fit)
persp(te.norm$x,te.norm$y,te.norm$z, theta = 30, phi = 20,
      xlab="\nTrait axis",ylab="\n\nSoil water",zlab="\nFitness",col=adjustcolor("white",alpha.f=0.3),
      main="(f)", font.main=1, cex.main=1.2,
      cex.lab=1,lwd=0.3, border="grey40") -> pmat
points(trans3d(spdat$t,spdat$e,spdat$f,pmat), pch=19, cex=1, col=spdat$sp)
#points(trans3d(spdat$t,spdat$e,0.01,pmat), pch=19, cex=1.5, col=spdat$sp)
#dev.off()