#############################
### Plant Strategies      ###
### Daniel C. Laughlin    ###
#############################

### Chapter 9 ###

library(MetBrewer)
library(learnPopGen)

## Hawk-Dove Game
# V=ResourceValue, C=Cost
V=2
C=1
Payoff <- matrix(c(0.5*(V-C),0,V,V/2),2,2)
v_over_c <- hawk.dove(p=c(0.01,0.99),M=Payoff,time=15)

V=2
C=3
Payoff <- matrix(c(0.5*(V-C),0,V,V/2),2,2)
v_under_c <- hawk.dove(p=c(0.01,0.99),M=Payoff,time=15)

#Figure 9.2
#tiff(file="Fig9.2.HawkDove.tif", res=300, width=2100, height=1050)
par(mfrow=c(1,2), mar=c(4,4,2,1))
cols <- met.brewer(name="Moreau",n=7,type="discrete")
plot(c(1:15), v_over_c$dove,type="l", lwd=4, ylim=c(0,1), col=cols[3], cex.main=0.8,
     xlab="Time", ylab="Frequency", main="(a) Resource Value = 2, Cost of Injury = 1")
lines(c(1:15), v_over_c$hawk, lty=2, lwd=4, col=cols[5])
legend("right", legend=c("Dove","Hawk"), lwd=3, lty=c(1,2), bty="n", col=cols[c(3,5)])

plot(c(1:15), v_under_c$dove,type="l", lwd=4, ylim=c(0,1),col=cols[3], cex.main=0.8,
     xlab="Time", ylab="Frequency", main="(b) Resource Value = 2, Cost of Injury = 3")
lines(c(1:15), v_under_c$hawk, lty=2, lwd=4, col=cols[5])
#dev.off()
### End of figure



## Lotka-Volterra Game

library(MetBrewer)
cols <- met.brewer(name="Moreau",n=7,type="discrete")

# Functions-------------------------------------------------

### K function
K_v = function(Km=100, v, sigk=sqrt(12.5)) {
  K <- Km * exp(-(v^2)/(2*sigk^2))
  return(K)
}

### alpha function asymmetrical
aij = function(v,u,siga=2,B=2){
  aij <- 1 + exp(-((v-u+B)^2)/(2*siga^2)) - exp(-(B^2)/(2*siga^2))
  return(aij)
}

### update population sizes function for 2 species game
updateN=function(N,r,K,alpha){
  # N,r, and K are vectors with 2 elements
  # alpha is a 2x2 matrix
  # syntax: out = updateN(N,r,K,alpha)
  # out is a vector with 2 elements (updated population size of spp 1 and spp2)
  
  spp1 = N[1] + r[1]*N[1]* (K[1] - alpha[1,1]*N[1] - alpha[1,2]*N[2]) / K[1]
  spp2 = N[2]+r[2]*N[2]*(K[2]-alpha[2,2]*N[2]-alpha[2,1]*N[1])/K[2]
  out = c(spp1,spp2)
  return(out)
}

# Starting Parameters----------------------------------------------

r=c(0.25,0.25)
K=c(K_v(v=3.12),K_v(v=-0.24))
alpha=matrix(NA,2,2)
alpha[1,1]=aij(v=3.12,u=3.12)
alpha[1,2]=aij(v=3.12,u=-0.24) # check this
alpha[2,1]=aij(v=-0.24,u=3.12) # check this
alpha[2,2]=aij(v=-0.24,u=-0.24)
initialN=c(10,100)
Time=150
tiny=0.00001  #extinction threshold

# Loop --------------------------------------------------

N=matrix(NA,Time,2)
N[1,]=initialN

for(i in 2:Time){
  tmp=updateN(N[i-1,],r,K,alpha)
  tmp[tmp<tiny] = 0
  N[i,] = tmp
}

data <- cbind(1:Time, N)

### G-function
x=data[150,c(2,3)]
G_f = function(v, r=0.25, x){
  G <- (r/K_v(v=v)) * (K_v(v=v) - aij(v=v,u=3.12)*x[1] - aij(v=v,u=-0.24)*x[2])
  return(G)
}

# Plot output--------------------------------------------

### Figure 9.4
#tiff(file="Fig9.4.LVGame.tif", res=300, width=2000, height=2000)
par(mfrow=c(2,2), mar=c(5,4,2,1), cex.lab=1.2)
v <- seq(-10,10,length.out=200)
plot(v, K_v(v=v), ylab="Carrying capacities", xlab="Strategy u", type="l", main="(a)", lwd=2)

plot(v,aij(v=v,u=-5), ylab="Competition coefficients", xlab="Strategy u", type="l", main="(b)", lwd=2, col=cols[3])
lines(v,aij(v=v,u=0), lwd=2, col=cols[5])
lines(v,aij(v=v,u=5), lwd=2, col=cols[7])
legend("topright", c("uj = -5","uj = 0","uj = 5"), col=cols[c(3,5,7)], lwd=2, bty="n", cex=0.8)

plot(data[,1],data[,2],type="l", col=cols[3],xlab="Time", ylab="Population size (N)",ylim=c(0,110),lwd=3, main="(c)")
lines(data[,1],data[,3],type="l",col=cols[5],lwd=3, lty=3)
legend("topright",c("Strategy u1 = 3.12", "Strategy u2 = -0.24"), lwd=3, col=cols[c(3,5)], lty=c(1,3))

v <- seq(-5,5,length.out=200)
plot(v, G_f(v=v, x=x), xlab="Strategy u", ylab="G-function", type="l", lwd=2, main="(d)", ylim=c(-0.09,0.01))
abline(h=0, lwd=1, lty=2)
#dev.off()
### end of Figure 9.4



### Figure 9.5
G_f = function(v, r=0.25, x, u){
  G <- (r/K_v(v=v)) * (K_v(v=v) - aij(v=v,u=u[1])*x[1] - aij(v=v,u=u[2])*x[2])
  return(G)
}

#tiff(file="Fig9.5.LVGame.tif", res=300, width=2300, height=2300)
par(mfcol=c(2,2), mar=c(5,5,2,1), cex.lab=1.5)
plot(v, G_f(v=v, x=c(51,38), u=c(2,1)), xlab="", ylab="G-function", type="l", lwd=2, main="(a)")
abline(h=0, lty=2)
points(c(2,1),c(0.02,-0.02), pch=19, cex=2, col=cols[c(5,5)])
arrows(0.3, -0.02, 1.5, 0.025, length=0.15, lwd=1.5, col=cols[5])
plot(v, G_f(v=v, x=c(51,38), u=c(2.6,1.6)), xlab="Strategy u", ylab="G-function", type="l", lwd=2, main="(b)")
abline(h=0, lty=2)
points(c(2.6,1.6),c(-0.008,-0.035), pch=19, cex=2, col=cols[c(5,5)])
arrows(0.9, -0.038, 2.2, -0.004, length=0.15, lwd=1.5, col=cols[5])
plot(v, G_f(v=v, x=c(51,38), u=c(3.4,0.4)), xlab="", ylab="", type="l", lwd=2, main="(c)")
abline(h=0, lty=2)
points(c(3.4,0.4),c(-0.018,-0.009), pch=19, cex=2, col=cols[c(3,5)])
legend("bottom",c("Under selection","ESS"), col=cols[c(5,3)], pch=19, cex=1.3, bty="n")
arrows(1, -0.007, 0,-0.002, length=0.15, lwd=1.5, col=cols[5])
plot(v, G_f(v=v, x=c(51.34,38.91), u=c(3.12,-0.24)), xlab="Strategy u", ylab="", type="l", lwd=2, main="(d)")
abline(h=0, lty=2)
points(c(3.12,-0.24),c(0,0), pch=19, cex=2, col=cols[c(3,3)])
#dev.off()
### End of Figure 9.5