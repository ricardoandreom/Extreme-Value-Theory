# libraries
library(readxl)
library(latex2exp)
library(Rcmdr)


marcas <- readXL("C:/Users/Admin/Desktop/Tese/Saltos/TJ_fem.xlsx",rownames=FALSE, header=TRUE, na="", sheet="Folha2", stringsAsFactors=FALSE)
View(marcas)

x <- marcas[,"Mark"]
x
#   Pickands estimator  
gammahat_pickands<-function(k){
  1/log(2)*log((x[700-floor((k+1)/4)+1]-x[700-2*floor((k+1)/4)+1])/
                 (x[700-2*floor((k+1)/4)+1]-x[700-4*floor((k+1)/4)+1]))}
xp<-seq(20,699,1)
P_est<-sapply(xp,gammahat_pickands)
#pdf('estimador_Pickands.pdf')
plot(P_est,type="l",xlab="k",ylab=expression(hat(gamma)[list(P)]),main=TeX(r'(Estimador de $\gamma$)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=0, col="green")
#dev.off()

# Moments Estimator
Mnr<-function(k,r) {
  y<-NULL
  for(j in 1:k) {
    y[j]<-(log(x[700-j+1])-log(x[700-k]))^r
  }
  (1/k)*sum(y)
}
gammahat_momentos<-function(k) Mnr(k,1)+0.5*(1-(Mnr(k,2)/(Mnr(k,1))^2-1)^-1)
xm<-seq(10,699,1)
M_est<-sapply(xm,gammahat_momentos)
#pdf('estimador_momentos.pdf')
plot(M_est,type="l",xlab="k",ylab=expression(hat(gamma)[list(M)]),main=TeX(r'(Estimador de $\gamma$)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=0,col="green")
#dev.off()

# GammaMenos Estimator
gammahat_menos<-function(k) 1-0.5*(1-((Mnr(k,1))^2)/Mnr(k,2))^-1
x_menos<-seq(1,699,1)
Estimador_Gamma_menos<-sapply(x_menos,gammahat_menos)
#pdf('estimador_gama_menos.pdf')
plot(Estimador_Gamma_menos,type="l",xlab="k",ylab=expression(hat(gamma)[list("-")]),main=TeX(r'(Estimador de $\gamma$)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=0,col="green")
#dev.off() 

# Mixed Moments Estimator
Lnk<-function(k) {
  y<-NULL
  for(j in 1:k) {
    y[j]<-(1-(x[700-k]/x[700-j+1]))
  }
  (1/k)*sum(y)}

phi<-function(k) (Mnr(k,1)-Lnk(k))/(Lnk(k))^2
gammahat_mm<-function(k) (phi(k)-1)/(1+2*min(phi(k)-1,0))
xmm<-seq(40,699,1)
MM_est<-sapply(xmm,gammahat_mm)
#pdf('estimador_gama_mm.pdf') 
plot(MM_est,type="l",xlab="k",ylab=expression(hat(gamma)[list(MM)]),main=TeX(r'(Estimador de $\gamma$)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=0,col="green")
#dev.off()

#################################################################

# Test1 Gamma=0
TMnr<-function(k,r) {
  y<-NULL
  for(j in 1:k) {
    y[j]<-(x[700-j+1]-x[700-k])^r
  }
  (1/k)*sum(y)
}
G<-function(k) TMnr(k,2)/(TMnr(k,1))^2
EstaTeste1<-function(k) (G(k)-2)*(sqrt(k/4))
xtm<-seq(10,699,1)
Ttest1<-sapply(xtm,EstaTeste1)
#pdf('teste1_gama=zero.pdf') # guardar o plot em pdf
plot(xtm,Ttest1,type="l",xlab="k",ylab=expression(G[list(n,k)]),
     main=TeX(r'(Teste Greenwood)', bold=TRUE),mgp=c(2.2,1,0)) 
abline(h=-1.645, col="red")
abline(v=165, col="blue")
text(210,-3.8,"k=165",cex=0.9,col="blue")
text(600,-1.4,TeX(r'($z_{0.05}=-1.645$)',bold=TRUE),cex=0.9,col="red")
#dev.off()

# Test2 Gamma=0
H<-function(k) 1/(k*(G(k)-1))
EstaTeste2<-function(k) (k*H(k)-1)*(sqrt(k/4))
pvalor2<-function(k) 1-pnorm(EstaTeste2(k))
xtm<-seq(20,699,1)
Ttest2<-sapply(xtm,EstaTeste2)
#pdf('teste2_gama=zero_hasoferwang.pdf') 
plot(xtm,Ttest2,type="l",xlab="k",ylab=expression(HW[list(n,k)]),
     main=TeX(r'(Teste Hasofer & Wang)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=1.645, col="red")
abline(v=160, col="blue")
text(200,1.2,"k=160",cex=0.9,col="blue")
text(580,1.9,TeX(r'($z_{0.95}=1.645$)',bold=TRUE),cex=0.9,col="red")
#dev.off()

# Right endpoint finitess test
aux <- function(k) Mnr(k,1)^2/Mnr(k,2)
ank <- function(k) x[700-k]*0.5*Mnr(k,1)*(1-aux(k))^-1
Tn1<-function(k) {
  y<-NULL
  for(j in 1:k) {
    y[j]<-(x[700-j]-x[700-k]-ank(k))/(x[700]-x[700-k])
  }
  (1/k)*sum(y)
}
Tn1A  <- function(k) sqrt(k)*log(k)*Tn1(k)
u<-seq(20,699,1)
TestFin<-sapply(u,Tn1A)
#pdf('teste_finitude_xF.pdf')
plot(u,TestFin,type="l",xlab="k",ylab=expression(E[list(n,k)]),main=TeX(r'(Teste à finitude de $x_{F}$)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=-1.96, col="red")
abline(h=1.96, col="red")
abline(v=103, col="blue")
text(140,-9.5,"k=103",cex=0.9,col="blue")
text(600,-2.35,TeX(r'($z_{0.025}=-1.96$)',bold=TRUE),cex=0.9,col="red")
#dev.off()

#Right endpoint 1
XFhat1<-function(k) 
  x[700-k]-(x[700-k]*Mnr(k,1)*(1-gammahat_menos(k)))/gammahat_menos(k)
xF1<-seq(100,699,1)
xFplot1<-sapply(xF1,XFhat1)
#pdf('xF1.pdf')
plot(xF1,xFplot1,type="l",xlab="k",ylab=expression(hat(x[list(F)])),main=TeX(r'(Estimador de $x_{F}$)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=max(x),col="magenta")
#dev.off()

# Right endpoint 2
XFhat2<-function(k) 
  x[700-k]-(x[700-k]*Mnr(k,1)*(1-gammahat_menos(k)))/gammahat_momentos(k)
xF2<-seq(1,699,1)
xFplot2<-sapply(xF2,XFhat2)
#pdf('xF2.pdf')
plot(xF2,xFplot2,type="l",xlab="k",ylab=expression(hat(x[list(F)])),main=TeX(r'(Estimador de $x_{F}$)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=max(x),col="magenta")
#dev.off()

#Right endpoint 3
XFhat3<-function(k) 
  x[700-k]-(x[700-k]*Mnr(k,1)*(1-gammahat_menos(k)))/gammahat_mm(k)
xF3<-seq(1,699,1)
xFplot3<-sapply(xF3,XFhat3)
#pdf('xF3.pdf')
plot(xF3,xFplot3,type="l",xlab="k",ylab=expression(hat(x[list(F)])),main=TeX(r'(Estimador de $x_{F}$)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=max(x), col="magenta")
#dev.off()

#Right endpoint 4 - Article Fraga Alves
XFhat4A<-function(k) {
  y<-NULL
  for(j in 0:k-1) {
    y[j]<-(x[700-k-j])*log(1+1/(j+k))
  }
  (1/log(2))*sum(y)
}
XFhat4<-function(k) x[700]+x[700-k]-XFhat4A(k)
xF4<-seq(1,350,1)
xFplot4<-sapply(xF4,XFhat4)
#pdf('xF4.pdf')
plot(xF4,xFplot4,type="l",xlab="k",ylab=expression(hat(x[list(F)])),main=TeX(r'(Estimador de $x_{F}$)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=max(x),col="magenta")
#dev.off()

#  Test Gamma=0 Fraga Alves2017
TesteFA<-function(k) (XFhat4(k)-x[700-k])/(x[700-k]-x[700-2*k])
EstTesteFA<-function(k) (log(2))*TesteFA(k)-(log(k)+0.5*log(k))
En<-seq(10,350,1)
ETn<-sapply(En,EstTesteFA)
#pdf('Teste_gamma=0_Fraga_Alves2017.pdf')
plot(En,ETn,type="l",xlab="k",ylab=expression(G[list(n,k)]^{"*"}),main=TeX(r'(Teste FA)', bold=TRUE),mgp=c(2.2,1,0))
abline(h=-1.0972,col="red")
text(270,-0.5,TeX(r'($g_{0.05}=-1.0972$)',bold=TRUE),cex=0.9,col="red")
#dev.off()

# x_F and gamma estimators with k_optimal
kopt=165 #TJ female
#Kopt=223 #TJ male
#kopt=310 #LJ female

gammahat_pickands(kopt)
gammahat_momentos(kopt)
gammahat_menos(kopt)
gammahat_mm(kopt)

XFhat1(kopt)
XFhat2(kopt)
XFhat3(kopt) 
XFhat4(kopt)