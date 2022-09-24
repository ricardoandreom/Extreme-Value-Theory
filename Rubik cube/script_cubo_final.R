library(extRemes)
library(evd)
library(fExtremes)
library(Rcmdr)
library(readxl)
library(latex2exp)

marcas <- readXL("C:/Users/Admin/Desktop/Tese/Cubo Rubik/cubo magico.xlsx",rownames=FALSE, header=TRUE, na="", sheet="Sheet1", stringsAsFactors=FALSE)
View(marcas)

x<- marcas[,"Simmetric_Mark"]
x

#pdf('histograma.pdf')
hist(x,prob=T,ylim=c(0,3),xlab="x",main="GEV",ylab="Freq",font=2,family="serif"
     ,font.lab=2,cex.lab=1.5,col='skyblue')
#dev.off()

fit1 = fevd(x,type="GEV")
fit1 #->(Usa estimadores MV)

png(file='fit1.png',width=1000, height=1000,res=100)
plot(fit1)
dev.off()

out1<-gevFit(x,type="pwm")
out1
#plot(out1)

'probability quantiles for both methods - order 0.95,0.98,0.99'

qevd(0.99,-4.8981079,0.1119791,0.4756207,type="GEV")
qevd(0.98,-4.8981079,0.1119791,0.4756207,type="GEV")
qevd(0.95,-4.8981079,0.1119791,0.4756207,type="GEV")

qevd(0.99,-4.8868204,0.1300748,0.2685176,type="GEV")
qevd(0.98,-4.8868204,0.1300748,0.2685176,type="GEV")
qevd(0.95,-4.8868204,0.1300748,0.2685176,type="GEV")

#############################################################
n<-100
GS<-(max(x)-x[floor(n/2)+1])/(x[floor(n/2)+1]-min(x))
bn<-(log(n)+log(log(2)))/(log(log(n))-log(log(2)))
an<-1/(log(log(n)))
Gstar<-(GS-bn)/an
Gstar