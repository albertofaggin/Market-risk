############################## Carico le librerie ##############################
rm(list=ls())
library("quantmod")
library("astsa")
library("readxl")
library("tidyverse")
library("fImport")
library("fBasics")
library("rugarch")
library("highfrequency")
library("xts")
library("zoo")
library("chron")
library("gridExtra")

########################## Lettura e pulizia dei dati ##########################
rm(list=ls())
setwd("~/Desktop/Master/Analisi dei dati in finanza/02. Rischio di mercato/Homework")
data=read_excel('STOXX600-2.xlsx','TotalReturnsIndexes','B7:AZ3202',col_names=FALSE,na="NA")
T=dim(data)[1]
#data1=as.matrix(data)
data1=data
i=is.na(data1)
idr=rowSums(i)
idc=colSums(i)
plot(idc) # la presenza di missing è associata a valori non nulli. Si osserva che
# tengono a crescere, quindi sono le imprese più piccole che sono da poco
# entrate nell'indice
data1=data1[,idc==0]
i=is.na(data1)
idr=rowSums(i)
plot(idr) # non sono più presenti dati mancanti
idc=colSums(i)
T=dim(data1)[1]
N=dim(data1)[2]
data1 = data.frame(data1)

r=100*(log(data1[2:T,])-log(data1[1:T-1,]))
r = as.matrix(r)
i0= r==0
i0r=rowSums(i0)
plot(i0r) #posso osservare la frequenza degli zeri

r=r[i0r<300,]# prendo le righe dove questo indicatore risulta essere minore di 300


dataFF = read.csv("Factors_Fama.csv", sep = ";")
str(dataFF)
View(dataFF)
F=as.matrix(dataFF[,-c(1,7)]) # elimino la prima colonna relativa al periodo

hist(F, nclass = 90, xlim = c(-2.5,2.5), main = "Istogramma di frequenza dei rendimenti")

dataFF2 = read_excel('Factors_Futures.xlsx','Foglio1','B2:E3197',col_names=FALSE,na="NA")
str(dataFF2)
T=dim(dataFF2)[1]
N=dim(dataFF2)[2]
dataFF2 = data.frame(dataFF2)


rFF2=100*(log(dataFF2[2:T,]+1)-log(dataFF2[1:T-1,]+1))

find.NA=function(data){
     NA.count=rep(0,ncol(data))
    for (i in 1:ncol(data)){
         NA.count[i]=length(which(is.na(data[,i])))
       }
     return(NA.count)
  }
find.NA(rFF2)
# Sostituisco i valori NA con 0
rFF2[is.na(rFF2)] <- 0

F2 = as.matrix(rFF2)
hist(F2, nclass = 90, xlim = c(-50,50), main = "Istogramma di frequenza dei rendimenti")

Fact = cbind(F[2:3196,],F2)
colnames(Fact) = c("Mkt.RF","SMB","HML","RMW","CMA","ENDEX-TTF-GAS","TRPC-SRMC-Coal",
                "EEX-Phelix", "Crude Oil Spot")
hist(Fact, nclass = 90, xlim = c(-40,40), main = "Istogramma di frequenza dei rendimenti")


######## Stima del modello lineare, salvando parametri stimati e residui #######

T=dim(r)[1]
N=dim(r)[2]
K=dim(Fact)[2]
pars=NULL
resLM=NULL
# la stima del modello viene fatto equazione per equazione, così da salvare
# in una matrice i parametri e i residui (che ci servono per guardare le correlazioni)

for(i in 1:N){
  out=lm(r[,i]~Fact)
  pars=rbind(pars,out$coefficients)
  resLM=cbind(resLM,out$residuals)
}

b = data.frame(pars)

# queste sono le intercette = molto piccole, siamo intorno allo 0.05%
ggplot(b) +
  aes(x = seq(1:51), y = X.Intercept.) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  labs(title = "Intercetta ") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
# medio giornaliero. 
# questi sono i valori del mercato (beta), e sono quelli che ci aspettavamo poiché positivi

# Rappresentazione grafica coefficienti dei stimati per ogni fattore
{
p1 = ggplot(b) +
  aes(x = seq(1:51), y = FactMkt.RF) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  labs(title = "Market ") +
  labs(x = "Titoli", y = "Fattore di Mercato") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p1
# SBM
p2 = ggplot(b) +
 aes(x = seq(1:51), y = FactSMB) +
 geom_point(shape = "circle", size = 1.5, colour = "black") +
 labs(title = "SBM factor") +
 labs(x = "Titoli", y = "Fattore SMB") +
 theme_gray() +
 theme(plot.title = element_text(face = "bold", hjust = 0.5))
p2
# HML
p3 = ggplot(b) +
  aes(x = seq(1:51), y = FactHML) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  labs(title = "HML factor") +
  labs(x = "Titoli", y = "Fattore HML") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p3
# RMW
p4 = ggplot(b) +
  aes(x = seq(1:51), y = FactRMW) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  labs(title = "RMW factor") +
  labs(x = "Titoli", y = "Fattore RMW") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p4
# CMA
p5 = ggplot(b) +
  aes(x = seq(1:51), y = FactCMA) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  labs(title = "CMA factor") +
  labs(x = "Titoli", y = "Fattore CMA") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p5
# ENDEX TTF-GAS"TRPC-SRMC-Coal
p6 = ggplot(b) +
  aes(x = seq(1:51), y = FactENDEX.TTF.GAS) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  labs(title = "ENDEX TTF GAS factor") +
  labs(x = "Titoli", y = "Fattore ENDEX TTF Gas") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p6
# TRPC-SRMC-Coal
p7 = ggplot(b) +
  aes(x = seq(1:51), y = FactTRPC.SRMC.Coal) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  labs(title = "TRPC SRMC Coal factor") +
  labs(x = "Titoli", y = "Fattore TRPC SRMC Coal") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p7
# EEX-Phelix
p8 = ggplot(b) +
  aes(x = seq(1:51), y = FactEEX.Phelix) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  labs(title = "EEX Phelix factor") +
  labs(x = "Titoli", y = "Fattore EEX Phelix") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p8
# Crude Oil Spot
p9 = ggplot(b) +
  aes(x = seq(1:51), y = FactCrude.Oil.Spot) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  labs(title = "Crude Oil Spot factor") +
  labs(x = "Titoli", y = "Fattore Crude Oil Spot") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p9
}
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 3, nrow = 3)

# Volendo si può osservare l'istogramma di frequenza dei residui

hist(resLM, nclass = 200, xlim = c(-10,10), main = "Istogramma di frequenza dei residui",
     xlab = "Residui", ylab = "Frequenza")

# Boxplot dei coefficienti stimati
boxplot(pars[,1:6], main = "Fattori di Fama - French")
boxplot(pars[,7:10], main = "Fattori di mercato")


# Rappresentazione dei prezzi e dei rendimenti dei fattori
{
# Prendo i valori delle date così da poter costruire il grafico
dat = read_excel('STOXX600.xlsx','TotalReturnsIndexes','A5487:A8682',col_names=FALSE,na="NA")
str(dat)
dat = data.frame(dat)
dat = as.Date(dat[,1])

datFF2 <- data.frame(Date = dat, Y = dataFF2)
colnames(datFF2) = c("Data", "Y1", "Y2", "Y3", "Y4")


pf1 = ggplot(datFF2) +
  aes(x = Data, y = Y1) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = "Price",title = "Prezzi del fattore ENDEX TTF GAS") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
pf2 = ggplot(datFF2) +
  aes(x = Data, y = Y2) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = "Price",title = "Prezzi del fattore TRPC SRMC Coal") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
pf3 = ggplot(datFF2) +
  aes(x = Data, y = Y3) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = "Price",title = "Prezzi del fattore EEX Phelix") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
pf4 = ggplot(datFF2) +
  aes(x = Data, y = Y4) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = "Price",title = "Prezzi del fattore Crude Oil Spot") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))


rendFF2 = rFF2
rendFF2 <- data.frame(Date = dat[2:3196], Y = rendFF2)
colnames(rendFF2) = c("Data", "Y1", "Y2", "Y3", "Y4")

rf1 = ggplot(rendFF2) +
  aes(x = Data, y = Y1) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = "Return",title = "Rendimenti del fattore ENDEX TTF GAS") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
rf2 = ggplot(rendFF2) +
  aes(x = Data, y = Y2) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = "Return",title = "Rendimenti del fattore TRPC SRMC Coal") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
rf3 = ggplot(rendFF2) +
  aes(x = Data, y = Y3) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = "Return",title = "Rendimenti del fattore EEX Phelix") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
rf4 = ggplot(rendFF2) +
  aes(x = Data, y = Y4) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = "Return",title = "Rendimenti del fattore Crude Oil Spot") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
}
grid.arrange(pf1,pf2,pf3,pf4, nrow = 2, ncol = 2)
grid.arrange(rf1,rf2,rf3,rf4, nrow = 2, ncol = 2)



# stima dei momenti dei fattori e dei fattori standardizzati
# questo ci servirà quando si andrà ad eliminare l'ipotesi di normalità dei fattori
muF=as.matrix(colMeans(Fact)) # media dei fattori
sigmaF=cov(Fact) # fattori con livelli di varianza differenti, con la varianza del mercato
# (prima riga e prima colonna) è la più alta
P=chol(sigmaF) # standardizzazione mediante Cholesky
# fattori standardizzati che serviranno più avanti per l'ipotesi di non normalità
Zf=Fact-matrix(1,T,1) %*% t(muF) # calcolo Z che ci servirà per effettuare ricampionamenti
Zf=Zf %*% inv(P) 

# pesi portafoglio
w=matrix(1/N,N,1) # portafoglio EW con pesi uguali

# quantile del VaR
a=0.05 # 5%

  
####################### SENZA COMPONENTE IDIOSINCRATICA ########################

# caso 1 - VaR del portafoglio sotto ipotesi di Fattori distribuiti 
# secondo una normale multivariata 
# Posso calcolare media e varianza del portafoglio. Essendo normali anche i rendimenti lo saranno
# calcoliamo il VaR per fattori distribuiti come una normale
muP=t(w) %*% (pars[,1] + pars[,2:10]%*%muF[1:9]) # rendimento del portafoglio
# questa corrisponde alla formula nella slice 33

SigmaP= t(w) %*% (pars[,2:10] %*%  sigmaF[1:9,1:9] %*% t(pars[,2:10])) %*% w
VaRC1=muP + sqrt(SigmaP)*qnorm(a) #VaR ad un giorno
# in un giorno la perdita massima accettabile è -1.53 %. Più elevato è questo valore
# più elevato è il rischio di questo portafolio


########################## COMPONENTE IDIOSINCRATICA ###########################

# Calcolo la matrice di varianza e covarianza dei residui
sigmaE = cov(resLM)
idios = t(w) %*% sigmaE %*% w
VaR_dis=muP + sqrt(SigmaP+idios)*qnorm(a)







