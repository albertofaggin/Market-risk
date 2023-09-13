############################## Carico le librerie ##############################
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

########################## Lettura e pulizia dei dati ##########################
rm(list=ls())
setwd("~/Desktop/Master/Analisi dei dati in finanza/02. Rischio di mercato/Homework")
data=read_excel('STOXX600.xlsx','TotalReturnsIndexes','B7:WC8682',col_names=FALSE,na="NA")
head(data)
T=dim(data)[1]
#data1=as.matrix(data)
data1=data
i=is.na(data1)
idr=rowSums(i)
idc=colSums(i)
data1=data1[,idc==0]
i=is.na(data1)
idr=rowSums(i)
idc=colSums(i)
T=dim(data1)[1]
N=dim(data1)[2]
data1 = data.frame(data1)

r=100*(log(data1[2:T,])-log(data1[1:T-1,]))
r = as.matrix(r)
i0= r==0
i0r=rowSums(i0)
r=r[i0r<300,] # togliamo le festività
r = data.frame(r)

# Prendo i valori delle date così da poter costruire il grafico
dat = read_excel('STOXX600.xlsx','TotalReturnsIndexes','A7:A8682',col_names=FALSE,na="NA")
str(dat)
dat = data.frame(dat)
dat = as.Date(dat[,1])

# seleziono un titolo
Y=r$...374 # titolo numero 374 relativo al titolo Naturgy Energy

# Creo un dataframe con lo scopo di rappresentare graficamente i rendimenti del
# titolo Natargy Energy
df <- data.frame(Date = dat[2:8676], Y = Y)

# Grafico dei rendimenti
{ggplot(df) +
  aes(x = Date, y = Y) +
  geom_line(colour = "#000000") +
  labs(
    x = "Time",
    y = "Redimenti",
    title = "Rendimento del titolo Naturgy Energy"
  ) +
  theme_gray() +
  theme( plot.title = element_text(size = 15L, face = "bold",  hjust = 0.5)
  )
}
# Grafico dei prezzi
{
  price = data$...374
  dp = data.frame(Date = dat, Y = price)  
  ggplot(dp) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(
      x = "Time",
      y = "Prezzi",
      title = "Prezzi del titolo Naturgy Energy"
    ) +
    theme_gray() +
    theme( plot.title = element_text(size = 15L, face = "bold",  hjust = 0.5)
    )
  }


# Creo un oggetto times series al fine d'avere per ogni rendimento il valore 
# temporale relativo al giorno
Y=xts(x=Y,order.by=dat[2:8676])

# Dai rendimenti passiamo alle perdite. I rendimenti erano definiti in punti percentuali
# quindi li riscalo

# costruisco i blocchi data una dimensione degli stessi
l=as.matrix(-Y/100) # vettore delle perdite in punti percentuali
T=dim(l)[1]     # dimensione campionaria
m=21         # lunghezza blocco
N=floor(T/m)
t=N*m
l1=l[1:t]
b=matrix(l1,m,N)
bmax=apply(b,2,max) # prendo il massimo dei blocchi
# Istrograma di frequenza per i massimi a blocchi
hist(bmax, nclass = 60, xlim = c(0,0.17)) # costurisco un istogramma di frequenza
# coda superiore abbastanza lunga, con una stima dei massimi concentrata sulla
# parte iniziale.

# Istogramma di frequenza dei massimi a blocchi
{a = data.frame(bmax)
ggplot(a) +
  aes(x = bmax) +
  geom_histogram(bins = 30L, fill = "#2D3542") +
  labs( x = " ", y = " ",
    title = "Istogramma di frequenza dei massimi a blocchi"
  ) +
  theme_gray() +
  theme( plot.title = element_text(size = 15L, face = "bold",  hjust = 0.5)
  )
}
########################## stima tramite ML della GEV ##########################
library("stats4")

llgev <- function(muM,lnsigM,xiM) {
  sigM=exp(lnsigM)
  z=(bmax-muM)/sigM # sequenza dei massimi standardizzati
  u=1+(1/xiM) 
  if(min(1+xiM*z)>0){
    llt=-log(sigM)-(u)*log(1+xiM*z)-((1+xiM*z)^(-1/xiM))
  } else{
    llt=NA
  }
  -sum(llt)
}

est=stats4::mle(minuslogl=llgev,start=list(muM=0.01,lnsigM=log(0.1),xiM=1))
summary(est)

# standard errors dei parametri
coef(est)/sqrt(diag(est@vcov)) # vcov = matrice di varianza e covarianza

# calcolo residui
c=coef(est)
e=(1+c[3]*((bmax-c[1])/exp(c[2])))^(-1/c[3])

# qqplot
p=0.01*(1:99)
qemp_gev=quantile(e,probs=p) # quant. empirici
qteor_gev=qexp(p,rate=1) # quant. teorici
qqplot(qteor_gev,qemp_gev, xlab = "Quantili teorici", ylab = "Quantili empirici")
qqline(qemp_gev, distribution = function(p) qexp(p,rate=1))

# Grafico QQPlot
{df_gev = data.frame(qteor_gev,qemp_gev)
ggplot(df_gev, aes(x = qteor_gev, y = qemp_gev)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Quantili teorici",y = "Quantili empirici",
    title = "QQ - Plot residui GEV")+
  theme_gray() +
  theme( plot.title = element_text(size = 15L, face = "bold",  hjust = 0.5)
  )
}

# calcolo del VaR
a=0.05  # quantile del VaR (per le perdite) --> coda superiore al 99%
# m dimensione dei blocchi
mu=coef(est)[1]
sig=exp(coef(est)[2])
xi=coef(est)[3]
VaR=mu-(sig/xi)*(1-((-m*log(1-a))^(-xi)))

# calcolo dell'indice estremale
u=quantile(l,1-a) # soglia
Nu=sum(l>u)/T   # percentuale osservazioni sopra soglia
Gu=sum(bmax>u)/length(bmax) # percentuale massimi sopra soglia
th=(1/m)*(log(1-Gu)/log(1-Nu))  # indice estremale

# calcolo del VaR per serie non i.i.d.
VaRe=mu-(sig/xi)*(1-((-m*th*log(1-a))^(-xi)))
# vuol dire che c'è la probabilità dell'1% d'avere una perdita del 2.7%


# Stima del parametro di forma al variare del numero di blocchi
{
m = seq(15,70, 1)
c_xi = NULL
std_xi = NULL
c_mu = NULL
std_mu = NULL
c_ln = NULL
std_ln = NULL
for (i in 1:length(m)) {
  N=floor(T/m[i])
  t=N*m[i]
  l1=l[1:t]
  b=matrix(l1,m[i],N)
  bmax=apply(b,2,max)
  try({est=stats4::mle(minuslogl=llgev,start=list(muM=0.01,lnsigM=log(0.1),xiM=1))})
  std_xi[i] = sqrt(diag(est@vcov))[3]
  c_xi[i] = coef(est)[3]
  std_mu[i] = sqrt(diag(est@vcov))[1]
  c_mu[i] = coef(est)[1]
  std_ln[i] = sqrt(diag(est@vcov))[2]
  c_ln[i] = coef(est)[2]
}

a_xi = c_xi + 1.96*std_xi
b_xi = c_xi - 1.96*std_xi

tsplot(c_xi[4:length(c_xi)], ylim = c(0,0.5))
lines(a_xi[4:56])
lines(b_xi[4:56])

c_xi = data.frame(c_xi, seq(1:length(c_xi)))
colnames(c_xi) = c("Scala", "Time")
form = ggplot(c_xi) +
  aes(x = Time, y = Scala) +
  geom_line(colour = "#000000") +
  labs( x = "Numero di blocchi",y = " ", title = "Parametro di forma") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5)) +
  ylim(0, 0.4)

intervall = data.frame(a_xi,b_xi, seq(1:length(a_xi)))
colnames(intervall) = c("Upper", "Down", "Blocchi")

a1_xi = c_mu + 1.96*std_mu
b1_xi = c_mu - 1.96*std_mu
c_mu = data.frame(c_mu, seq(1:length(c_mu)))
colnames(c_mu) = c("Scala", "Time")
c_mu = c_mu[2:56,]

form1 = 
  ggplot(c_mu) +
  aes(x = Time, y = Scala) +
  geom_line(colour = "#000000") +
  labs( x = "Numero di blocchi",y = " ", title = "Parametro di locazione") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5)) +
  ylim(0.018, 0.038)

intervall1 = data.frame(a1_xi[2:56],b1_xi[2:56], seq(1:length(a1_xi[2:56])))
colnames(intervall1) = c("Upper", "Down", "Blocchi")

a2_xi = c_ln + 1.96*std_ln
b2_xi = c_ln - 1.96*std_ln

tsplot(c_ln[4:length(c_ln)])
lines(a2_xi[4:56])
lines(b2_xi[4:56])

c_ln = data.frame(c_ln, seq(1:length(c_ln)))
colnames(c_ln) = c("Scala", "Time")
c_ln = c_ln[2:56,]

form2 = 
  ggplot(c_ln) +
  aes(x = Time, y = Scala) +
  geom_line(colour = "#000000") +
  labs( x = "Numero di blocchi",y = " ", title = "Parametro di scala") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))

intervall2 = data.frame(a2_xi[2:56],b2_xi[2:56], seq(1:length(a2_xi[2:56])))
colnames(intervall2) = c("Upper", "Down", "Blocchi")

# Parametro di locazione
form1.1 = form1 + geom_line(data = intervall1, aes(x = Blocchi, y = Upper), 
                            colour = "red", linetype="longdash")+
  geom_line(data = intervall1, aes(x = Blocchi, y = Down), 
            colour = "red", linetype="longdash")
# Parametro di scala
form2.1 = form2 + geom_line(data = intervall2, aes(x = Blocchi, y = Upper), 
                            colour = "red", linetype="longdash")+
  geom_line(data = intervall2, aes(x = Blocchi, y = Down), 
            colour = "red", linetype="longdash")


}
# Parametro di forma
form + geom_line(data = intervall, aes(x = Blocchi, y = Upper), 
                 colour = "red", linetype="longdash")+
  geom_line(data = intervall, aes(x = Blocchi, y = Down), 
            colour = "red", linetype="longdash")
# Parametro di locazione e scala
grid.arrange(form1.1,form2.1, nrow = 1, ncol = 2)


########################### Valutazioni sulla GPD ##############################

# fisso la soglia u prendendo un quantile empirico al 99%, oppure posso prenderla
# a livello numerico

# fisso la soglia u
u=quantile(l,probs=0.95)

# perdite sopra soglia
el=l[l>u]-u 

# verifico che la distribuzione non sia esponenziale
p=0.01*(1:99)
qemp=quantile(el,probs=p)
qteor=qexp(p,rate=1)
qqplot(qteor,qemp)
qqline(qemp, distribution = function(p) qexp(p,rate=1))
# Si osserva una piccola deviazione nella parte superiore


# stima tramite ML dei parametri della GPD
llgpd <- function(muM,lnsigM,xiM) {
  sigM=exp(lnsigM)
  rhou=sigM+xiM*(u-muM)
  llt=-log(rhou)-(1+(1/xiM))*log(1+(xiM*el)/rhou)
  -sum(llt)
}

est=stats4::mle(minuslogl=llgpd,start=list(muM=0.001,lnsigM=log(0.2),xiM=0.01))
summary(est)

# mean excess function
ustep=seq(0.01,0.06,0.001) #fisso dei valori per la soglia
mef=NULL
for (i in ustep){
  eloc=l[l>i]-i
  mef=rbind(mef,mean(eloc))    
}
plot(ustep,mef)

# Mean excess plot
{q = cbind(data.frame(ustep), data.frame(mef))
q = q*100
gg =ggplot(q) +
  aes(x = ustep, y = mef) +
  geom_point(shape = "circle", size = 1.5, colour = "#000000") +
  labs(x = "Soglia in %", y = " ", title = "Mean excess plot al variare della soglia") +
  theme_gray() +
  theme(
    plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
gg + geom_smooth(data = q[34:nrow(q), ], aes(x = ustep, y = mef), method = "lm", se = FALSE)
}


# calcolo residui e qq-plot
c=coef(est)
rhou=exp(c[2])+c[3]*(u-c[1])
z=(1/c[3])*log(1+c[3]*el/rhou)
p=0.01*(1:99)
qemp_gpd=quantile(z,probs=p)
qteor_gpd=qexp(p,rate=1)
qqplot(qteor_gpd,qemp_gpd)
qqline(qemp_gpd, distribution = function(p) qexp(p,rate=1))

df_gpd = data.frame(qteor_gpd,qemp_gpd)
ggplot(df_gpd, aes(x = qteor_gpd, y = qemp_gpd)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Quantili teorici",y = "Quantili empirici",
       title = "QQ - Plot residui GPD")+
  theme_gray() +
  theme( plot.title = element_text(size = 15L, face = "bold",  hjust = 0.5)
  )


# calcolo del VaR
a=0.05
VaRGPD=u-(rhou/c[3])*(1-((1-a)*length(el)/length(l))^(-c[3]))

# Stima del parametro di forma al variare della soglia (quantile)
{
  perc = seq(0.55,0.99, 0.01)
  
  coef_xi = NULL
  std_coef_xi = NULL
  VaR_GPD = NULL
  for (i in 1:length(perc)) {
    u=quantile(l,probs=(perc[i]))
    el=l[l>u]-u
    try({est=stats4::mle(minuslogl=llgpd,start=list(muM=0.001,lnsigM=log(0.2),xiM=0.01))})
    c=coef(est)
    coef_xi[i] = coef(est)[3]
    std_coef_xi[i] = sqrt(diag(est@vcov))[3]
    rhou=exp(c[2])+c[3]*(u-c[1])
    VaR_GPD[i]=u-(rhou/c[3])*(1-((1-a)*length(el)/length(l))^(-c[3]))
  }
  # Ottengo dei valori NaN per alcuni valori degli standard error  ??????????
  
  
  d_coef_xi = data.frame(coef_xi, perc)
  quant_form = ggplot(d_coef_xi) +
    aes(x = perc, y = coef_xi) +
    geom_line(colour = "#000000") +
    labs(x = "Quantile",y = " ",
         title = "Stima del parametro di forma al variare della soglia (quantile)") +
    theme_gray() +
    theme(plot.title = element_text(size = 15L,face = "bold",hjust = 0.5))
  quant_form              # HA SENSO ???????????????????????
  
  std_xi_u = coef_xi + 1.96*std_coef_xi
  std_xi_l = coef_xi - 1.96*std_coef_xi
  
  int = data.frame(std_xi_u, std_xi_l, perc)
  colnames(int) = c("Upper", "Down", "Blocchi")
  
  quant_form + geom_line(data = int, aes(x = Blocchi, y = Upper), 
                         colour = "red", linetype="longdash")+
    geom_line(data = int, aes(x = Blocchi, y = Down), 
              colour = "red", linetype="longdash")
}

##################################### GARCH ####################################

# EGARCH Skew-T
spec2 <- ugarchspec(variance.model = list(model="eGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model="sstd")
# fit2 <- ugarchfit(spec2,Y,out.sample = 250)
fit2 <- ugarchfit(spec2,Y)
plot(fit2)
c2=fit2@fit$coef
z2=fit2@fit$z
# for2=ugarchforecast(fit2,n.ahead=1,n.roll=249)
for2=ugarchforecast(fit2,n.ahead=1)
plot(for2)

sigma2for=(t(for2@forecast$sigmaFor))
q2=qdist(distribution="sstd",p=0.01,mu=0,sigma=1,skew=c2[6],shape=c2[7])
VaR2=c2[1]+q2*sigma2for
VaR2


################### Quantile Regression (HAR-type on returns) ##################

# Carico i dati
{library(quantreg)
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
rm(list=ls())
setwd("~/Desktop/Master/Analisi dei dati in finanza/02. Rischio di mercato/Homework")
data=read_excel('STOXX600.xlsx','TotalReturnsIndexes','B7:WC8682',col_names=FALSE,na="NA")
head(data)
T=dim(data)[1]
data1=data
i=is.na(data1)
idr=rowSums(i)
idc=colSums(i)
data1=data1[,idc==0]
i=is.na(data1)
idr=rowSums(i)
idc=colSums(i)
T=dim(data1)[1]
N=dim(data1)[2]
data1 = data.frame(data1)
r=100*(log(data1[2:T,])-log(data1[1:T-1,]))
r = as.matrix(r)
i0= r==0
i0r=rowSums(i0)
r=r[i0r<300,] # togliamo le festività
r = data.frame(r)
dat = read_excel('STOXX600.xlsx','TotalReturnsIndexes','A7:A8682',col_names=FALSE,na="NA")
str(dat)
dat = data.frame(dat)
dat = as.Date(dat[,1])
Y=r$...374
df <- data.frame(Date = dat[2:8676], Y = Y)
Y=xts(x=Y,order.by=dat[2:8676])
}

########## Stima QR con dipendenza del rendimento ritardato 

T=length(Y)

multi_sup1 <- rq(Y[2:(T)] ~ Y[1:(T-1)], tau = c(0.90,0.95,0.99))
multi_sup1

multi_inf1 <- rq(Y[2:(T)] ~ Y[1:(T-1)], tau = c(0.01,0.05,0.10))
multi_inf1

y_rend1 = data.frame(Y[2:T], Y[1:(T-1)])
colnames(y_rend1) = c("Rend_T", "Rend_T_1")

# STIMA MODELLO LINEARE
mod = lm(Y[2:(T)]~Y[1:(T-1)])
summary(mod)

# Grafico QR vs LM
{y_plot = ggplot(y_rend1) +
  aes(x = Rend_T_1, y = Rend_T) +
  geom_point(shape = "circle", size = 1.5, colour = "#000000") +
  labs(x = "Naturgy Energy (t-1)", y = "Naturgy Energy (t)",
       title = "Confronto LM e QR con dipendenza del rendimento ritardato ") +
  theme_gray()

y_plot + geom_abline(intercept = coef(multi_sup1)[1,1] ,                             
                     slope = coef(multi_sup1)[2,1], color="red",
                     linetype="dashed", size=1)+ 
          geom_abline(intercept = coef(multi_sup1)[1,2] ,
                     slope = coef(multi_sup1)[2,2], color="red",
                     linetype="dashed", size=1)+ 
          geom_abline(intercept = coef(multi_sup1)[1,3] ,
                     slope = coef(multi_sup1)[2,3], color="red",
                     linetype="dashed", size=1) +
          geom_abline(intercept = coef(multi_inf1)[1,1] ,                             
                     slope = coef(multi_inf1)[2,1], color="red",
                     linetype="dashed", size=1)+ 
          geom_abline(intercept = coef(multi_inf1)[1,2] ,
                     slope = coef(multi_inf1)[2,2], color="red",
                     linetype="dashed", size=1)+ 
          geom_abline(intercept = coef(multi_inf1)[1,3] ,
                    slope = coef(multi_inf1)[2,3], color="red",
                    linetype="dashed", size=1) +
          geom_abline(intercept = coef(mod)[1] , 
                           slope = coef(mod)[2], color="blue", 
                           linetype="dashed", size=1)}

multi_qr1 = rq(Y[2:(T)] ~ Y[1:(T-1)], tau = seq(0.01,0.99,0.01))

# Plot valori intercetta
{int_qr1 = NULL
  for (j in 1:99) {
    int_qr1[j] = multi_qr1$coefficients[1,j]  
  }
  
  int_qr1 = data.frame(int_qr1,seq(0.01,0.99,0.01))
  int_plot2 = ggplot(int_qr1) +
    aes(x = seq.0.01..0.99..0.01., y = int_qr1) +
    geom_line(colour = "#000000", size = 1) +
    labs(x = "Quantili", y = " ", title = "Intercetta") +
    theme_gray() +
    theme(
      plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
}
int_plot2 + geom_hline(yintercept = mod$coefficients[1], 
                       col = "blue",linetype="dashed", size = 1)
# Plot valori Lag1
{Lag1.1 = NULL
  for (j in 1:99) {
    Lag1.1[j] = multi_qr$coefficients[2,j]  
  }
  
  Lag1.1 = data.frame(Lag1.1,seq(0.01,0.99,0.01))
  plot_lag1.1 = ggplot(Lag1.1) +
    aes(x = seq.0.01..0.99..0.01., y = Lag1.1) +
    geom_line(colour = "#000000", size = 1) +
    labs(x = "Quantili", y = " ", title = "Lag 1") +
    theme_gray() +
    theme(
      plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))}
plot_lag1.1 + geom_hline(yintercept = mod$coefficients[2], col = "blue", linetype="dashed",
           size = 1)

# Stima del VaR
rqfit1 <- rq(Y[2:(T)] ~ Y[1:(T-1)], tau = 0.05)
VaR_qr1 = coef(rqfit1)[1]+coef(rqfit1)[2]*Y[1:(T-1)]
plot(VaR_qr1, type = "l")
# Grafico del VaR
{var_qr1 = data.frame(Date = dat[3:8676], Y = VaR_qr1)
ggplot(var_qr1) +
  aes(x = Date, y = Y) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = " ",title = "Calcolo del VaR - QR dipendenza rendimento ritardato") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
}
########## Stima QR con dipendenza del rendimento ritardato e del quadrato

Y=r$...374 # titolo numero 374 relativo al titolo Naturgy Energy

r_2 =(Y[1:(T-1)])^2
multi_sup <- rq(Y[2:(T)] ~ Y[1:(T-1)] + r_2, tau = c(0.90,0.95,0.99))
multi_sup

multi_inf <- rq(Y[2:(T)] ~ Y[1:(T-1)] + r_2, tau = c(0.01,0.05,0.10))
multi_inf

y_rend = data.frame(Y[2:T], Y[1:(T-1)])
colnames(y_rend) = c("Rend_T", "Rend_T_1")

# Grafico QR vs LM
{  y_plot = ggplot(y_rend) +
  aes(x = Rend_T_1, y = Rend_T) +
  geom_point(shape = "circle", size = 1.5, colour = "#000000") +
  labs(x = "Naturgy Energy (t-1)", y = "Naturgy Energy (t)",
       title = "Confronto ML e QR con dipendenza del rendimento ritardato e del quadrato") +
  theme_gray()

a = coef(multi_sup)[3,3]
b = coef(multi_sup)[2,3]
c = coef(multi_sup)[1,3]
f = function(x) {
  a*x^2 + b*x + c
}

a1 = coef(multi_sup)[3,2]
b1 = coef(multi_sup)[2,2]
c1 = coef(multi_sup)[1,2]
f1 = function(x) {
  a1*x^2 + b1*x + c1
}

a2 = coef(multi_sup)[3,1]
b2 = coef(multi_sup)[2,1]
c2 = coef(multi_sup)[1,1]
f2 = function(x) {
  a2*x^2 + b2*x + c2
}

a3 = coef(multi_inf)[3,3]
b3 = coef(multi_inf)[2,3]
c3 = coef(multi_inf)[1,3]
f3 = function(x) {
  a3*x^2 + b3*x + c3
}

a4 = coef(multi_inf)[3,2]
b4 = coef(multi_inf)[2,2]
c4 = coef(multi_inf)[1,2]
f4 = function(x) {
  a4*x^2 + b4*x + c4
}

a5 = coef(multi_inf)[3,1]
b5 = coef(multi_inf)[2,1]
c5 = coef(multi_inf)[1,1]
f5 = function(x) {
  a5*x^2 + b5*x + c5
}

mod2 = lm(Y[2:(T)] ~ Y[1:(T-1)] + r_2)

}

y_plot +
  geom_function(fun = f, size = 0.5, colour = "red",linetype="dashed") +
  geom_function(fun = f1, size = 0.5, colour = "red",linetype="dashed") +
  geom_function(fun = f2, size = 0.5, colour = "red",linetype="dashed") +
  geom_function(fun = f3, size = 0.5, colour = "red",linetype="dashed") +
  geom_function(fun = f4, size = 0.5, colour = "red",linetype="dashed") +
  geom_function(fun = f5, size = 0.5, colour = "red",linetype="dashed") +
  geom_abline(intercept = coef(mod2)[1] , 
              slope = coef(mod2)[2], color="blue", 
              linetype="dashed", size=1) +
  ylim(-10, 10)


multi_qr = rq(Y[2:(T)] ~ Y[1:(T-1)] + r_2, tau = seq(0.01,0.99,0.01))

# Plot valori intercetta
{int_qr = NULL
for (j in 1:99) {
  int_qr[j] = multi_qr$coefficients[1,j]  
}

int_qr = data.frame(int_qr,seq(0.01,0.99,0.01))
  int_plot1 = ggplot(int_qr) +
  aes(x = seq.0.01..0.99..0.01., y = int_qr) +
  geom_line(colour = "#000000", size = 1) +
  labs(x = "Quantili", y = " ", title = "Intercetta") +
  theme_gray() +
  theme(
    plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
}
int_plot1 + geom_hline(yintercept = mod2$coefficients[1], col = "blue", linetype="dashed",
                       size = 1)
# Plot valori Lag1
{Lag1 = NULL
for (j in 1:99) {
  Lag1[j] = multi_qr$coefficients[2,j]  
  }

Lag1 = data.frame(Lag1,seq(0.01,0.99,0.01))
plot_lag1_2 = ggplot(Lag1) +
  aes(x = seq.0.01..0.99..0.01., y = Lag1) +
  geom_line(colour = "#000000", size = 1) +
  labs(x = "Quantili", y = " ", title = "Lag 1") +
  theme_gray() +
  theme(
    plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
}
plot_lag1_2 + geom_hline(yintercept = mod2$coefficients[2], col = "blue", linetype="dashed",
                         size = 1)
# Plot valori Lag1_2
{Lag1_2 = NULL
for (j in 1:99) {
  Lag1_2[j] = multi_qr$coefficients[3,j]  
}

Lag1_2 = data.frame(Lag1_2,seq(0.01,0.99,0.01))
plot_lag2_2 = ggplot(Lag1_2) +
  aes(x = seq.0.01..0.99..0.01., y = Lag1_2) +
  geom_line(colour = "#000000", size = 1) +
  labs(x = "Quantili", y = " ", title = "Lag 1 squared") +
  theme_gray() +
  theme(
    plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))

}
plot_lag2_2 + geom_hline(yintercept = mod2$coefficients[3], col = "blue", linetype="dashed",
                         size = 1)

# Stima del VaR
rqfit <- rq(Y[2:(T)] ~ Y[1:(T-1)] + r_2, tau = 0.95)
VaR_qr = coef(rqfit)[1]+coef(rqfit)[2]*Y[1:(T-1)]+coef(rqfit)[3]*r_2
plot(VaR_qr, type = "l")
# Grafico del VaR
{var_qr2 = data.frame(Date = dat[3:8676], Y = VaR_qr)
  ggplot(var_qr2) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time",y = " ",title = "Calcolo del VaR - QR dipendenza rendimento ritardato e al quadrato") +
    theme_gray() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

########## Stima mediante QR con un fattore

# Lettura dei fattori
{
  dataFF = read.csv("Fama_Factors.csv", sep = ";")
  str(dataFF)
  F=as.matrix(dataFF[,-c(1,7)]) # elimino la prima colonna relativa al periodo
  Fact = F[,1]
}
# Creo la matrice dei rendimenti del titolo naturgy energy
{data_qr=read_excel('STOXX600.xlsx','TotalReturnsIndexes','B137:WC8682',col_names=FALSE,na="NA")
head(data_qr)
T=dim(data_qr)[1]
#data1=as.matrix(data)
data1_qr=data_qr
i_qr=is.na(data1_qr)
idr_qr=rowSums(i_qr)
idc_qr=colSums(i_qr)
data1_qr=data1_qr[,idc_qr==0]
i_qr=is.na(data1_qr)
idr_qr=rowSums(i_qr)
idc_qr=colSums(i_qr)
T_qr=dim(data1_qr)[1]
N_qr=dim(data1_qr)[2]
data1_qr = data.frame(data1_qr)

r_qr=100*(log(data1_qr[2:T_qr,])-log(data1_qr[1:T_qr-1,]))
r_qr = data.frame(r_qr)

# Prendo i valori delle date così da poter costruire il grafico
dat_qr = read_excel('STOXX600.xlsx','TotalReturnsIndexes','A137:A8682',col_names=FALSE,na="NA")
str(dat_qr)
dat_qr = data.frame(dat_qr)
dat_qr = as.Date(dat_qr[,1])

# seleziono un titolo
Y_qr=r_qr$...374 # titolo numero 374 relativo al titolo Naturgy Energy
}
df_qr <- data.frame(Date = dat_qr[2:8546], Y = Y_qr)

# STIMA MODELLO LINEARE
mod1 = lm(Y_qr~Fact)
summary(mod1)

# STIMA QR - tau = c(0.90,0.95,0.99)
multi_rqfit_sup <- rq(Y_qr ~ Fact, tau = c(0.90,0.95,0.99))
multi_rqfit_sup

# STIMA QR - tau = c(0.01,0.05,0.10)
multi_rqfit_inf <- rq(Y_qr ~ Fact, tau = c(0.01,0.05,0.10))
multi_rqfit_inf

# Grafico quantile regression vs linear model
{plot_qr = data.frame(Y_qr, Fact)
  
  fact_vs_rend = ggplot(plot_qr) +
    aes(x = Fact, y = Y_qr) +
    geom_point(shape = "circle", size = 1.5, colour = "#000000") +
    labs(x = "Fattore di mercato", y = "Titolo Naturgy Energy",
         title = "Confronto LM e QR con un fattore") +
    theme_gray()
  
  fact_vs_rend + geom_abline(intercept = coef(multi_rqfit_sup)[1,1] , 
                             slope = coef(multi_rqfit_sup)[2,1], color="red", 
                             linetype="dashed", size=1) + 
              geom_abline(intercept = coef(multi_rqfit_sup)[1,2] , 
              slope = coef(multi_rqfit_sup)[2,2], color="red", 
              linetype="dashed", size=1) +
              geom_abline(intercept = coef(multi_rqfit_sup)[1,3] , 
              slope = coef(multi_rqfit_sup)[2,3], color="red", 
              linetype="dashed", size=1) + 
              geom_abline(intercept = coef(multi_rqfit_inf)[1,1] , 
              slope = coef(multi_rqfit_inf)[2,1], color="red", 
              linetype="dashed", size=1) + 
              geom_abline(intercept = coef(multi_rqfit_inf)[1,2] , 
              slope = coef(multi_rqfit_inf)[2,2], color="red", 
              linetype="dashed", size=1) +
              geom_abline(intercept = coef(multi_rqfit_inf)[1,3] , 
              slope = coef(multi_rqfit_inf)[2,3], color="red", 
              linetype="dashed", size=1)+ 
              geom_abline(intercept = coef(mod1)[1] , 
              slope = coef(mod1)[2], color="blue", 
              linetype="dashed", size=1)
}
# Grafico CONFRONTO INTERCETA quantile regression vs linear model
{
multi_rq <- rq(Y_qr ~ Fact, tau = seq(0.01,0.99,0.01))

int = NULL
for (j in 1:99) {
  int[j] = multi_rq$coefficients[1,j]  
}

int = data.frame(int,seq(0.01,0.99,0.01))
int_plot = ggplot(int) +
  aes(x = seq.0.01..0.99..0.01., y = int) +
  geom_line(colour = "#000000", size = 1) +
  labs(x = "Quantili", y = " ", title = "Intercetta") +
  theme_gray() +
  theme(
    plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))}
int_plot + geom_hline(yintercept = mod1$coefficients[1], col = "blue", linetype="dashed",
                      size = 1)
# Grafico CONFRONTO COEFFICIENTE quantile regression vs linear model
{coef_F = NULL
for (j in 1:99) {
  coef_F[j] = multi_rq$coefficients[2,j]  
}

coef_F = data.frame(coef_F,seq(0.01,0.99,0.01))
coef_F_plot = ggplot(coef_F) +
  aes(x = seq.0.01..0.99..0.01., y = coef_F) +
  geom_line(colour = "#000000", size = 1) +
  labs(x = "Quantili", y = " ", title = "Coefficiente") +
  theme_gray() +
  theme(
    plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))}
coef_F_plot + geom_hline(yintercept = mod1$coefficients[2], col = "blue", linetype="dashed",
                      size = 1)

# Stima del VaR
rqfit_F <- rq(Y_qr ~ Fact, tau = 0.95)
VaR_qr_F = coef(rqfit_F)[1]+coef(rqfit_F)[2]*Fact
plot(VaR_qr_F, type = "l")
# Grafico del VaR
{var_qr3 = data.frame(Date = dat[1:8545], Y = VaR_qr_F)
  ggplot(var_qr3) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time",y = " ",title = "Calcolo del VaR - QR dipendenza di un fattore") +
    theme_gray() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}
################################### CONFRONTO ##################################


# Carico e leggo i dati fino al 2022
{
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
  library("quantreg")
  library("gridExtra")
  
rm(list=ls())
data=read_excel('STOXX600.xlsx','TotalReturnsIndexes','B7:WC8617',col_names=FALSE,na="NA")
head(data)
T=dim(data)[1]
data1=data
i=is.na(data1)
idr=rowSums(i)
idc=colSums(i)
data1=data1[,idc==0]
i=is.na(data1)
idr=rowSums(i)
idc=colSums(i)
T=dim(data1)[1]
N=dim(data1)[2]
data1 = data.frame(data1)

r=100*(log(data1[2:T,])-log(data1[1:T-1,]))
r = as.matrix(r)
i0= r==0
i0r=rowSums(i0)
r=r[i0r<300,] # togliamo le festività
r = data.frame(r)
dat = read_excel('STOXX600.xlsx','TotalReturnsIndexes','A7:A8617',col_names=FALSE,na="NA")
str(dat)
dat = data.frame(dat)
dat = as.Date(dat[,1])
Y=r$...374
df <- data.frame(Date = dat[2:8611], Y = Y)
Y=xts(x=Y,order.by=dat[2:8611])
}

spec2 <- ugarchspec(variance.model = list(model="eGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model="sstd")
fit2 <- ugarchfit(spec2,Y,out.sample = 250)
#plot(fit2)
c2=fit2@fit$coef
z2=fit2@fit$z
for2=ugarchforecast(fit2,n.ahead=1,n.roll=249)
#plot(for2)

sigma2for=(t(for2@forecast$sigmaFor))
q2=qdist(distribution="sstd",p=0.01,mu=0,sigma=1,skew=c2[6],shape=c2[7])
VaR1=c2[1]+q2*sigma2for
VaR1

# GARCH + empirical quantile of innovations (first sample)
VaR2=c2[1]+quantile(z2,0.01)*sigma2for

# GARCH + Cornish-Fisher on innovations
sk2=skewness(z2)
kt2=kurtosis(z2)
q1=qdist(distribution="norm",p=0.01,mu=0,sigma=1)
q2CF=q1+(sk2[1]/6)*((q1^2)-1)+((kt2[1]-3)/24)*((q1^3)-3)-((sk2[1]^2)/36)*q1*(2*q1*q1-5)
VaR3=c2[1]+q2CF*sigma2for


# Rappresentazione grafica del VaR modelli GARCH
{
  d_VaR1 = data.frame(Date = dat[8361:8610], Y = VaR1[,1])
  p_Var1 = ggplot(d_VaR1) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time", y = " ", title = "Stima del VaR modello EGARCH") +
    theme_gray()
  d_VaR2 = data.frame(Date = dat[8361:8610], Y = VaR2[,1])
  p_Var2 = ggplot(d_VaR2) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time", y = " ", title = "Stima del VaR modello EGARCH con quantile empirico") +
    theme_gray()
  d_VaR3 = data.frame(Date = dat[8361:8610], Y = VaR3[,1])
  p_Var3 = ggplot(d_VaR3) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time", y = " ", title = "Stima del VaR modello EGARCH con approssimazione di Cornish - Fisher") +
    theme_gray()
  library(gridExtra)
}
grid.arrange(p_Var1,p_Var2,p_Var3, ncol = 3, nrow = 1)


# Quantile regression

Y=r$...374
T = length(Y)
VaR4 = NULL
for (i in 0:250) {
  out=rq(Y[(2+i):(T-250+i)]~Y[(1+i):(T-251+i)]+abs(Y[(1+i):(T-251+i)]),tau=0.01)
  c=coefficients(out)
  VaR4[i]=c[1]+c[2]*Y[(T-250-i)]+c[3]*abs(Y[(T-250+i)])
}

df <- data.frame(Date = dat[2:8611], Y = Y)
Y=xts(x=Y,order.by=dat[2:8611])
T = length(Y)
VaR5 = NULL
for (i in 0:250) {
  out1=rq(Y[(2+i):(T-250+i)]~Y[(1+i):(T-251+i)],tau=0.01)
  c1=coefficients(out1)
  VaR5[i]=c1[1]+c1[2]*Y[(T-250+i)]  
}

plot.ts(VaR5)

# Rappresentazione grafica VaR
{
  VaR4 = VaR4[-c(251)]
  VaR5 = VaR5[-c(251)]
d_VaR4 = data.frame(Date = dat[8361:8610], Y = VaR4)
p_Var4 = ggplot(d_VaR4) +
  aes(x = Date, y = Y) +
  geom_line(colour = "#000000") +
  labs(x = "Time", y = " ", title = "Stima del VaR modello QR - abs(return)") +
  theme_gray()
d_VaR5 = data.frame(Date = dat[8361:8610], Y = VaR5)
p_Var5 = ggplot(d_VaR5) +
  aes(x = Date, y = Y) +
  geom_line(colour = "#000000") +
  labs(x = "Time", y = " ", title = "Stima del VaR modello QR - return") +
  theme_gray()
}
grid.arrange(p_Var4,p_Var5, ncol = 2, nrow = 1)


# eccezioni al VaR

T=length(Y)
YF=Y[(T-249):T]

AllVaR=cbind(VaR1,VaR2,VaR3,VaR4)
e1=YF<VaR1
e2=YF<VaR2
e3=YF<VaR3
e4=YF<VaR4
e5=YF<VaR5
E=cbind(e1,e2,e3,e4, e5)
colSums(E)


# confronto con test Kupiec Christoffersen
phat=colSums(E)/250;
k=colSums(E)  # eccezioni al VaR
LRK=2*log(((1-phat)^(250-k))*(phat^k))-2*log(((1-0.01)^(250-k))*(0.01^k))
1-pchisq(LRK,1)

# confronto con Loss functions
# Lopez loss
LFLopez1=colSums((YF<VaR1)*(1+(YF-VaR1)^2))
LFLopez2=colSums((YF<VaR2)*(1+(YF-VaR2)^2))
LFLopez3=colSums((YF<VaR3)*(1+(YF-VaR3)^2))
LFLopez4=colSums((YF<VaR4)*(1+(YF-VaR4)^2))
LFLopez5=colSums((YF<VaR5)*(1+(YF-VaR5)^2))

LFAbs1=colSums((YF<0)*(abs(abs(YF)-abs(VaR1))))
LFAbs2=colSums((YF<0)*(abs(abs(YF)-abs(VaR2))))
LFAbs3=colSums((YF<0)*(abs(abs(YF)-abs(VaR3))))
LFAbs4=colSums((YF<0)*(abs(abs(YF)-abs(VaR4))))
LFAbs5=colSums((YF<0)*(abs(abs(YF)-abs(VaR5))))

loss1 = cbind(LFLopez1,LFLopez2,LFLopez3,LFLopez4,LFLopez5)
loss2 = cbind(LFAbs1,LFAbs2,LFAbs3,LFAbs4,LFAbs5)


df <- data.frame(Date = dat[2:8611], Y = Y)
Y=xts(x=Y,order.by=dat[2:8611])

df = df[c(8361:8610),]
plot_df = ggplot(df) +
  aes(x = Date, y = Y) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = " ",title = "Rendimento titolo Naturgy Energy + VaR 1%") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

conf = plot_df + geom_line(data = d_VaR1, aes(x = Date, y = Y), 
                    colour = "red", linetype="longdash")

conf1 = plot_df + geom_line(data = d_VaR2, aes(x = Date, y = Y), 
                    colour = "red", linetype="longdash")

conf2 = plot_df + geom_line(data = d_VaR3, aes(x = Date, y = Y), 
                    colour = "red", linetype="longdash")
grid.arrange(conf,conf1,conf2, ncol = 3, nrow = 1)


conf3 = plot_df + geom_line(data = d_VaR4, aes(x = Date, y = Y), 
                            colour = "red", linetype="longdash")

conf4 = plot_df + geom_line(data = d_VaR5, aes(x = Date, y = Y), 
                            colour = "red", linetype="longdash")

grid.arrange(conf3,conf4, ncol = 2, nrow = 1)

############# Carico e leggo i dati fino al 2021
{
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
  library("quantreg")
  library("gridExtra")
  
  rm(list=ls())
  data=read_excel('STOXX600.xlsx','TotalReturnsIndexes','B7:WC8357',col_names=FALSE,na="NA")
  head(data)
  T=dim(data)[1]
  data1=data
  i=is.na(data1)
  idr=rowSums(i)
  idc=colSums(i)
  data1=data1[,idc==0]
  i=is.na(data1)
  idr=rowSums(i)
  idc=colSums(i)
  T=dim(data1)[1]
  N=dim(data1)[2]
  data1 = data.frame(data1)
  
  r=100*(log(data1[2:T,])-log(data1[1:T-1,]))
  r = as.matrix(r)
  i0= r==0
  i0r=rowSums(i0)
  r=r[i0r<300,] # togliamo le festività
  r = data.frame(r)
  dat = read_excel('STOXX600.xlsx','TotalReturnsIndexes','A7:A8357',col_names=FALSE,na="NA")
  str(dat)
  dat = data.frame(dat)
  dat = as.Date(dat[,1])
  Y=r$...374
  df <- data.frame(Date = dat[2:8351], Y = Y)
  Y=xts(x=Y,order.by=dat[2:8351])
}

spec2 <- ugarchspec(variance.model = list(model="eGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model="sstd")
fit2 <- ugarchfit(spec2,Y,out.sample = 250)
#plot(fit2)
c2=fit2@fit$coef
z2=fit2@fit$z
for2=ugarchforecast(fit2,n.ahead=1,n.roll=249)
#plot(for2)

sigma2for=(t(for2@forecast$sigmaFor))
q2=qdist(distribution="sstd",p=0.01,mu=0,sigma=1,skew=c2[6],shape=c2[7])
VaR1=c2[1]+q2*sigma2for
VaR1

# GARCH + empirical quantile of innovations (first sample)
VaR2=c2[1]+quantile(z2,0.01)*sigma2for

# GARCH + Cornish-Fisher on innovations
sk2=skewness(z2)
kt2=kurtosis(z2)
q1=qdist(distribution="norm",p=0.01,mu=0,sigma=1)
q2CF=q1+(sk2[1]/6)*((q1^2)-1)+((kt2[1]-3)/24)*((q1^3)-3)-((sk2[1]^2)/36)*q1*(2*q1*q1-5)
VaR3=c2[1]+q2CF*sigma2for

# Rappresentazione grafica del VaR modelli GARCH
{
  d_VaR1 = data.frame(Date = dat[8102:8351], Y = VaR1[,1])
  p_Var1 = ggplot(d_VaR1) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time", y = " ", title = "Stima del VaR modello EGARCH") +
    theme_gray()
  d_VaR2 = data.frame(Date = dat[8102:8351], Y = VaR2[,1])
  p_Var2 = ggplot(d_VaR2) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time", y = " ", title = "Stima del VaR modello EGARCH con quantile empirico") +
    theme_gray()
  d_VaR3 = data.frame(Date = dat[8102:8351], Y = VaR3[,1])
  p_Var3 = ggplot(d_VaR3) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time", y = " ", title = "Stima del VaR modello EGARCH con approssimazione di Cornish - Fisher") +
    theme_gray()
  library(gridExtra)
}
grid.arrange(p_Var1,p_Var2,p_Var3, ncol = 3, nrow = 1)

# Quantile regression 
library(quantreg)

Y=r$...374
T = length(Y)
VaR4 = NULL
for (i in 0:250) {
  out=rq(Y[(2+i):(T-250+i)]~Y[(1+i):(T-251+i)]+abs(Y[(1+i):(T-251+i)]),tau=0.01)
  c=coefficients(out)
  VaR4[i]=c[1]+c[2]*Y[(T-250-i)]+c[3]*abs(Y[(T-250+i)])
}

df <- data.frame(Date = dat[2:8351], Y = Y)
Y=xts(x=Y,order.by=dat[2:8351])
T = length(Y)
VaR5 = NULL
for (i in 0:250) {
  out1=rq(Y[(2+i):(T-250+i)]~Y[(1+i):(T-251+i)],tau=0.01)
  c1=coefficients(out1)
  VaR5[i]=c1[1]+c1[2]*Y[(T-250+i)]  
}


# Rappresentazione grafica VaR
{
  VaR4 = VaR4[-c(251)]
  VaR5 = VaR5[-c(251)]
  d_VaR4 = data.frame(Date = dat[8102:8351], Y = VaR4)
  p_Var4 = ggplot(d_VaR4) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time", y = " ", title = "Stima del VaR modello QR - abs(return)") +
    theme_gray()
  d_VaR5 = data.frame(Date = dat[8102:8351], Y = VaR5)
  p_Var5 = ggplot(d_VaR5) +
    aes(x = Date, y = Y) +
    geom_line(colour = "#000000") +
    labs(x = "Time", y = " ", title = "Stima del VaR modello QR - return") +
    theme_gray()
}
grid.arrange(p_Var4,p_Var5, ncol = 2, nrow = 1)

# eccezioni al VaR

T=length(Y)
YF=Y[(T-249):T]

AllVaR=cbind(VaR1,VaR2,VaR3,VaR4, VaR5)
e1=YF<VaR1
e2=YF<VaR2
e3=YF<VaR3
e4=YF<VaR4
e5=YF<VaR5
E=cbind(e1,e2,e3,e4, e5)
colSums(E)


# confronto con test Kupiec Christoffersen
phat=colSums(E)/250;
k=colSums(E)  # eccezioni al VaR
LRK=2*log(((1-phat)^(250-k))*(phat^k))-2*log(((1-0.01)^(250-k))*(0.01^k))
1-pchisq(LRK,1)

# confronto con Loss functions
# Lopez loss
LFLopez1=colSums((YF<VaR1)*(1+(YF-VaR1)^2))
LFLopez2=colSums((YF<VaR2)*(1+(YF-VaR2)^2))
LFLopez3=colSums((YF<VaR3)*(1+(YF-VaR3)^2))
LFLopez4=colSums((YF<VaR4)*(1+(YF-VaR4)^2))
LFLopez5=colSums((YF<VaR5)*(1+(YF-VaR5)^2))


LFAbs1=colSums((YF<0)*(abs(abs(YF)-abs(VaR1))))
LFAbs2=colSums((YF<0)*(abs(abs(YF)-abs(VaR2))))
LFAbs3=colSums((YF<0)*(abs(abs(YF)-abs(VaR3))))
LFAbs4=colSums((YF<0)*(abs(abs(YF)-abs(VaR4))))
LFAbs5=colSums((YF<0)*(abs(abs(YF)-abs(VaR5))))


loss1 = cbind(LFLopez1,LFLopez2,LFLopez3,LFLopez4,LFLopez5)
loss2 = cbind(LFAbs1,LFAbs2,LFAbs3,LFAbs4,LFAbs5)


df <- data.frame(Date = dat[2:8351], Y = Y)
Y=xts(x=Y,order.by=dat[2:8351])

df = df[c(8101:8610),]
plot_df = ggplot(df) +
  aes(x = Date, y = Y) +
  geom_line(colour = "#000000") +
  labs(x = "Time",y = " ",title = "Rendimento titolo Naturgy Energy + VaR 1%") +
  theme_gray() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

conf = plot_df + geom_line(data = d_VaR1, aes(x = Date, y = Y), 
                           colour = "red", linetype="longdash")

conf1 = plot_df + geom_line(data = d_VaR2, aes(x = Date, y = Y), 
                            colour = "red", linetype="longdash")

conf2 = plot_df + geom_line(data = d_VaR3, aes(x = Date, y = Y), 
                            colour = "red", linetype="longdash")
grid.arrange(conf,conf1,conf2, ncol = 3, nrow = 1)


conf3 = plot_df + geom_line(data = d_VaR4, aes(x = Date, y = Y), 
                            colour = "red", linetype="longdash")

conf4 = plot_df + geom_line(data = d_VaR5, aes(x = Date, y = Y), 
                            colour = "red", linetype="longdash")

grid.arrange(conf3,conf4, ncol = 2, nrow = 1)











