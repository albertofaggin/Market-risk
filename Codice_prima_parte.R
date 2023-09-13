############################### Loading the libraries ################# #############
rm(list=ls())
library("quantmod")
library("astsa")
libraries("readxl")
library("tidyverse")
library("fImport")
libraries("fBasics")
library("rugarch")
library("highfrequency")
library("xts")
library("zoo")
library("chron")
library("gridExtra")

########################## Reading and cleaning data #################### #######
rm(list=ls())
setwd("~/Desktop/Master/Data Analytics in Finance/02. Market Risk/Homework")
data=read_excel('STOXX600-2.xlsx','TotalReturnsIndexes','B7:AZ3202',col_names=FALSE,na="NA")
T=dim(data)[1]
#data1=as.matrix(data)
data1=date
i=is.na(data1)
idr=rowSums(i)
idc=colSums(i)
plot(idc) # the presence of missing is associated with non-null values. It is observed that
# they care about growing, so it's the smaller businesses that are new
# enter the index
data1=data1[,idc==0]
i=is.na(data1)
idr=rowSums(i)
plot(idr) # there is no longer any missing data
idc=colSums(i)
T=dim(data1)[1]
N=dim(data1)[2]
data1 = data.frame(data1)

r=100*(log(data1[2:T,])-log(data1[1:T-1,]))
r = as.matrix(r)
i0= r==0
i0r=rowSums(i0)
plot(i0r) #I can observe the frequency of zeros

r=r[i0r<300,]# I take the rows where this indicator is less than 300


dataFF = read.csv("Factors_Fama.csv", sep = ";")
str(dataFF)
View(dataFF)
F=as.matrix(dataFF[,-c(1,7)]) # I delete the first column relating to the period

hist(F, nclass = 90, xlim = c(-2.5,2.5), main = "Frequency histogram of returns")

dataFF2 = read_excel('Factors_Futures.xlsx','Sheet1','B2:E3197',col_names=FALSE,na="NA")
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
# I replace NA values with 0
rFF2[is.na(rFF2)] <- 0

F2 = as.matrix(rFF2)
hist(F2, nclass = 90, xlim = c(-50,50), main = "Frequency histogram of returns")

Fact = cbind(F[2:3196,],F2)
colnames(Fact) = c("Mkt.RF","SMB","HML","RMW","CMA","ENDEX-TTF-GAS","TRPC-SRMC-Coal",
                 "EEX-Phelix", "Crude Oil Spot")
hist(Fact, nclass = 90, xlim = c(-40,40), main = "Frequency histogram of returns")


######## Linear model estimation, saving estimated parameters and residuals #######

T=dim(r)[1]
N=dim(r)[2]
K=dim(Fact)[2]
pars=NULL
resLM=NULL
# the model estimation is done equation by equation, so as to save
# in a matrix the parameters and residuals (which we need to look at the correlations)

for(i in 1:N){
   out=lm(r[,i]~Fact)
   pars=rbind(pars,out$coefficients)
   resLM=cbind(resLM,out$residuals)
}

b = data.frame(pars)

# these are the intercepts = very small, we are around 0.05%
ggplot(b) +
   aes(x = seq(1:51), y = X.Intercept.) +
   geom_point(shape = "circle", size = 1.5, color = "black") +
   labs(title = "Intercept ") +
   theme_gray() +
   theme(plot.title = element_text(face = "bold", hjust = 0.5))
# daily average.
# these are the market values (beta), and they are what we expected since they were positive

# Graphical representation of the estimated coefficients for each factor
{
p1 = ggplot(b) +
   aes(x = seq(1:51), y = FactMkt.RF) +
   geom_point(shape = "circle", size = 1.5, color = "black") +
   labs(title = "Market ") +
   labs(x = "Securities", y = "Market Factor") +
   theme_gray() +
   theme(plot.title = element_text(face = "bold", hjust = 0.5))
p1
# SBM
p2 = ggplot(b) +
  aes(x = seq(1:51), y = FactSMB) +
  geom_point(shape = "circle", size = 1.5, color = "black") +
  labs(title = "SBM factor") +
  labs(x = "Securities", y = "SMB Factor") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
p2
#HML
p3 = ggplot(b) +
   aes(x = seq(1:51), y = FactHML) +
   geom_point(shape = "circle", size = 1.5, color = "black") +
   labs(title = "HML factor") +
   labs(x = "Securities", y = "HML Factor") +
   theme_gray() +
   theme(plot.title = element_text(face = "bold", hjust = 0.5))
p3
#RMW
p4 = ggplot(b) +
   aes(x = seq(1:51), y = FactRMW) +
   geom_point(shape = "circle", size = 1.5, color = "black") +
   labs(title = "RMW factor") +
   labs(x = "Securities", y = "RMW Factor") +
   theme_gray() +
   theme(plot.title = element_text(face = "bold", hjust = 0.5))
p4
# CMA
p5 = ggplot(b) +
   aes(x = seq(1:51), y = FactCMA) +
   geom_point(shape = "circle", size = 1.5, color = "black") +
   labs(title = "CMA factor") +
   labs(x = "Securities", y = "CMA Factor") +
   theme_gray() +
   theme(plot.title = element_text(face = "bold", hjust = 0.5))
p5
# ENDEX TTF-GAS"TRPC-SRMC-Coal
p6 = ggplot(b) +
   aes(x = seq(1:51), y = FactENDEX.TTF.GAS) +
   geom_point(shape = "circle", size = 1.5, color = "black") +
   labs(title = "ENDEX TTF GAS factor") +
   labs(x = "Securities", y = "ENDEX TTF Gas Factor") +
   theme_gray() +
   theme(plot.title = element_text(face = "bold", hjust = 0.5))
p6
#TRPC-SRMC-Coal
p7 = ggplot(b) +
   aes(x = seq(1:51), y = FactTRPC.SRMC.Coal) +
   geom_point(shape = "circle", size = 1.5, color = "black") +
   labs(title = "TRPC SRMC Coal factor") +
   labs(x = "Securities", y = "SRMC Coal TRPC Factor") +
   theme_gray() +
   theme(plot.title = element_text(face = "bold", hjust = 0.5))
p7
# EEX-Phelix
p8 = ggplot(b) +
   aes(x = seq(1:51), y = FactEEX.Phelix) +
   geom_point(shape = "circle", size = 1.5, color = "black") +
   labs(title = "EEX Phelix factor") +
   labs(x = "Securities", y = "Phelix EEX Factor") +
   theme_gray() +
   theme(plot.title = element_text(face = "bold", hjust = 0.5))
p8
# Crude Oil Spot
p9 = ggplot(b) +
   aes(x = seq(1:51), y = FactCrude.Oil.Spot) +
   geom_point(shape = "circle", size = 1.5, color = "black") +
   labs(title = "Crude Oil Spot factor") +
   labs(x = "Securities", y = "Crude Oil Spot Factor") +
   theme_gray() +
   theme(plot.title = element_text(face = "bold", hjust = 0.5))
p9
}
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 3, nrow = 3)

# If you want you can observe the frequency histogram of the residuals

hist(resLM, nclass = 200, xlim = c(-10,10), main = "Frequency histogram of residuals",
      xlab = "Residuals", ylab = "Frequency")

# Boxplot of the estimated coefficients
boxplot(pars[,1:6], main = "Famous Factors - French")
boxplot(pars[,7:10], main = "Market factors")


# Representation of factor prices and returns
{
# I take the date values so I can build the graph
dat = read_excel('STOXX600.xlsx','TotalReturnsIndexes','A5487:A8682',col_names=FALSE,na="NA")
str(dat)
dat = data.frame(dat)
dat = as.Date(dat[,1])

datFF2 <- data.frame(Date = dat, Y = dataFF2)
colnames(datFF2) = c("Data", "Y1", "Y2", "Y3", "Y4")


pf1 = ggplot(datFF2) +
   aes(x = Date, y = Y1) +
   geom_line(colour = "#000000") +
   labs(x = "Time",y = "Price",title = "ENDEX TTF GAS factor prices") +
   theme_gray() +
   theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
pf2 = ggplot(datFF2) +
   aes(x = Date, y = Y2) +
   geom_line(colour = "#000000") +
   labs(x = "Time",y = "Price",title = "TRPC SRMC Coal Factor Prices") +
   theme_gray() +
   theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
pf3 = ggplot(datFF2) +
   aes(x = Date, y = Y3) +
   geom_line(colour = "#000000") +
   labs(x = "Time",y = "Price",title = "EEX Phelix Factor Pricing") +
   theme_gray() +
   theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
pf4 = ggplot(datFF2) +
   aes(x = Date, y = Y4) +
   geom_line(colour = "#000000") +
   labs(x = "Time",y = "Price",title = "Spot Crude Oil Factor Prices") +
   theme_gray() +
   theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))


rendFF2 = rFF2
rendFF2 <- data.frame(Date = dat[2:3196], Y = rendFF2)
colnames(rendFF2) = c("Data", "Y1", "Y2", "Y3", "Y4")

rf1 = ggplot(rendFF2) +
   aes(x = Date, y = Y1) +
   geom_line(colour = "#000000") +
   labs(x = "Time",y = "Return",title = "ENDEX TTF GAS Factor Returns") +
   theme_gray() +
   theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
rf2 = ggplot(rendFF2) +
   aes(x = Date, y = Y2) +
   geom_line(colour = "#000000") +
   labs(x = "Time",y = "Return",title = "TRPC SRMC Coal Factor Returns") +
   theme_gray() +
   theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
rf3 = ggplot(rendFF2) +
   aes(x = Date, y = Y3) +
   geom_line(colour = "#000000") +
   labs(x = "Time",y = "Return",title = "EEX Phelix Factor Returns") +
   theme_gray() +
   theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
rf4 = ggplot(rendFF2) +
   aes(x = Date, y = Y4) +
   geom_line(colour = "#000000") +
   labs(x = "Time",y = "Return",title = "Crude Oil Spot Factor Returns") +
   theme_gray() +
   theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))
}
grid.arrange(pf1,pf2,pf3,pf4, nrow = 2, ncol = 2)
grid.arrange(rf1,rf2,rf3,rf4, nrow = 2, ncol = 2)



# estimation of factor moments and standardized factors
# this will be useful when we eliminate the hypothesis of normality of the factors
muF=as.matrix(colMeans(Fact)) # average of the factors
sigmaF=cov(Fact) # factors with different variance levels, with the market variance
# (first row and first column) is the highest
P=chol(sigmaF) # standardization using Cholesky
# standardized factors that will be used later for the non-normality hypothesis
Zf=Fact-matrix(1,T,1) %*% t(muF) # Z calculation that we will need to perform resampling
Zf=Zf %*% inv(P)

# wallet weights
w=matrix(1/N,N,1) # EW portfolio with equal weights

# VaR quantile
a=0.05 # 5%

######################## WITHOUT IDIOSYNCRATIC COMPONENT ########################

# case 1 - Portfolio VaR under distributed factors hypothesis
# according to a multivariate normal
# I can calculate the mean and variance of the portfolio. Since the returns are normal, they will also be normal
# we calculate the VaR for factors distributed as a normal
muP=t(w) %*% (pars[,1] + pars[,2:10]%*%muF[1:9]) # portfolio return
# this corresponds to the formula in slice 33

SigmaP= t(w) %*% (pars[,2:10] %*% sigmaF[1:9,1:9] %*% t(pars[,2:10])) %*% w
VaRC1=muP + sqrt(SigmaP)*qnorm(a) #VaR one day
# in one day the maximum acceptable loss is -1.53%. The higher this value is
# the higher the risk of this portfolio


############################ IDIOSYNCRATIC COMPONENT #############################

# Calculate the variance and covariance matrix of the residuals
sigmaE = cov(resLM)
idios = t(w) %*% sigmaE %*% w
VaR_dis=muP + sqrt(SigmaP+idios)*qnorm(a)






