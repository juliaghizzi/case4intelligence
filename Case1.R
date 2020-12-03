library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(stargazer)
library(stringr)
library(tseries)
library(forecast)

#--------- DIRECTORIES

direc_input  =  'C:/Users/julia/OneDrive/Documentos/4intelligence/Challenge_4i'
direc_output =  'C:/Users/julia/OneDrive/Documentos/4intelligence/Challenge_4i/output'

#--------- FILES

tfp_file   = 'TFP.csv'

#-------- SETTING DIRECTORY

setwd(direc_output)

#--------- EXERCISE

tfp_data = fread(paste(direc_input,tfp_file,sep = "/"),sep = ",")

#-------- FIRST LOOK AT DATA

#Plotting time series
ggplot(tfp_data, aes(y=rtfpna, x=year))+
  geom_line(size=1)+
  theme_bw()+
  theme(legend.title = element_blank())+xlab('TFP')+ylab('Year')+
  facet_grid(~isocode)
ggsave('ts_all.png')
# All TFP series seem to be not stationary

#Creating time series objects
USA = ts(filter(tfp_data, isocode=="USA")$rtfpna, start = min(tfp_data$year), frequency = 1)
MEX = ts(filter(tfp_data, isocode=="MEX")$rtfpna, start = min(tfp_data$year), frequency = 1)
CAN = ts(filter(tfp_data, isocode=="CAN")$rtfpna, start = min(tfp_data$year), frequency = 1)
aux_data = cbind(USA,CAN,MEX)
aux_name = c('USA','CAN','MEX')
n        = length(aux_data[,1])


#Plotting x_t against x_t+s
s<-1                             # ! SET THE LAG 
png(paste("plot_xt_xt",t,".png",sep = ""), width = 500, height = 500)
par(mfrow=c(2,2),mai=c(0.1,0.7,0.7,0.1))
for(i in 1:3){
  plot(aux_data[1:(n-s),i],aux_data[(1+s):n,i], 
       xlab = paste(aux_name[i],"t+1"),
       ylab = paste(aux_name[i],"t"))
}
dev.off()

# The time series are strongly autocorrelated, specially USA TFP

# Taking the first difference of the time series - TFPgrowth
png("first_diff", width = 500, height = 500)
par(mfrow=c(2,2),mai=c(0.1,0.7,0.7,0.1))
for(i in 1:3){
  plot(diff(aux_data[,i]), ylab = paste(aux_name[i]))
  abline(h=mean(diff(aux_data[,i])), col="blue")
}
dev.off()
# 

# Checking for unit roots
vars = list(USA,MEX,CAN)
names(vars)=c('USA','MEX','CAN')

print("p-values of ADF test")
lapply(vars, function(x) adf.test(x)$p.value)

print("p-values of Phillips-Perron test")
lapply(vars, function(x) pp.test(x)$p.value)

#Indeed, all series have a unit root

lapply(vars, function(x) adf.test(diff(x))$p.value)

#After taking the first difference MEX still has a unit root
adf.test(diff(MEX,lag=2))$p.value

`Stationary Series` = cbind(diff(USA),diff(CAN),diff(MEX,lag=2))
colnames(`Stationary Series`) = c('dUSA','dCAN','ddMEX')
plot(`Stationary Series`)


#Checking ACF and PACF
vars = list(diff(USA),diff(MEX,lag=2),diff(CAN))
names(vars)=c('dUSA','ddMEX','dCAN')

png("acf.png", width = 500, height = 500)
par(mfrow=c(1,3),mai=c(0.3,0.25,0.5,0.25))
for (i in 1:3){
  Acf(vars[[i]],main=names(vars)[[i]])
}
dev.off()

png("pacf.png", width = 500, height = 500)
par(mfrow=c(1,3),mai=c(0.3,0.25,0.5,0.25))
for (i in 1:3){
  Pacf(vars[[i]],main=names(vars)[[i]])
}
dev.off()

# GUESSES FROM GRAPH ANALISYS
# dUSA:  no memory at all -ARIMA (0,1,0) with drift
# ddMEX: ARIMA(p,2,0)
# dCAN:  ARIMA(0,1,q)

#-------- FORECAST

#Now that we have some idea of the process that generated the series,let's use
#the R algorithm that test many specifications conditional on d

USA_treino = USA[1:(62-10)]
CAN_treino = CAN[1:(62-10)]
MEX_treino = MEX[1:(62-10)]

USA_out = USA[(62-9):62]
CAN_out = CAN[(62-9):62]
MEX_out = MEX[(62-9):62]

# USA - Checking the fit of the initial model

usaOPT = auto.arima(USA_treino,d=1)
summary(usaOPT)
coeftest(usaOPT)
checkresiduals(CANOPT) 

plot(USA_treino, type = "l")
lines(usaOPT$fitted,col='blue')

fct = forecast(usaOPT,h=10)
plot(fct)

plot(as.numeric(USA_out), type = "l")
lines(as.numeric(fct$mean),col='red')

# CAN - Checking the fit of the initial model

CANOPT = auto.arima(CAN_treino,d=1)
summary(CANOPT)
coeftest(CANOPT)
checkresiduals(CANOPT) 

plot(CAN_treino, type = "l")
lines(CANOPT$fitted,col='blue')

fct = forecast(CANOPT,h=10)
plot(fct)

plot(as.numeric(CAN_out), type = "l")
lines(as.numeric(fct$mean),col='red')

# MEX - Checking the fit of the initial model

MEXOPT = auto.arima(MEX_treino,d=2)
summary(MEXOPT)
coeftest(MEXOPT)
checkresiduals(MEXOPT) 

plot(MEX_treino, type = "l")
lines(MEXOPT$fitted,col='blue')

fct = forecast(MEXOPT,h=10)
plot(fct)

plot(as.numeric(MEX_out), type = "l")
lines(as.numeric(fct$mean),col='red')

# TESTING OTHER MODELS: All combinations of +1/-1 AR and MA lags

#####################
#    !ATTENTION!    # 
####################
#SET INITIAL ARIMA:
p = 0
d = 2
q = 1 

# SET TIME SERIES:
treino = MEX_treino
out    = MEX_out

store<-c()
for (j in 0:2)  {
for (i in 0:2)  {
  for (k in 1:4){
    if (k==4 &  p-i>=0 & q-j>=0){
      guess = arima(treino, order=c(p-i,d,q-j))
      fct   = forecast(guess,h=10)
      ARIMA=t(c(p-i,d,q-j))
    } else {
      if (k==3 & p-i>=0) {
      guess = arima(treino, order=c(p-i,d,q+j))
      fct   = forecast(guess,h=10)
      ARIMA=t(c(p-i,d,q+j))
    } else {
      if (k==2 & q-j>=0){
      guess = arima(treino, order=c(p+i,d,q-j))
      fct   = forecast(guess,h=10)
      ARIMA=t(c(p+i,d,q-j))
    } else { 
      guess = arima(treino, order=c(p+i,d,q+j))
      fct   = forecast(guess,h=10)
      ARIMA=t(c(p+i,d,q+j))
    }}}
    
    RMSE =accuracy(as.numeric(out),as.numeric(fct$mean))[2]
    MAE  =accuracy(as.numeric(out),as.numeric(fct$mean))[3]
    A    =cbind(ARIMA,RMSE,MAE)%>%print
    store=rbind(store,A)
    rm(fct)
    rm(guess)
    rm(A)
  }
  }
  }

a=as.data.frame(store) %>% rename(P=1,D=2,Q=3,RMSE=4,MAE=5) %>% distinct %>%
  filter(RMSE==min(RMSE),MAE==min(MAE))%>%
  mutate(initial = ifelse(P==p & D==d & Q==q,"THE INITIAL IS THE BEST",""))

print('Best Models for Forecast')
a
unique(a$initial)

BEST = arima(treino, c(unique(a$P),unique(a$D),unique(a$Q)))
summary(BEST)
checkresiduals(BEST)
coeftest(BEST)
BEST_fct = forecast(BEST,h=10)
plot(BEST_fct)

plot(as.numeric(out), type = "l")
lines(as.numeric(BEST_fct$mean),col='red')

# All selected models have normally distributed residuals and no autocorrelation
# Since the selected models are in line with the initial guesses, I rather keep
# the algorithm's choice.


#USA

USA_I = auto.arima(USA)

forecast(USA_I,h=10)%>%autoplot+theme_bw()+theme(plot.title = element_text(size=8,face='bold'),axis.text.y = element_text(size=8),axis.text.x = element_text(size=8),axis.title=element_text(size=8))
ggsave("usa_fct.png")

#CAN
CAN_I= arima(CAN,c(2,1,2))
CAN_B= arima(CAN,c(4,1,4))

I=forecast(CAN_I,h=10)%>%autoplot+theme_bw()+theme(plot.title = element_text(size=8,face='bold'),axis.text.y = element_text(size=8),axis.text.x = element_text(size=8),axis.title=element_text(size=8))
B=forecast(CAN_B,h=10)%>%autoplot+theme_bw()+theme(plot.title = element_text(size=8,face='bold'),axis.text.y = element_text(size=8),axis.text.x = element_text(size=8),axis.title=element_text(size=8))
plot_grid(I,B)
ggsave("can_fct.png")

#MEX
MEX_I= arima(MEX,c(0,2,1))
MEX_B= arima(MEX,c(2,2,1))

I=forecast(MEX_I,h=10)%>%autoplot+theme_bw()+theme(plot.title = element_text(size=8,face='bold'),axis.text.y = element_text(size=8),axis.text.x = element_text(size=8),axis.title=element_text(size=8))
B=forecast(MEX_B,h=10)%>%autoplot+theme_bw()+theme(plot.title = element_text(size=8,face='bold'),axis.text.y = element_text(size=8),axis.text.x = element_text(size=8),axis.title=element_text(size=8))
plot_grid(I,B)
ggsave("mex_fct.png")

cbind(c(2012:2021),
            forecast(USA_I,h=10)$mean,
            forecast(CAN_I,h=10)$mean,
            forecast(MEX_I,h=10)$mean)%>% 
  as.data.frame%>% rename(USA=2,CAN=3,MEX=4,Year=1)%>%
stargazer(summary = F,out='I_fct.tex')

cbind(c(2012:2021),
            forecast(CAN_B,h=10)$mean,
            forecast(MEX_B,h=10)$mean)%>% 
  as.data.frame%>% rename(CAN=2,MEX=3,Year=1)%>%
stargazer(summary = F,out='B_fct.tex')

