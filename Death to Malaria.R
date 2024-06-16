library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(dplyr)

library(quantmod)
library(tseries)
library(xts)
library(tidyverse)

library(readxl)

###########################################################
#### NEW USERS BEFORE January 2019 - AFTER March 2020(NEW USERS BEFORE January 2019 - AFTER March 2020)
###########################################################

SMD <- read_excel("/home/refilwe/Desktop/Aims Essay/Data/severe_malaria_cases_deaths.xlsx", sheet = "Sheet1", col_types = c("date","numeric", "numeric","numeric", "numeric"))

View(SMD)
names(SMD)
nrow(SMD)

SMD_BI<-SMD[SMD$index<88,]
SMD_AI<-SMD[SMD$index>87,]


plot(SMD_BI$death_due_to_severe_malaria)
plot(SMD_AI$death_due_to_severe_malaria)



plot(SMD_BI$index, SMD_BI$death_due_to_severe_malaria,type = "o")
plot(SMD_AI$index, SMD_AI$death_due_to_severe_malaria,type = "o")



plot(SMD$index, SMD$death_due_to_severe_malaria,xaxt="n",type = "o",lwd="2", main="Deaths due to severe malaria",xlab = "", ylab = "Total confirmed deaths ")
axis(1, at=SMD$index,labels=SMD$Months,xlab = "X-axis Label", ylab = "Y-axis Label", col.axis="black", las=2, cex.axis=1.0, tck=-.01)
abline (v=c(88),col="red",lwd="2")





################################################################################################################
### Temporary series analysis NU PF INT(Temporary series analysis NU PF INT)

Timeseries_BI <-  ts(SMD_BI$death_due_to_severe_malaria , start = c(2013,1), end= c(2020,03), frequency=12)
Timeseries_AI <-  ts(SMD_AI$death_due_to_severe_malaria, start = c(2020,4), end= c(2023,12), frequency=12)
Timeseries <-  ts(SMD$death_due_to_severe_malaria, start = c(2013,1), end= c(2023,12), frequency=12)
Timeseries_BI

plot(Timeseries)



Acf(Timeseries, plot = TRUE,main = "Deaths due to severe malaria")
Pacf(Timeseries,plot = TRUE)
Acf(Timeseries_BI, plot = TRUE,main = "Deaths due to severe malaria 2013-2020 before differencing")
Pacf(Timeseries_BI,plot = TRUE,main = "Deaths due to severe malaria 2013-2020")



adf.test(Timeseries) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
kpss.test(Timeseries) #KPSS: Null is Stationary and Alt is non stationary

adf.test(Timeseries_BI) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
kpss.test(Timeseries_BI) #KPSS: Null is Stationary and Alt is non stationary

ndiffs(Timeseries) #how many AIfferences needed
nsdiffs(Timeseries) #how many seasonal AIfferences needed
ndiffs(Timeseries_BI) #how many AIfferences needed
nsdiffs(Timeseries_BI) #how many seasonal AIfferences needed

bi_dif<-diff(Timeseries,lag=12,differences = 1)
Acf(bi_dif,main = "Deaths due to severe malaria")
Pacf(bi_dif,main = "Deaths due to severe malaria")
plot(bi_dif)
decomposed <- stl(bi_dif, s.window = "periodic")
plot(decomposed)

bi_dif<-diff(Timeseries_BI,lag=12,differences = 1)
Acf(bi_dif,main = "Deaths due to severe malaria 2013-2020 after differencing ")
Pacf(bi_dif,main = "Deaths due to severe malaria 2013-2020 after differencing")
plot(bi_dif)
decomposed <- stl(bi_dif, s.window = "periodic")
plot(decomposed)
##################################################################################################
############################## Forecast model ###################################################


fit_Timeseries_BI <- auto.arima(Timeseries_BI)
fit_Timeseries_BI #best model
checkresiduals(fit_Timeseries_BI)
fit4 <- Arima(Timeseries_BI, order=c(2,0,0), seasonal=c(0,1,0), include.constant=TRUE)
fit4
summary(fit4)

fit5 <- Arima(Timeseries_BI, order=c(1,0,0), seasonal=c(1,1,0), include.constant=TRUE)
fit5
summary(fit5)

fit6 <- Arima(Timeseries_BI, order=c(0,0,0), seasonal=c(1,1,0), include.constant=TRUE)
fit6
summary(fit6)

fit7 <- Arima(Timeseries_BI, order=c(0,0,0), seasonal=c(1,1,1), include.constant=TRUE) 
fit7
summary(fit7)

#################################################################################
############################ fitting model ######################################
fit_Timeseries <- auto.arima(Timeseries)
fit_Timeseries
checkresiduals(fit_Timeseries)
summary(fit_Timeseries)

fit3S <- Arima(Timeseries, order=c(1,0,1), seasonal=c(0,1,0),include.constant = TRUE)
fit3S
summary(fit3S)

fit4S <- Arima(Timeseries, order=c(2,0,0), seasonal=c(0,1,0),include.constant = TRUE)
fit4S
summary(fit4S)

fit5S <- Arima(Timeseries, order=c(1,0,0), seasonal=c(1,1,0),include.constant = TRUE)
fit5S
summary(fit5S)

fit6S <- Arima(Timeseries, order=c(0,0,0), seasonal=c(1,1,0),include.constant = TRUE)
fit6S
summary(fit6S)

fit7S <- Arima(Timeseries, order=c(0,0,0), seasonal=c(1,1,1),include.constant = TRUE) 
fit7S
summary(fit7S)

##################################################################################


# h = 10*12 because, forecast is for 10 years for all 12 months
ffcast_fit_Timeseries_BI <-forecast(fit_Timeseries_BI, level=c(95), h=4*12)
ffcast_fit_Timeseries_BI
plot(ffcast_fit_Timeseries_BI)
lines(Timeseries,col="red")
abline (v=2020.3,col="red",lwd="2")

#plot(ffcast_TS_NU_PF_AV_INT_CONT_D,xaxt = "n", main="AIfférence du nombre de NU entre intervention et contôle")
#(AIfference in the number of NU between intervention and control)
#par(new=TRUE)
plot(ffcast_fit_Timeseries_BI, ylim=c(0,1100), main="Deaths from severe malaria",xlab = "Months", ylab = "Total Deaths")
axis(side=1, at=SMD$index,labels=SMD$Months, xlab = "f", ylab = "ds", col.axis="black", las=3, cex.axis=1.0, tck=-.01)
lines(Timeseries,col="red",lwd="1",type="o")
#abline (v=c(2019,2020.3),col="black",lwd="2")
abline (v= 2020.2,col="red",lwd="2")
text(c(2016,2021.5), c(0,0),labels=c("Before intervention","After intervention"), cex=1,col=c("black","black"))
legend("topleft",2020, 100, legend=c("Observed", "fitted","forecast"), col=c("red", "black","blue"), lty=1:2)
#axis(side=1, at=c(2017,2017.2,2017.3,2017.4,2017.5,2017.6,2017.7,2017.8, 2017.9,2017.10,2017.11,2017.12,2018,2018.2,2018.3,2018.4,2018.5,2018.6,2018.7,2018.8,2018.9,2018.10,2018.11,2018.12,2019,2019.2,2019.3,2019.4,2019.5,2019.6,2019.7,2019.8,2019.9,2019.10,2019.11,2019.12,2020,2020.2,2020.3,2020.4,2020.5,2020.6,2020.7,2020.8,2020.9,2020.10,2020.11,2020.12,2021,2021.2,2021.3,2021.4,2021.5,2021.6,2021.7,2021.8,2021.9,2021.10,2021.11,2021.12,2022,2022.2,2022.3,2022.4,2022.5,2022.6,2022.7,2022.8,2022.9,2022.10,2022.11,2022.12),labels=c("Janv_2017","Fev_2017","Mars_2017","Avril_2017","Mai_2017","Juin_2017","Juillet_2017","Août_2017","Sept_2017","Oct_2017","Nov_2017","1/12/2017", "Janv_2018","Fev_2018","Mars_2018","Avril_2018","Mai_2018","Juin_2018","Juillet_2018","Août_2018","Sept_2018","Oct_2018","Nov_2018","Dec_2018", "Janv_2019","Fev_2019","Mars_2019","Avril_2019","Mai_2019","Juin_2019","Juillet_2019","Août_2019","Sept_2019","Oct_2019","Nov_2019","Dec_2019", "Janv_2020","Fev_2020","Mars_2020","Avril_2020","Mai_2020","Juin_2020","Juillet_2020","Août_2020","Sept_2020","Oct_2020","Nov_2020","Dec_2020", "Janv_2021","Fev_2021","Mars_2021","Avril_2021","Mai_2021","Juin_2021","Juillet_2021","Août_2021","Sept_2021","Oct_2021","Nov_2021","Dec_2021","Janv_2022","Fev_2022","Mars_2022","Avril_2022","Mai_2022","Juin_2022","Juillet_2022","Août_2022","Sept_2022","Oct_2022","Nov_2022","Dec_2022"), col.axis="black", las=2, cex.axis=1.0, tck=.1)
#plot(fitted(fit_Timeseries))

lines(fitted((fit_Timeseries)),col="black")
forcasted_value <- ffcast_fit_Timeseries_BI[["mean"]]
view(forcasted_value)
fitted_values <- fit_Timeseries[["fitted"]]
view(fitted_values)

death_malaria_ff <- cbind(forcasted_value,fitted_values)
death_malaria_ff <- cbind(SMD,death_malaria_ff)
death_malaria_ff


install.packages("writexl")
library("writexl")

write_xlsx(death_malaria_ff, "/home/refilwe/Desktop/Aims Essay/Data/severe_malaria_cases_deaths.xlsx")
#########################################################################################

SMD_peaks <- read_excel("/home/refilwe/Desktop/Aims Essay/Data/severe_malaria_cases_deaths.xlsx", sheet = "peaks", col_types = c("date","numeric", "numeric","numeric", "numeric"))

Death_peaks_to_march20<-SMD_peaks[SMD_peaks$index<88,]
Death_peaks_from_april20<-SMD_peaks[SMD_peaks$index>87,]



#SMD_peaks_20_23$Months <- as.Date(SMD_peaks_20_23$Months)
#str(SMD_peaks_20_23)

##################################

library(ggplot2)

# Adjusting the ggplot code
ggplot() +
  #geom_point(data = Death_peaks_to_march20, aes(x = index, y = death_due_to_severe_malaria), col = "blue") +
  #geom_smooth(data = Death_peaks_to_march20, aes(x = index, y = death_due_to_severe_malaria), method = "lm", se = FALSE, color = "blue") +
  
  #geom_point(data = Death_peaks_from_april20, aes(x = index, y = death_due_to_severe_malaria), color = "blue") +
  #geom_smooth(data = Death_peaks_from_april20, aes(x = index, y = death_due_to_severe_malaria), method = "lm", se = FALSE, color = "blue") +
  
  #geom_point(data = Death_peaks_to_march20, aes(x = index, y = fitted_values), color = "green") +
  #geom_smooth(data = Death_peaks_to_march20, aes(x = index, y = fitted_values), method = "lm", se = FALSE, color = "green") +
  
  geom_point(data = Death_peaks_from_april20, aes(x = index, y = fitted_values), color = "green") +
  geom_smooth(data = Death_peaks_from_april20, aes(x = index, y = fitted_values), method = "lm", se = FALSE, color = "green") +
  
  geom_point(data = Death_peaks_from_april20, aes(x = index, y = forecast), color = "red") +
  geom_smooth(data = Death_peaks_from_april20, aes(x = index, y = forecast), method = "lm", se = FALSE, color = "red") +
  
  labs(x = "Months", y = "Deaths due to severe malaria", title = "Comparison of severe malaria deaths peaks post-intervention 2020-2023") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),legend.position = "bottom"
  ) +
  geom_vline(xintercept = 88, linetype = "dashed", color = "black")

####################################################################################################
#model equations of the peaks
ff_model <- lm(Death_peaks_from_april20$forecast~Death_peaks_from_april20$index)# regression model for forcasted vaues 2020,4_2023
summary(ff_model)

ftt_model <- lm(Death_peaks_from_april20$fitted_values~Death_peaks_from_april20$index)# regression model for fitted values 2020,4_2023
summary(ftt_model)

#ftted <- lm(Death_peaks_to_march20$index~Death_peaks_to_march20$fitted_values)# regression model for fitted values 2013_2020,3
#summary(ftted)



#Total_model1 <- lm(Death_peaks_to_march20$index~Death_peaks_to_march20$death_due_to_severe_malaria)#regression model for observed values 2013_2020,3
#summary(Total_model1)

#Total_model2 <- lm(Death_peaks_from_april20$index~Death_peaks_from_april20$death_due_to_severe_malaria)#regression model for observed values 2020,4_2023
#summary(Total_model2)
