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
FamilyPlan <- read_excel("/home/refilwe/Desktop/Aims Essay/Data/Family planning.xlsx", sheet = "Sheet1", col_types = c("date","numeric", "numeric","numeric", "numeric"))


###########################################################
#### NEW USERS BEFORE April 2020 - AFTER April 2020
###########################################################

View(FamilyPlan)
names(FamilyPlan)
nrow(FamilyPlan)
#FamilyPlan$index <- seq(1,72)
FamilyPlan_BI<-FamilyPlan[FamilyPlan$index<88,]
FamilyPlan_AI<-FamilyPlan[FamilyPlan$index>87,]



plot(FamilyPlan_BI$Total_new_users)
plot(FamilyPlan_AI$Total_new_users)


plot(FamilyPlan_BI$index, FamilyPlan_BI$Total_new_users,type = "o")
plot(FamilyPlan_AI$index, FamilyPlan_AI$Total_new_users,type = "o")




plot(FamilyPlan$index, FamilyPlan$Total_new_users,xaxt="n",type = "o",lwd="2", main="Family planning",xlab = "", ylab = "Total new users ")
axis(1, at=FamilyPlan$index,labels=FamilyPlan$Months,xlab = "X-axis Label", ylab = "Y-axis Label", col.axis="black", las=2, cex.axis=1.0, tck=-.01)
abline (v=c(88),col="red",lwd="2")


### Timeseries series analysis

Timeseries_BI <-  ts(FamilyPlan_BI$Total_new_users , start = c(2013,1), end= c(2020,03), frequency=12)
Timeseries_AI <-  ts(FamilyPlan_AI$Total_new_users, start = c(2020,4), end= c(2023,12), frequency=12)
Timeseries <-  ts(FamilyPlan$Total_new_users, start = c(2013,1), end= c(2023,12), frequency=12)
Timeseries_BI

################## decomposition ###################################################
decomposed_ts <- stl(Timeseries_BI, s.window = "periodic")
plot(decomposed_ts)
decomposed <- stl(Timeseries_BI, s.window = "periodic")
plot(decomposed)
###################################################################################

################## decomposition ###################################################
decomposed_ts <- stl(Timeseries_BI, s.window = "periodic")
plot(decomposed_ts)
decomposed <- stl(Timeseries_BI, s.window = "periodic")
plot(decomposed)
###################################################################################


Timeseries_AI
plot(Timeseries,main = "New Family planning users")
plot(Timeseries_BI, main = "New Family planning users before covid intervention",type = "o",ylab="New users")

Acf(Timeseries_BI, plot = TRUE)
Pacf(Timeseries_BI,plot = TRUE)



adf.test(Timeseries) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
kpss.test(Timeseries_BI) #KPSS: Null is Stationary and Alt is non stationary
ndiffs(Timeseries) #how many differences needed
nsdiffs(Timeseries) #how many seasonal differences needed

adf.test(Timeseries_BI) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
kpss.test(Timeseries_BI) #KPSS: Null is Stationary and Alt is non stationary
ndiffs(Timeseries) #how many differences needed
nsdiffs(Timeseries) #how many seasonal differences needed





Acf(Timeseries,main = "New Family planning users 2013-2023")
Pacf(Timeseries,main = "New Family planning users 2013-2023")

bi_dif2<-diff(Timeseries,lag=12,differences = 1)
Acf(bi_dif2,main = "New Family planning users 2013-2023")
Pacf(bi_dif2,main = "New Family planning users 2013-2023")



Acf(Timeseries_BI,main = "New Family planning users 2013-2020")
Pacf(Timeseries_BI,main = "New Family planning users 2013-2020")

bi_dif1<-diff(Timeseries_BI,lag=12,differences = 1)
Acf(bi_dif1)
Pacf(bi_dif1)

###################################Fitting model########################################################
fit_Timeseries <- auto.arima(Timeseries)
fit_Timeseries
summary(fit_Timeseries)
fit3S <- Arima(Timeseries, order=c(0,0,1), seasonal=c(1,1,1))
fit3S
summary(fit3S)
############################## testing for better models ########################################
fit_Timeseries_BI <- auto.arima(Timeseries_BI)
fit_Timeseries_BI
checkresiduals(fit_Timeseries_BI)


fit3S <- Arima(Timeseries, order=c(1,0,1), seasonal=c(0,1,0))
fit3S
summary(fit3S)

fit4S <- Arima(Timeseries, order=c(2,0,0), seasonal=c(1,1,1)) #best model
fit4S
summary(fit4S)
checkresiduals(fit4S)

fit5S <- Arima(Timeseries, order=c(0,0,2), seasonal=c(1,1,1))
fit5S
summary(fit5S)

fit6S <- Arima(Timeseries, order=c(0,0,0), seasonal=c(1,1,0))
fit6S
summary(fit6S)

fit7S <- Arima(Timeseries, order=c(0,0,0), seasonal=c(1,1,1)) 

fit8S <- Arima(Timeseries, order=c(1,0,1), seasonal=c(1,1,1)) #best model
fit8S
summary(fit8S)

checkresiduals(fit8S)
#################################################################################################


# h = 10*12 because, forecast is for 10 years for all 12 months
ffcast_fit_Timeseries_BI <-forecast(fit_Timeseries_BI, level=c(95), h=4*12)
plot(ffcast_fit_Timeseries_BI,type = "o")
lines(Timeseries,col="red")
#abline (v=2020,col="red",lwd="2")




plot(ffcast_fit_Timeseries_BI, ylim=c(-0,100000), main="Family planning",yaxt = "n",xlab = "Months", ylab = "Total New Users(Thousands)",type = "o")
axis(1, at=FamilyPlan$index,labels=FamilyPlan$Months,xlab = "X-axis Label", ylab = "Y-axis Label", col.axis="black", las=2, cex.axis=1.0, tck=-.01)
axis(2, at = seq(0, 100000, by = 10000), labels = seq(0, 100, by = 10), las = 2)
lines(fitted(fit7S),col="black",ylim=c(-0,100000),main="Family planning")
#plot(FamilyPlan$INT_CONT,col="red",lwd="2",type="o", ylim=c(-30000,30000))
lines(Timeseries,col="red",lwd="1",type="o")
#abline (v=c(2019,2020.3),col="green",lwd="2")
abline (v=2020.2,col="red",lwd="3")
text(c(2016,2022.5), c(0,0),labels=c("Before covid restrictions","After covid restrictions"), cex=1,col=c("black","black"))
legend("topleft",2020, 100, legend=c("fitted", "Observed","restriction","forecast"), col=c("black", "red","red","blue"), lty=1:2)

#axis(side=1, at=c(2013,2013.2,2013.3,2013.4,2013.5,2013.6,2013.7,2013.8,2013.9,2013.10,2013.11,2013.12,2017,2017.2,2017.3,2017.4,2017.5,2017.6,2017.7,2017.8, 2017.9,2017.10,2017.11,2017.12,2018,2018.2,2018.3,2018.4,2018.5,2018.6,2018.7,2018.8,2018.9,2018.10,2018.11,2018.12,2019,2019.2,2019.3,2019.4,2019.5,2019.6,2019.7,2019.8,2019.9,2019.10,2019.11,2019.12,2020,2020.2,2020.3,2020.4,2020.5,2020.6,2020.7,2020.8,2020.9,2020.10,2020.11,2020.12,2021,2021.2,2021.3,2021.4,2021.5,2021.6,2021.7,2021.8,2021.9,2021.10,2021.11,2021.12,2022,2022.2,2022.3,2022.4,2022.5,2022.6,2022.7,2022.8,2022.9,2022.10,2022.11,2022.12),labels=c("Janv_2017","Fev_2017","Mars_2017","Avril_2017","Mai_2017","Juin_2017","Juillet_2017","Août_2017","Sept_2017","Oct_2017","Nov_2017","1/12/2017", "Janv_2018","Fev_2018","Mars_2018","Avril_2018","Mai_2018","Juin_2018","Juillet_2018","Août_2018","Sept_2018","Oct_2018","Nov_2018","Dec_2018", "Janv_2019","Fev_2019","Mars_2019","Avril_2019","Mai_2019","Juin_2019","Juillet_2019","Août_2019","Sept_2019","Oct_2019","Nov_2019","Dec_2019", "Janv_2020","Fev_2020","Mars_2020","Avril_2020","Mai_2020","Juin_2020","Juillet_2020","Août_2020","Sept_2020","Oct_2020","Nov_2020","Dec_2020", "Janv_2021","Fev_2021","Mars_2021","Avril_2021","Mai_2021","Juin_2021","Juillet_2021","Août_2021","Sept_2021","Oct_2021","Nov_2021","Dec_2021","Janv_2022","Fev_2022","Mars_2022","Avril_2022","Mai_2022","Juin_2022","Juillet_2022","Août_2022","Sept_2022","Oct_2022","Nov_2022","Dec_2022"), col.axis="black", las=2, cex.axis=1.0, tck=.1)

forcasted_value <- ffcast_fit_Timeseries_BI[["mean"]]
view(forcasted_value)
fitted_values <- fit_Timeseries[["fitted"]]
view(fitted_values)


death_malaria_ff <- cbind(FamilyPlan,death_malaria_ff)
death_malaria_ff

library("xlsx")
# Write the first data set in a new workbook
write.xlsx(fitted_values, file = "solanka.xlsx",
           sheetName = "USA-ARRESTS", append = FALSE)

install.packages("writexl")
library("writexl")

write_xlsx(death_malaria_ff, "/home/refilwe/Desktop/Aims Essay/Data/severe_malaria_cases_deaths.xlsx")

#####################################################################################################################
#####################################Peaks##########################################################################
Familypeaks <- read_excel("/home/refilwe/Desktop/Aims Essay/Data/Family planning.xlsx", sheet = "peaks", col_types = c("date","numeric","numeric","numeric","numeric"))

Fpeaks_to_march20<-Familypeaks[Familypeaks$index<88,]
Fpeaks_from_april20<-Familypeaks[Familypeaks$index>87,]


########################## plotting of the peaks scatter plots and trend lines ############################# 
ggplot() +
  geom_point(data = Fpeaks_to_march20, aes(x = index, y = Total_new_users), col = "blue") +
  geom_smooth(data = Fpeaks_to_march20, aes(x = index, y = Total_new_users), method = "lm", se = FALSE, color = "blue") +
  
  geom_point(data = Fpeaks_from_april20, aes(x = index, y = Total_new_users), color = "blue") +
  geom_smooth(data = Fpeaks_from_april20, aes(x = index, y = Total_new_users), method = "lm", se = FALSE, color = "blue") +
  
  geom_point(data = Fpeaks_to_march20, aes(x = index, y = fitted_values), color = "green") +
  geom_smooth(data = Fpeaks_to_march20, aes(x = index, y = fitted_values), method = "lm", se = FALSE, color = "green") +
  
  geom_point(data = Fpeaks_from_april20, aes(x = index, y = fitted_values), color = "green") +
  geom_smooth(data = Fpeaks_from_april20, aes(x = index, y = fitted_values), method = "lm", se = FALSE, color = "green") +
  
  geom_point(data = Fpeaks_from_april20, aes(x = index, y = forecast), color = "red") +
  geom_smooth(data = Fpeaks_from_april20, aes(x = index, y = forecast), method = "lm", se = FALSE, color = "red") +
  
  labs(x = "Months", y = "New Users", title = "Comparison of New Users from 2013-2019 and 2020-2023") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),legend.position = "top"
  ) +
  geom_vline(xintercept = 88, linetype = "dashed", color = "black")

####################################################################################################
#model equations of the peaks
ff_model <- lm(Fpeaks_from_april20$forecast~Fpeaks_from_april20$index)# regression model for forcasted vaues 2020,4_2023
summary(ff_model)

#ftted <- lm(Fpeaks_to_march20$index~Fpeaks_to_march20$fitted_values)# regression model for fitted values 2013_2020,3
#summary(ftted)

ftt_model <- lm(Fpeaks_from_april20$fitted_values~Fpeaks_from_april20$index)# regression model for fitted values 2020,4_2023
summary(ftt_model)

#Total_model1 <- lm(Fpeaks_to_march20$index~Fpeaks_to_march20$Total_new_users)#regression model for observed values 2013_2020,3
#summary(Total_model1)

#Total_model2 <- lm(Fpeaks_from_april20$index~Fpeaks_from_april20$Total_new_users)#regression model for observed values 2020,4_2023
#summary(Total_model2)








