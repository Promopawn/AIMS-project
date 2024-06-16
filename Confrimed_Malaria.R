library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(dplyr)
install.packages("mratios")
library(mratios)
library(quantmod)
library(tseries)
library(xts)
library(tidyverse)

library(readxl)
MCC <- read_excel("/home/refilwe/Desktop/Aims Essay/Data/severe_malaria_cases.xlsx", sheet = "Sheet1", col_types = c("date","numeric", "numeric","numeric", "numeric"))


###########################################################
#### NEW USERS BEFORE January 2019 - AFTER March 2020(NEW USERS BEFORE January 2019 - AFTER March 2020)
###########################################################

View(MCC)
names(MCC)
nrow(MCC)
#MCC$index <- seq(1,72)
MCC_BI<-MCC[MCC$index<88,]
MCC_AI<-MCC[MCC$index>87,]


plot(MCC_BI$Case_of_severe_malaria_confirmed)
plot(MCC_AI$Case_of_severe_malaria_confirmed)

#plot(MCC_BI$NU_PF_control_Nombre_median)
#plot(MCC_AI$NU_PF_control_Nombre_median)

plot(MCC_BI$index, MCC_BI$Case_of_severe_malaria_confirmed,type = "o")
plot(MCC_AI$index, MCC_AI$Case_of_severe_malaria_confirmed,type = "o")

#plot(MCC_BI$index, MCC_BI$NU_PF_control_Nombre_median,type = "o")
#plot(MCC_AI$index, MCC_AI$NU_PF_control_Nombre_median,type = "o")


plot(MCC$index, MCC$Case_of_severe_malaria_confirmed,xaxt="n",type = "o",lwd="2", main="Confirmed cases of severe malaria",yaxt = "n",xlab = "", ylab = "Total confirmed malaria cases")
axis(1, at=MCC$index,labels=MCC$Months,xlab = "X-axis Label", ylab = "Y-axis Label", col.axis="black", las=2, cex.axis=1.0, tck=-.01)
axis(2, at = seq(0, 101000, by = 10010), labels = seq(0, 100, by = 10), las = 2)
#lines(MCC$index, MCC$NU_PF_control_Nombre_median,xaxt="n",type = "o",col="red",lwd="2")
#abline (v=c(74,75,76,77,78,79,80,81,82,83,84,85,86,87),col="Blue",lwd="2")
axis(side=1, at=MCC$index,labels=MCC$Months, col.axis="black", las=2, cex.axis=1.0, tck=-.01)
text(c(50,110), c(200,500),labels=c("Before intervention","During intervention"), cex=.8,col=c("red","red"))
abline (v=c(88),col="red",lwd="2")


### Temporary series analysis NU PF INT(Temporary series analysis NU PF INT)

Timeseries_BI <-  ts(MCC_BI$Case_of_severe_malaria_confirmed , start = c(2013,1), end= c(2020,3), frequency=12)
Timeseries_AI <-  ts(MCC_AI$Case_of_severe_malaria_confirmed, start = c(2020,4), end= c(2023,12), frequency=12)
Timeseries <-  ts(MCC$Case_of_severe_malaria_confirmed, start = c(2013,1), end= c(2023,12), frequency=12)

Timeseries_BI
Timeseries_AI
Timeseries


decomposed <- stl(Timeseries_BI, s.window = "periodic")
plot(decomposed)

Acf(Timeseries, plot = TRUE , main ="Confirmed cases of severe malaria 2013-2023")
Pacf(Timeseries,plot = TRUE , main ="Confirmed cases of severe malaria")



adf.test(Timeseries) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
kpss.test(Timeseries) #KPSS: Null is Stationary and Alt is non stationary
ndiffs(Timeseries) #how many differences needed
nsdiffs(Timeseries) #how many seasonal differences needed

adf.test(Timeseries_BI) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
kpss.test(Timeseries_BI) #KPSS: Null is Stationary and Alt is non stationary
ndiffs(Timeseries_BI) #how many differences needed
nsdiffs(Timeseries_BI) #how many seasonal differences needed


bi_dif<-diff(Timeseries,lag=12,differences = 1)
Acf(bi_dif, main ="Confirmed cases of severe malaria 2013-2023")
Pacf(bi_dif, main ="Confirmed cases of severe malaria 2013-2023")

##################################################################################################
############################## Forecast model ###################################################


fit_Timeseries_BI <- auto.arima(Timeseries_BI)
fit_Timeseries_BI
checkresiduals(fit_Timeseries_BI)# residuals take the form of white noise and are normally AIstributed
summary(fit_Timeseries_BI)

fit3 <- Arima(Timeseries_BI, order=c(1,0,0), seasonal=c(1,1,0))
fit3
summary(fit3)

fit4 <- Arima(Timeseries_BI, order=c(1,0,0), seasonal=c(2,1,0), include.constant = TRUE)
fit4
summary(fit4)

fit5 <- Arima(Timeseries_BI, order=c(1,0,0), seasonal=c(1,1,1)) #best model
fit5
summary(fit5)
checkresiduals(fit5)

fit6 <- Arima(Timeseries_BI, order=c(2,0,0), seasonal=c(1,1,0))
fit6
summary(fit6)



#################################################################################
############################ fitting model ######################################

bi_dif2<-diff(Timeseries,lag=12,differences = 1)
Acf(bi_dif2)
Pacf(bi_dif2)

fit_Timeseries <- auto.arima(Timeseries) #best model
fit_Timeseries
checkresiduals(fit_Timeseries)
summary(fit_Timeseries)

fit3S <- Arima(Timeseries, order=c(1,0,0), seasonal=c(1,1,1))
fit3S
summary(fit3S)

fit4S <- Arima(Timeseries, order=c(2,0,0), seasonal=c(0,1,0))
fit4S
summary(fit4S)

fit5S <- Arima(Timeseries, order=c(1,0,0), seasonal=c(1,1,0),include.constant = TRUE)
fit5S
summary(fit5S)

fit6S <- Arima(Timeseries, order=c(0,0,1), seasonal=c(0,1,1),include.constant = TRUE)
fit6S
summary(fit6S)

fit7S <- Arima(Timeseries, order=c(1,0,1), seasonal=c(0,1,1),include.constant = TRUE) 
fit7S
summary(fit7S)

##################################################################################


# h = 10*12 because, forecast is for 10 years for all 12 months
ffcast_fit_Timeseries_BI <-forecast(fit5, level=c(95), h=4*12)
plot(ffcast_fit_Timeseries_BI)
lines(Timeseries,col="red")
abline (v=2020.2,col="red",lwd="2")


#plot(ffcast_TS_NU_PF_AV_INT_CONT_D,xaxt = "n", main="Différence du nombre de NU entre intervention et contôle")
#(Difference in the number of NU between intervention and control)
#par(new=TRUE)
plot(ffcast_fit_Timeseries_BI, ylim=c(-0,101000), main="Confirmed cases of severe malaria",yaxt = "n",xlab = "Months", ylab = "Total confirmed malaria cases",type = "o")
axis(1, at=MCC$index,labels=MCC$Months,xlab = "X-axis Label", ylab = "Y-axis Label", col.axis="black", las=2, cex.axis=1.0, tck=-.01)
axis(2, at = seq(0, 101000, by = 10010), labels = seq(0, 100, by = 10), las = 2)
#plot(MCC$INT_CONT,col="red",lwd="2",type="o", ylim=c(-30000,30000))
lines(Timeseries,col="red",lwd="1",type="o")
lines((fitted(fit_Timeseries)))
#abline (v=c(2019,2020.3),col="black",lwd="2")
abline (v= 2020.2,col="red",lwd="2")
text(c(2016,2021.5), c(10000,10000),labels=c("Before covid restrictions","after covid restrictions"), cex=1,col=c("black","black"))
legend("topleft",2020, 100, legend=c("Observed", "Fitted","Forecast"), col=c( "red","black","blue"), lty=1:2)
#axis(side=1, at=c(2017,2017.2,2017.3,2017.4,2017.5,2017.6,2017.7,2017.8, 2017.9,2017.10,2017.11,2017.12,2018,2018.2,2018.3,2018.4,2018.5,2018.6,2018.7,2018.8,2018.9,2018.10,2018.11,2018.12,2019,2019.2,2019.3,2019.4,2019.5,2019.6,2019.7,2019.8,2019.9,2019.10,2019.11,2019.12,2020,2020.2,2020.3,2020.4,2020.5,2020.6,2020.7,2020.8,2020.9,2020.10,2020.11,2020.12,2021,2021.2,2021.3,2021.4,2021.5,2021.6,2021.7,2021.8,2021.9,2021.10,2021.11,2021.12,2022,2022.2,2022.3,2022.4,2022.5,2022.6,2022.7,2022.8,2022.9,2022.10,2022.11,2022.12),labels=c("Janv_2017","Fev_2017","Mars_2017","Avril_2017","Mai_2017","Juin_2017","Juillet_2017","Août_2017","Sept_2017","Oct_2017","Nov_2017","1/12/2017", "Janv_2018","Fev_2018","Mars_2018","Avril_2018","Mai_2018","Juin_2018","Juillet_2018","Août_2018","Sept_2018","Oct_2018","Nov_2018","Dec_2018", "Janv_2019","Fev_2019","Mars_2019","Avril_2019","Mai_2019","Juin_2019","Juillet_2019","Août_2019","Sept_2019","Oct_2019","Nov_2019","Dec_2019", "Janv_2020","Fev_2020","Mars_2020","Avril_2020","Mai_2020","Juin_2020","Juillet_2020","Août_2020","Sept_2020","Oct_2020","Nov_2020","Dec_2020", "Janv_2021","Fev_2021","Mars_2021","Avril_2021","Mai_2021","Juin_2021","Juillet_2021","Août_2021","Sept_2021","Oct_2021","Nov_2021","Dec_2021","Janv_2022","Fev_2022","Mars_2022","Avril_2022","Mai_2022","Juin_2022","Juillet_2022","Août_2022","Sept_2022","Oct_2022","Nov_2022","Dec_2022"), col.axis="black", las=2, cex.axis=1.0, tck=.1)


forcasted_value <- ffcast_fit_Timeseries_BI[["mean"]]
view(forcasted_value)
fitted_values <- fit_Timeseries[["fitted"]]
view(fitted_values)

death_malaria_ff <- cbind(forcasted_value,fitted_values)
death_malaria_ff <- cbind(MCC,death_malaria_ff)
death_malaria_ff

library("xlsx")
# Write the first data set in a new workbook
write.xlsx(forcasted_value, file = "solankafore2.xlsx",
           sheetName = "USA-ARRESTS", append = FALSE)

#############################################################################################


MSC_peaks <- read_excel("/home/refilwe/Desktop/Aims Essay/Data/severe_malaria_cases.xlsx", sheet = "peaks", col_types = c("date","numeric", "numeric","numeric", "numeric"))

MSC_peaks_to_march20<-MSC_peaks[MSC_peaks$index<88,]
MSC_peaks_from_april20<-MSC_peaks[MSC_peaks$index>87,]



#MSC_peaks_20_23$Months <- as.Date(MSC_peaks_20_23$Months)
#str(MSC_peaks_20_23)

##################################


ggplot() +
  #geom_point(data = MSC_peaks_to_march20, aes(x = index, y = Severe_malaria_confirmed), col = "blue") +
  #geom_smooth(data = MSC_peaks_to_march20, aes(x = index, y = Severe_malaria_confirmed), method = "lm", se = FALSE, color = "blue") +
  
  #geom_point(data = MSC_peaks_from_april20, aes(x = index, y = Severe_malaria_confirmed), color = "blue") +
  #geom_smooth(data = MSC_peaks_from_april20, aes(x = index, y = Severe_malaria_confirmed), method = "lm", se = FALSE, color = "blue") +
  
  #geom_point(data = MSC_peaks_to_march20, aes(x = index, y = fitted_values), color = "green") +
  #geom_smooth(data = MSC_peaks_to_march20, aes(x = index, y = fitted_values), method = "lm", se = FALSE, color = "green") +
  
  geom_point(data = MSC_peaks_from_april20, aes(x = index, y = fitted_values), color = "green") +
  geom_smooth(data = MSC_peaks_from_april20, aes(x = index, y = fitted_values), method = "lm", se = FALSE, color = "green") +
  
  geom_point(data = MSC_peaks_from_april20, aes(x = index, y = forecast), color = "red") +
  geom_smooth(data = MSC_peaks_from_april20, aes(x = index, y = forecast), method = "lm", se = FALSE, color = "red") +
  
  labs(x = "Months", y = "Confirmed severe malaria ", title = "Comparison of severe malaria peak cases from post intervention") +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),legend.position = "top"
  ) +
  geom_vline(xintercept = 88, linetype = "dashed", color = "black")

####################################################################################################
#model equations of the peaks
ff_model <- lm(MSC_peaks_from_april20$forecast~MSC_peaks_from_april20$index)# regression model for forcasted vaues 2020,4_2023
model_summary <-summary(ff_model)
coefficients_table <- model_summary$coefficients

ftt_model <- lm(MSC_peaks_from_april20$fitted_values~MSC_peaks_from_april20$index)# regression model for fitted values 2020,4_2023
summary(ftt_model)
model_summary2 <-summary(ftt_model)
coefficients_table2 <- model_summary2$coefficients

# Extract the standard error of the slope (the second row, second column)
slope1_se <- coefficients_table[2, "Std. Error"]

slope1 <- coefficients_table[2, "Estimate"]

slope2_se <- coefficients_table2[2, "Std. Error"]

slope2 <- coefficients_table2[2, "Estimate"]


ratio <-  slope2 / slope1
se_ratio <- sqrt((slope1_se/slope1)^2 + (slope2_se/slope2)^2) * abs(ratio)


# Calculate the confidence interval using the t-distribution
alpha <- 0.05  # 95% confidence interval
df1 <- ff_model$df.residual
df2 <- ff_model$df.residual
df_combined <- df1 + df2  # Combined degrees of freedom

t_value <- qt(1 - alpha / 2, df = df_combined)  # t-value for the t-distribution

lower_bound <- ratio - t_value * se_ratio
upper_bound <- ratio + t_value * se_ratio

ci <- c(lower_bound, upper_bound)
print(ci)

#Total_model1 <- lm(MSC_peaks_to_march20$Severe_malaria_confirmed~MSC_peaks_to_march20$index)#regression model for observed values 2013_2020,3
#summary(Total_model1)

Total_model2 <- lm(MSC_peaks_from_april20$Severe_malaria_confirmed~MSC_peaks_from_april20$index)#regression model for observed values 2020,4_2023
summary(Total_model2)

  
??ratioCI
  help(??ratioCI)
  
  
  
 

