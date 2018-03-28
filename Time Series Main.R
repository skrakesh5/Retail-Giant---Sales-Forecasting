#### 1 Loading the required libraries

library(forecast)
library(tseries)
require(graphics)
library(hash)
library(FinCal)

#2 Data understanding and Cleaning

#Loading the file into R
Global_store <- read.csv("Global Superstore.csv")
Global_Superstore <- Global_store[,c(3,8,13,19,20,22)]

# Checking the structure of the dataframes
str(Global_Superstore)

# Finding out the length of dataframe
length(unique(Global_Superstore$Row.ID))


#Checking for column having NA values
list.NA<-""
for (i in c(1:ncol(Global_Superstore)))
{
  len<-length(grep("TRUE",is.na(Global_Superstore[,i])))
  if(len > 0){
    list.NA<-paste(colnames(Global_Superstore[i]),":",len,list.NA)
  }
}

#No missing values for columns considering for analysis



#checking for distinct values
unique(Global_Superstore$Market)
unique(Global_Superstore$Segment)


# Order.Date and Ship.Date are not in a uniform format. Putting them in a uniform format.
Global_Superstore$Order.Date<-as.Date(Global_Superstore$Order.Date,"%d-%m-%Y")



#Creating Subset based on Market and Segment

Total_profit<-aggregate(Global_Superstore$Profit, by=list(Global_Superstore$Market,Global_Superstore$Segment), FUN=sum)

Average_sales_amount<-aggregate(Global_Superstore$Sales, by=list(Global_Superstore$Market,Global_Superstore$Segment), FUN=mean)

Total_sales_amount<-aggregate(Global_Superstore$Sales, by=list(Global_Superstore$Market,Global_Superstore$Segment), FUN=sum)

Number_of_sales<-aggregate(Global_Superstore$Sales, by=list(Global_Superstore$Market,Global_Superstore$Segment), FUN=length)

Global_Superstore_Subset<-data.frame(Total_profit,Average_sales_amount,Total_sales_amount,Number_of_sales)
Global_Superstore_Subset<-Global_Superstore_Subset[,c(1,2,3,6,9,12)]
colnames(Global_Superstore_Subset) <- c("Market","Segment","TotalProfit","AverageSalesAmount","TotalSalesAmount","NumberOfSales")


# Adding Profit as a percentage of Sales amount to the dataframe
Global_Superstore_Subset$ProfitPercentage<-(Global_Superstore_Subset$TotalProfit/Global_Superstore_Subset$TotalSalesAmount)*100



# Analysis of MonthlyTotalProfit, MonthlyAverageSales, MonthlyTotalSales, MonthlyNumberOfSales on Market, Segment with respect to Month and Year
Monthly_total_profit<-aggregate(Global_Superstore$Profit, by=list(Global_Superstore$Market,Global_Superstore$Segment,format(as.Date(Global_Superstore$Order.Date), "%Y%m")), FUN=mean)

Monthly_average_sales<-aggregate(Global_Superstore$Sales, by=list(Global_Superstore$Market,Global_Superstore$Segment,format(as.Date(Global_Superstore$Order.Date), "%Y%m")), FUN=mean)

Monthly_total_sales<-aggregate(Global_Superstore$Sales, by=list(Global_Superstore$Market,Global_Superstore$Segment,format(as.Date(Global_Superstore$Order.Date), "%Y%m")), FUN=sum)

Monthly_number_of_sales<-aggregate(Global_Superstore$Sales, by=list(Global_Superstore$Market,Global_Superstore$Segment,format(as.Date(Global_Superstore$Order.Date), "%Y%m")), FUN=length)

Monthly_total_quantity<-aggregate(Global_Superstore$Quantity, by=list(Global_Superstore$Market,Global_Superstore$Segment,format(as.Date(Global_Superstore$Order.Date), "%Y%m")), FUN=sum)


Global_Superstore_Monthly_Subset<-data.frame(Monthly_total_quantity,Monthly_total_profit,Monthly_average_sales,Monthly_total_sales,Monthly_number_of_sales)
Global_Superstore_Monthly_Subset<-Global_Superstore_Monthly_Subset[,c(1,2,3,4,8,12,16,20)]
colnames(Global_Superstore_Monthly_Subset) <- c("Market","Segment","Month","MonthlyTotalQuantity","MonthlyTotalProfit","MonthlyAverageSale","MonthlyTotalSales","MonthlyNumberOfSales")



# Adding monthly profits as a percentage of monthly sales
Global_Superstore_Monthly_Subset$MonthlyProfitPercentage<-(Global_Superstore_Monthly_Subset$MonthlyTotalProfit/Global_Superstore_Monthly_Subset$MonthlyTotalSales)*100
Global_Superstore_Monthly_Subset<-Global_Superstore_Monthly_Subset[order(Global_Superstore_Monthly_Subset$MonthlyTotalSales,decreasing=TRUE),]



# Finding Meand and Standard deviation of monthly profit percentage across Market and Segment
Mean_Global_Superstore_Monthly_Subset<-aggregate(Global_Superstore_Monthly_Subset$MonthlyProfitPercentage, by=list(Global_Superstore_Monthly_Subset$Market,Global_Superstore_Monthly_Subset$Segment), FUN=mean)
colnames(Mean_Global_Superstore_Monthly_Subset)<-c("Market","Segment","AverageMonthlyProfitPercent")

Standard_Deviation_Global_Superstore_Monthly_Subset<-aggregate(Global_Superstore_Monthly_Subset$MonthlyProfitPercentage, by=list(Global_Superstore_Monthly_Subset$Market,Global_Superstore_Monthly_Subset$Segment), FUN=sd)
colnames(Standard_Deviation_Global_Superstore_Monthly_Subset)<-c("Market","Segment","StandardDeviationMonthlyProfitPercent")

# Adding SD of monthly profit percentage to the Sales.Summary.Total
Global_Superstore_Subset<-data.frame(Global_Superstore_Subset,Mean_Global_Superstore_Monthly_Subset,Standard_Deviation_Global_Superstore_Monthly_Subset)
Global_Superstore_Subset<-Global_Superstore_Subset[,-c(8,9,11,12)]
Global_Superstore_Subset$CoVarienceMonthlyProfitPercentage<-Global_Superstore_Subset$StandardDeviationMonthlyProfitPercent/Global_Superstore_Subset$AverageMonthlyProfitPercent

# Order on Total profit to find the most profitable and consistent Market Segment 
Global_Superstore_Subset<-Global_Superstore_Subset[order(Global_Superstore_Subset$TotalProfit,decreasing=TRUE),]



# Plotting Market Segment for maximum Total Profit

# Market and Segment generating most profit
Total_profit_plot<-ggplot(Global_Superstore_Subset,aes(x=Global_Superstore_Subset$Market,y=Global_Superstore_Subset$TotalProfit,fill=Global_Superstore_Subset$Segment))
Total_profit_plot+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Profit")+ggtitle("Total Profit")


# Market and Segment having most profit margin
COV_plot<-ggplot(Global_Superstore_Subset,aes(x=Global_Superstore_Subset$Market,y=Global_Superstore_Subset$CoVarienceMonthlyProfitPercentage,fill=Global_Superstore_Subset$Segment))
COV_plot<-COV_plot+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Coefficient Variance of Monthly Profit")
COV_plot+ggtitle("Coefficient Variance of Monthly Profit Vs. Market Segment")

# Based on the Maximum Total Profits and Consistentency month on month, These Market and Segments were chosen:
# 1. APAC Consumer
# 2. EU Consumer


#3 Model Building

#Modeling APAC Consumer sales
APAC_Consumer <- subset(Global_Superstore_Monthly_Subset,Segment == "Consumer" & Market == "APAC")
#Making sure the time series is in right order
APAC_Consumer <- APAC_Consumer[order(APAC_Consumer$Month),]
APAC_Consumer$Num <- c(1:nrow(APAC_Consumer))
#prepare sales data
APACCon_Sales <- APAC_Consumer[,c("Num","MonthlyTotalSales")] 
APACCon_Qty <-  APAC_Consumer[,c("Num","MonthlyTotalQuantity")]

#Seperating testand train data 
# 6 months data is used for evaluation
APACConSalesTest <- APACCon_Sales[c((nrow(APACCon_Sales)-5):nrow(APACCon_Sales)),]
APACConSalesTrain <- APACCon_Sales[c(1:(nrow(APACCon_Sales)-6)),]
APACConQtyTest <- APACCon_Qty[c((nrow(APACCon_Qty)-5):nrow(APACCon_Qty)),]
APACConQtyTrain <- APACCon_Qty[c(1:(nrow(APACCon_Qty)-6)),]


#create time series
APACConSalesTimeser <- ts(APACConSalesTrain[,2])
plot(APACConSalesTimeser)
APACConQtyTimeser <- ts(APACConQtyTrain[,2])
plot(APACConQtyTimeser)


#smoothening the series
#using w = 1
APACConSalesTimeserSmooth <- stats::filter(APACConSalesTimeser, 
                                     filter=rep(1/(2*1+1),(2*1+1)), 
                                     method='convolution', sides=2)


#Smoothing left end of the time series

diff <- APACConSalesTimeserSmooth[1+2] - APACConSalesTimeserSmooth[1+1]
for (i in seq(1,1,-1)) {
  APACConSalesTimeserSmooth[i] <- APACConSalesTimeserSmooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APACConSalesTimeser)
diff <- APACConSalesTimeserSmooth[n-1] - APACConSalesTimeserSmooth[n-1-1]
for (i in seq(n-1+1, n)) {
  APACConSalesTimeserSmooth[i] <- APACConSalesTimeserSmooth[i-1] + diff
}



#Plot the smoothed time series
plot(APACConSalesTimeser)
lines(APACConSalesTimeserSmooth, col="blue", lwd=2)

#filter using 
cols <- c("red", "yellow", "green", "black")
alphas <- c(0.02, 0.1,0.5, 0.8)
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(APACConSalesTimeser, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

#using the filter()  smoothning since it gives better fit

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- APACConSalesTrain$Num
AMACConSalessmootheddf <- as.data.frame(cbind(timevals_in, as.vector(APACConSalesTimeserSmooth)))
colnames(AMACConSalessmootheddf) <- c('Month', 'Sales')


#Now, let's fit a combination of multiplicative and additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
APACConSales_lmfit <- lm(AMACConSalessmootheddf$Sales ~ sin(0.5*AMACConSalessmootheddf$Month) *
              poly(AMACConSalessmootheddf$Month,2) 
            + cos(0.5*AMACConSalessmootheddf$Month) * 
              poly(AMACConSalessmootheddf$Month,2)
            + sin(0.05*AMACConSalessmootheddf$Month)*
              AMACConSalessmootheddf$Month, 
            data=AMACConSalessmootheddf)

#Changed LMFit after seeing Mape Value
APACConSales_lmfit <- lm(Sales ~ 
                cos(0.5*Month) * I(Month^2) 
              - I(Month^2)
              +sin(0.5*Month)
              - cos(0.5*Month)
              + Month, data=AMACConSalessmootheddf) 
summary(APACConSales_lmfit)
accuracy(APACConSales_lmfit)
AMACConSalesPredict <- predict(APACConSales_lmfit, Month=timevals_in)
lines(timevals_in, AMACConSalesPredict, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APACConSales <- APACConSalesTimeser-AMACConSalesPredict
plot(local_pred_APACConSales, col='red', type = "l")
acf(local_pred_APACConSales)
acf(local_pred_APACConSales, type="partial")
armafit_APACConSales <- auto.arima(local_pred_APACConSales)

tsdiag(armafit_APACConSales)
armafit_APACConSales


#We have just pure noise in local_pre_APACConSales

#checking if the residual series is white noise

resi_APACConSales <- local_pred_APACConSales-fitted(armafit_APACConSales)

adf.test(resi_APACConSales,alternative = "stationary")
kpss.test(resi_APACConSales)

#evaluating the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- APACConSalesTest$Num
global_pred_out <- predict(APACConSales_lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,APACConSalesTest[,2])[5]
MAPE_class_dec
#26.70807 which is good


#So, that was classical decomposition, now let's do an ARIMA fit

APACCon_Salesautoarima <- auto.arima(APACConSalesTimeserSmooth)
APACCon_Salesautoarima
tsdiag(APACCon_Salesautoarima)
plot(APACCon_Salesautoarima$x, col="black")
lines(fitted(APACCon_Salesautoarima), col="red")

#Again, let's check if the residual series is white noise

APACCon_sales_resi_auto_arima <- APACConSalesTimeserSmooth - fitted(APACCon_Salesautoarima)

adf.test(APACCon_sales_resi_auto_arima,alternative = "stationary")
kpss.test(APACCon_sales_resi_auto_arima)

#Also, let's evaluate the model using MAPE
APACCon_sales_fcast_auto_arima <- predict(APACCon_Salesautoarima, n.ahead = 6)

APACCon_sales_MAPE_auto_arima <- accuracy(APACCon_sales_fcast_auto_arima$pred,APACConSalesTest[,2])[5]
APACCon_sales_MAPE_auto_arima
#27.33866

#ploting the predictions along with original values

auto_arima_pred <- c(fitted(APACCon_Salesautoarima),ts(APACCon_sales_fcast_auto_arima$pred))
plot(APACConSalesTimeser, col = "black")
lines(auto_arima_pred, col = "red")





#####
#Modeling APAC Con Qty

#prepare sales data
APACCon_Qty <-  APAC_Consumer[,c("Num","MonthlyTotalQuantity")]


#create time series
APACConQtyTimeser <- ts(APACConQtyTrain[,2])
plot(APACConQtyTimeser)

#smoothening the series
#using w = 1
APACConQtyTimeserSmooth <- stats::filter(APACConQtyTimeser, 
                                           filter=rep(1/(2*1+1),(2*1+1)), 
                                           method='convolution', sides=2)


#Smoothing left end of the time series

diff <- APACConQtyTimeserSmooth[1+2] - APACConQtyTimeserSmooth[1+1]
for (i in seq(1,1,-1)) {
  APACConQtyTimeserSmooth[i] <- APACConQtyTimeserSmooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APACConQtyTimeser)
diff <- APACConQtyTimeserSmooth[n-1] - APACConQtyTimeserSmooth[n-1-1]
for (i in seq(n-1+1, n)) {
  APACConQtyTimeserSmooth[i] <- APACConQtyTimeserSmooth[i-1] + diff
}

#Plot the smoothed time series

#timevals_in <- indata$Month
lines(APACConQtyTimeserSmooth, col="blue", lwd=2)

#filter using 
cols <- c("red", "yellow", "green", "black")
alphas <- c(0.02, 0.1,0.5, 0.8)
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(APACConQtyTimeser, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

#using the filter()  smoothning since it gives better fit

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- APACConQtyTrain$Num
AMACConQtysmootheddf <- as.data.frame(cbind(timevals_in, as.vector(APACConQtyTimeserSmooth)))
colnames(AMACConQtysmootheddf) <- c('month', 'Qty')


#Now, let's fit a combination of multiplicative and additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit_q <- lm(Qty ~ 
                + sin(0.5*month) * month
              + sin(0.5*month) * I(month^2)
              + cos(0.5*month) * I(month^3)
              - cos(0.5*month) 
              - sin(0.5*month)
              - I(month^2)
              - I(month^3)
              + month, data=AMACConQtysmootheddf)


summary(lmfit_q)
accuracy(lmfit_q)
AMACConQtyPredict <- predict(lmfit_q, Month=timevals_in)
lines(timevals_in, AMACConQtyPredict, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APACConQty <- APACConQtyTimeser-AMACConQtyPredict
plot(local_pred_APACConQty, col='red', type = "l")
acf(local_pred_APACConQty)
acf(local_pred_APACConQty, type="partial")
armafit_APACConQty <- auto.arima(local_pred_APACConQty)

tsdiag(armafit_APACConQty)
armafit_APACConQty

#We have just pure noise in local_pre_APACConSales

#checking if the residual series is white noise

resi_APACConQty <- local_pred_APACConQty-fitted(armafit_APACConQty)

adf.test(resi_APACConQty,alternative = "stationary")
kpss.test(resi_APACConQty)

#evaluating the model using MAPE
#First, let's make a prediction for the last 6 months

APAConQty_timevals_out <- APACConQtyTest$Num
APACConQty_global_pred_out <- predict(lmfit_q,data.frame(month =APAConQty_timevals_out))

fcast <- APACConQty_global_pred_out
APACConQty_MAPE_class_dec <- accuracy(fcast,APACConQtyTest[,2])[5]
APACConQty_MAPE_class_dec



#So, that was classical decomposition, now let's do an ARIMA fit

APACCon_Qtyautoarima <- auto.arima(APACConQtyTimeserSmooth)
APACCon_Qtyautoarima
tsdiag(APACCon_Qtyautoarima)
plot(APACCon_Qtyautoarima$x, col="black")
lines(fitted(APACCon_Qtyautoarima), col="red")

#Again, let's check if the residual series is white noise

APACCon_Qty_resi_auto_arima <- APACConQtyTimeserSmooth - fitted(APACCon_Qtyautoarima)

adf.test(APACCon_Qty_resi_auto_arima,alternative = "stationary")
kpss.test(APACCon_Qty_resi_auto_arima)

#Also, let's evaluate the model using MAPE
APACCon_Qty_fcast_auto_arima <- predict(APACCon_Qtyautoarima, n.ahead = 6)

APACCon_Qty_MAPE_auto_arima <- accuracy(APACCon_Qty_fcast_auto_arima$pred,APACConQtyTest[,2])[5]
APACCon_Qty_MAPE_auto_arima

#ploting the predictions along with original values

APACConQty_auto_arima_pred <- c(fitted(APACCon_Qtyautoarima),ts(APACCon_Qty_fcast_auto_arima$pred))
plot(APACConQtyTimeser, col = "black")
lines(APACConQty_auto_arima_pred, col = "red")




#####
#EU

#Modeling EU Consumer sales
EU_Consumer <- subset(Global_Superstore_Monthly_Subset,Segment == "Consumer" & Market == "EU")
#Making sure the time series is in right order
EU_Consumer <- EU_Consumer[order(EU_Consumer$Month),]
EU_Consumer$Num <- c(1:nrow(EU_Consumer))
#prepare sales data
EUCon_Sales <- EU_Consumer[,c("Num","MonthlyTotalSales")] 
EUCon_Qty <-  EU_Consumer[,c("Num","MonthlyTotalQuantity")]

#Seperating testand train data 
# 6 months data is used for evaluation
EUConSalesTest <- EUCon_Sales[c((nrow(EUCon_Sales)-5):nrow(EUCon_Sales)),]
EUConSalesTrain <- EUCon_Sales[c(1:(nrow(EUCon_Sales)-6)),]
EUConQtyTest <- EUCon_Qty[c((nrow(EUCon_Qty)-5):nrow(EUCon_Qty)),]
EUConQtyTrain <- EUCon_Qty[c(1:(nrow(EUCon_Qty)-6)),]


#create time series
EUConSalesTimeser <- ts(EUConSalesTrain[,2])
plot(EUConSalesTimeser)
EUConQtyTimeser <- ts(EUConQtyTrain[,2])
plot(EUConQtyTimeser)


#smoothening the series
#smoothening the series
#using w = 1
EUConSalesTimeserSmooth <- stats::filter(EUConSalesTimeser, 
                                         filter=rep(1/(2*1+1),(2*1+1)), 
                                         method='convolution', sides=2)


#Smoothing left end of the time series

diff <- EUConSalesTimeserSmooth[1+2] - EUConSalesTimeserSmooth[1+1]
for (i in seq(1,1,-1)) {
  EUConSalesTimeserSmooth[i] <- EUConSalesTimeserSmooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EUConSalesTimeser)
diff <- EUConSalesTimeserSmooth[n-1] - EUConSalesTimeserSmooth[n-1-1]
for (i in seq(n-1+1, n)) {
  EUConSalesTimeserSmooth[i] <- EUConSalesTimeserSmooth[i-1] + diff
}

#Plot the smoothed time series
plot(EUConSalesTimeser)
lines(EUConSalesTimeserSmooth, col="blue", lwd=2)

#filter using 
cols <- c("red", "yellow", "green", "black")
alphas <- c(0.02, 0.1,0.5, 0.8)
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(EUConSalesTimeser, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

#using the lmfit()  smoothning since it gives better fit
#EUConSalesTimeserSmooth <- HoltWinters(EUConSalesTimeser, alpha=0.5,beta=FALSE, gamma=FALSE)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- EUConSalesTrain$Num
EUConSalessmootheddf <- as.data.frame(cbind(timevals_in, as.vector(EUConSalesTimeserSmooth)))
colnames(EUConSalessmootheddf) <- c('Month', 'Sales')


#Now, let's fit a combination of multiplicative and additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
EUConSales_lmfit <-  lm(Sales ~ poly(Month,3) 
                        +  cos(0.5*Month)
                        +   sin(0.5*Month)
                        +    sin(0.5*Month) * Month
                        + Month, data=EUConSalessmootheddf)
summary(EUConSales_lmfit)
accuracy(EUConSales_lmfit)
EUConSalesPredict <- predict(EUConSales_lmfit, Month=timevals_in)
lines(timevals_in, EUConSalesPredict, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EUConSales <- EUConSalesTimeser-EUConSalesPredict
plot(local_pred_EUConSales, col='red', type = "l")
acf(local_pred_EUConSales)
acf(local_pred_EUConSales, type="partial")
armafit_EUConSales <- auto.arima(local_pred_EUConSales)

tsdiag(armafit_EUConSales)
armafit_EUConSales


#We have just pure noise in local_pre_EUConSales

#checking if the residual series is white noise

resi_EUConSales <- local_pred_EUConSales-fitted(armafit_EUConSales)

adf.test(resi_EUConSales,alternative = "stationary")
kpss.test(resi_EUConSales)

#evaluating the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- EUConSalesTest$Num
global_pred_out <- predict(EUConSales_lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,EUConSalesTest[,2])[5]
MAPE_class_dec
#23.28411 which is good


#So, that was classical decomposition, now let's do an ARIMA fit

EUCon_Salesautoarima <- auto.arima(EUConSalesTimeserSmooth)
EUCon_Salesautoarima
tsdiag(EUCon_Salesautoarima)
plot(EUCon_Salesautoarima$x, col="black")
lines(fitted(EUCon_Salesautoarima), col="red")

#Again, let's check if the residual series is white noise

EUCon_sales_resi_auto_arima <- EUConSalesTimeserSmooth - fitted(EUCon_Salesautoarima)

adf.test(EUCon_sales_resi_auto_arima,alternative = "stationary")
kpss.test(EUCon_sales_resi_auto_arima)

#Also, let's evaluate the model using MAPE
EUCon_sales_fcast_auto_arima <- predict(EUCon_Salesautoarima, n.ahead = 6)

EUCon_sales_MAPE_auto_arima <- accuracy(EUCon_sales_fcast_auto_arima$pred,EUConSalesTest[,2])[5]
EUCon_sales_MAPE_auto_arima
#31.04337

#ploting the predictions along with original values

auto_arima_pred <- c(fitted(EUCon_Salesautoarima),ts(EUCon_sales_fcast_auto_arima$pred))
plot(EUConSalesTimeser, col = "black")
lines(auto_arima_pred, col = "red")





#####
#Modeling EU Con Qty

#prepare sales data
EUCon_Qty <-  EU_Consumer[,c("Num","MonthlyTotalQuantity")]


#create time series
EUConQtyTimeser <- ts(EUConQtyTrain[,2])
plot(EUConQtyTimeser)

#smoothening the series
#using w = 1
EUConQtyTimeserSmooth <- stats::filter(EUConQtyTimeser, 
                                       filter=rep(1/(2*1+1),(2*1+1)), 
                                       method='convolution', sides=2)


#Smoothing left end of the time series

diff <- EUConQtyTimeserSmooth[1+2] - EUConQtyTimeserSmooth[1+1]
for (i in seq(1,1,-1)) {
  EUConQtyTimeserSmooth[i] <- EUConQtyTimeserSmooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EUConQtyTimeser)
diff <- EUConQtyTimeserSmooth[n-1] - EUConQtyTimeserSmooth[n-1-1]
for (i in seq(n-1+1, n)) {
  EUConQtyTimeserSmooth[i] <- EUConQtyTimeserSmooth[i-1] + diff
}

#Plot the smoothed time series

#timevals_in <- indata$Month
lines(EUConQtyTimeserSmooth, col="blue", lwd=2)

#filter using 
cols <- c("red", "yellow", "green", "black")
alphas <- c(0.02, 0.1,0.5, 0.8)
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(EUConQtyTimeser, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

#using the filter()  smoothning since it gives better fit

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- EUConQtyTrain$Num
EUConQtysmootheddf <- as.data.frame(cbind(timevals_in, as.vector(EUConQtyTimeserSmooth)))
colnames(EUConQtysmootheddf) <- c('month', 'Qty')


#Now, let's fit a combination of multiplicative and additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit_q <- lm(Qty ~ 
                + sin(0.5*month) * month
              + sin(0.5*month) * I(month^2)
              + cos(0.5*month) * I(month^3)
              - cos(0.5*month) 
              - sin(0.5*month)
              - I(month^2)
              - I(month^3)
              + month, data=EUConQtysmootheddf)


summary(lmfit_q)
accuracy(lmfit_q)
EUConQtyPredict <- predict(lmfit_q, Month=timevals_in)
lines(timevals_in, EUConQtyPredict, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EUConQty <- EUConQtyTimeser-EUConQtyPredict
plot(local_pred_EUConQty, col='red', type = "l")
acf(local_pred_EUConQty)
acf(local_pred_EUConQty, type="partial")
armafit_EUConQty <- auto.arima(local_pred_EUConQty)

tsdiag(armafit_EUConQty)
armafit_EUConQty

#We have just pure noise in local_pre_EUConQty

#checking if the residual series is white noise

resi_EUConQty <- local_pred_EUConQty-fitted(armafit_EUConQty)

adf.test(resi_EUConQty,alternative = "stationary")
kpss.test(resi_EUConQty)

#evaluating the model using MAPE
#First, let's make a prediction for the last 6 months

EUonQty_timevals_out <- EUConQtyTest$Num
EUConQty_global_pred_out <- predict(lmfit_q,data.frame(month =EUonQty_timevals_out))

fcast <- EUConQty_global_pred_out
EUConQty_MAPE_class_dec <- accuracy(fcast,EUConQtyTest[,2])[5]
EUConQty_MAPE_class_dec
#32.21227


#So, that was classical decomposition, now let's do an ARIMA fit

EUCon_Qtyautoarima <- auto.arima(EUConQtyTimeserSmooth)
EUCon_Qtyautoarima
tsdiag(EUCon_Qtyautoarima)
plot(EUCon_Qtyautoarima$x, col="black")
lines(fitted(EUCon_Qtyautoarima), col="red")

#Again, let's check if the residual series is white noise

EUCon_Qty_resi_auto_arima <- EUConQtyTimeserSmooth - fitted(EUCon_Qtyautoarima)

adf.test(EUCon_Qty_resi_auto_arima,alternative = "stationary")
kpss.test(EUCon_Qty_resi_auto_arima)

#Also, let's evaluate the model using MAPE
EUCon_Qty_fcast_auto_arima <- predict(EUCon_Qtyautoarima, n.ahead = 6)

EUCon_Qty_MAPE_auto_arima <- accuracy(EUCon_Qty_fcast_auto_arima$pred,EUConQtyTest[,2])[5]
EUCon_Qty_MAPE_auto_arima
#30.91381

#ploting the predictions along with original values

EUConQty_auto_arima_pred <- c(fitted(EUCon_Qtyautoarima),ts(EUCon_Qty_fcast_auto_arima$pred))
plot(EUConQtyTimeser, col = "black")
lines(EUConQty_auto_arima_pred, col = "red")

