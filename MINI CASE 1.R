library("forecast");library("ggplot2");library("readxl")
us_retail_data<-read_excel("POBF2eDataFiles/US_retail_sales_2.xlsx")
View(us_retail_data)
#understanding data :
retail_decomp<-decompose(retail_ts)
plot(retail_decomp)
##the data set has a somewhat linear trend and seasonality and random errors.

##setting the data into a time series
retail_ts<-ts(us_retail_data[,3][1],start=c(2001,1),frequency=12)
View(retail_ts)
autoplot(retail_ts)

####Setting an estimation sample 
retail_train<-window(retail_ts, start=c(2001,1),end=c(2012,12))
View(retail_train)

####### benchmark method : "Seasonal naive"
retail_snaive<-snaive(retail_ts,h=1)
summary(retail_snaive) #residual sd = 7.9236 #point_forecast= 348.575

####simple exponential smoothing : 
### we try to find out one step ahead forecast for the estimation sample which will
###help us to find the smoothing constant alpha .
retail_ses<-ses(retail_train,initial='simple',h=37)
####summary of retail_ses
summary(retail_ses)

#RMSE= 24.58442 #alpha=0.2097 

####checking whether there are white noise in the data set .
checkresiduals(retail_ses)## p value is less than signficance level so we reject
###the null hypothesis of Ljung box test. 

###graphical representation of retail_ses and the fitted value
autoplot(retail_ses,xlab="TIME",ylab="Sales($billions)")+autolayer(fitted(retail_ses))


###Defining the forecast function for SES
retail_fc_ses<-function(x,h){forecast(ses(x,initial="simple",alpha=0.2097, h=1))}

###determining the rolling forecast errors
retails_fce<-tsCV(retail_ts,forecastfunction = retail_fc_ses,h=1)

###determining the point forecast for Jan 2016
retail_fc_ses_pfc<-function(x,h){forecast(ses(retail_ts,initial="simple",alpha=0.2097, h=1))}
summary(retail_fc_ses_pfc())
###point forecast for Jan 2016 : 

###limiting forecast errors in test set
test_retails_fce<-subset(retails_fce,start=length(retails_fce)-36)
test_retail_mse<-mean(test_retails_fce^2,na.rm=TRUE)
test_retail_rmse<-sqrt(test_retail_mse)
test_retail_mae<-mean(abs(test_retails_fce),na.rm=TRUE)
test_retail_mape<-100*mean(abs(test_retails_fce)/lag(retail_ts,k=1), na.rm=TRUE)
print(test_retail_mse)
print(test_retail_rmse)
print(test_retail_mae)
print(test_retail_mape)
#RMSE (test)=28.33395 #MAE(TEST)=21.28052  MAPE=5.5493 #pointforecast(JAN 2016)-406.6095


### Addressing trend in the data
## linear exponential smoothing with damped trend :
### If there is damped trend in the data , the initialization will be 'optimal'.
retail_les<-holt(retail_train,initial='optimal', damped=TRUE,h=37)
summary(retail_les)
##parameters :alpha=0.0553 #beta = 0.0504 ##train RMSE : 23.60122 #MAE= 17.4903 MAPE=5.679 

####checking whether there are white noise in the data set .
checkresiduals(retail_les)## p value is less than signficance level so we reject
###the null hypothesis of Ljung box test.

###graphical representation of retail_ses and the fitted value
autoplot(retail_les,xlab="TIME",ylab="Sales($billions)",col='purple')+autolayer(fitted(retail_les),col = 2)


#######Defining the forecast function for LES with damped trend
retail_fc_les<-function(x,h){forecast(holt(x,alpha=0.0553,beta=0.0504,damped = TRUE),h=1)}

#####determining the rolling forecast errors
retails_fce_les<-tsCV(retail_ts,forecastfunction = retail_fc_les,h=1)

###determining the point forecast for Jan 2016
retail_fc_les_pfc<-function(x,h){forecast(holt(retail_ts,damped=TRUE,alpha=0.0504,beta=0.0504),h=1)}
summary(retail_fc_les_pfc())
####point forecast jan 2016 :399.4967

###limiting forecast errors in test set

testdata_retails_fce_les<-subset(retails_fce_les,start=length(retails_fce_les)-36)
test_retail_mse_les<-mean(testdata_retails_fce_les^2,na.rm=TRUE)
test_rmse_les<-sqrt(test_retail_mse_les)
test_rmse_les
test_retail_mae_les<-mean(abs(testdata_retails_fce_les),na.rm=TRUE)
test_retail_mae_les
test_retail_mape_les<-100*mean(abs(testdata_retails_fce_les)/lag(retail_ts,k=1), na.rm=TRUE)
print(test_retail_mape_les)
###RMSE(test)=27.39009 #MAE(test)=19.7212 #MAPE(test)=5.15261 


####Addressing random errors . trend and multiplicative seasonality
####holtwinters (multiplicative)
retail_hw_multi<-hw(retail_train,damped = TRUE,seasonal = 'multiplicative',initial='optimal',h=37)
summary(retail_hw_multi) 

####parameters : ####Train RMSE=6.215 MAE=4.873 MAPE=1.5768 point forecast=343.1732
###   alpha = 0.6221 #beta  = 0.026 
#gamma = 0.4599

checkresiduals(retail_hw)### p value is less than signficance level ,so we reject
###the null hypothesis of Ljung box test.

###graphical representation of retail_hw and the fitted value
autoplot(retail_hw,xlab="TIME",ylab="Sales($billions)",col='purple')+autolayer(fitted(retail_hw),col = 2)

### defining the function for holt winter multiplicative
retail_fc_hw_multi<-function(x,h){forecast(hw(x,damped=TRUE,initial='optimal',seasonal = "multiplicative",alpha = 0.6221,beta=0.026,gamma = 1e-04,h=h))}
                                           
#####determining the rolling forecast errors
retails_fce_hw_multi<-tsCV(retail_ts,forecastfunction = retail_fc_hw_multi,h=1)

####determining point forecast for jan 2016
retail_fc_hw_multi<-function(x,h){forecast(hw(retail_ts,damped=TRUE,initial='optimal',seasonal = "multiplicative",alpha = 0.6221,beta=0.026,gamma = 1e-04,h=1))}
summary(retail_fc_hw_multi())
###point forecast for jan 2016 : 358.6691


###limiting forecast errors in test set
testdata_retails_fce_hw_multi<-subset(retails_fce_hw_multi,start=length(retails_fce_hw_multi)-36)
test_retail_mse_hw_multi<-mean(testdata_retails_fce_hw_multi^2 , na.rm=TRUE)
test_rmse_hw_multi<-sqrt(test_retail_mse_hw_multi)
print(test_rmse_hw_multi)
test_retail_mae_hw_multi<-mean(abs(testdata_retails_fce_hw_multi),na.rm=TRUE)
print(test_retail_mae_hw_multi)
test_retail_mape_hw_multi<-100*mean(abs(testdata_retails_fce_hw_multi)/lag(retail_ts,k=1), na.rm=TRUE)
print(test_retail_mape_hw_multi)
###RMSE_hw_multi(test)=5.243027 ### MAE=4.625851 ###MAPE=1.218375



####Addressing random errors . trend and multiplicative seasonality
###holtwinters (additive)
retail_hw_add<-hw(retail_train,damped=TRUE,seasonal='additive',initial='optimal',h=37)
summary(retail_hw_add)
###parameters: alpha = 0.6323 beta  = 0 gamma = 0.4087 
#train RMSE = 6.407 MAE=5.108 MAPE=1.661


checkresiduals(retail_hw_add)### p value is less than signficance level so we reject
###the null hypothesis of Ljung box test.


###graphical representation of retail_hw and the fitted value
autoplot(retail_hw_add,xlab="TIME",ylab="Sales($billions)",col='purple')+autolayer(fitted(retail_hw_add),col = 2)

retail_fc_hw_add<-function(x,h){forecast(hw(x,damped=TRUE,initial='optimal',seasonal = "additive",alpha = 0.5283,beta=0.054,gamma = 1e-04,h=1))}

#####determining the rolling forecast errors
retails_fce_hw_add<-tsCV(retail_ts,forecastfunction = retail_fc_hw_add,h=1)

####determining point forecast for jan 2016
retail_fc_hw_add<-function(x,h){forecast(hw(retail_ts,damped=TRUE,initial='optimal',seasonal = "additive",alpha = 0.5283,beta=0.054,gamma = 1e-04,h=1))}
summary(retail_fc_hw_add())
####point forecast hw_add = 372.7058


###limiting forecast errors in test set
testdata_retails_fce_hw_add<-subset(retails_fce_hw_add,start=length(retails_fce_hw_add)-36)
test_retail_mse_hw_add<-mean(testdata_retails_fce_hw_add^2 , na.rm=TRUE)
test_rmse_hw_add<-sqrt(test_retail_mse_hw_add)
print(test_rmse_hw_add)
test_retail_mae_hw_add<-mean(abs(testdata_retails_fce_hw_add),na.rm=TRUE)
print(test_retail_mae_hw_add)
test_retail_mape_hw_add<-100*mean(abs(testdata_retails_fce_hw_add)/lag(retail_ts,k=1), na.rm=TRUE)
print(test_retail_mape_hw_add)
###RMSE_hw_add(test)=7.208 ### MAE=5.34025 ###MAPE=1.405194


#### different estimation sample
retail_train_data<-window(retail_ts, start=c(2010,1),end=c(2013,12))
retail_hw_multi_diff_es<-hw(retail_train_data,damped = TRUE,seasonal = 'multiplicative',initial='optimal',h=25)
summary(retail_hw_multi_diff_es)

retail_train_data2<-window(retail_ts, start=c(2004,1),end=c(2014,12))
retail_hw_multi_diff_es2<-hw(retail_train_data2,damped = TRUE,seasonal = 'multiplicative',initial='optimal',h=13)
summary(retail_hw_multi_diff_es2)


####Calculations for appendix
