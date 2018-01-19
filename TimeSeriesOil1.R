#this code snippet takes the oil dataset from fpp2 package and compares various time series model to see which one fits better and has minimum RMSE.

library(fpp2)
autoplot(oil)
gglagplot(oil)
ggAcf(oil)
#significant correlation with last 4 lags, it can be infered that the current oil production is
#significantly correlated with last 3-4 oil productions
Box.test(oil, lag= 10, type="Ljung")
# in Ljung-Box test the p-value is very small, also stating that this is not a white noise type series

oiltrain<- window(oil, end=2008)
oilnaivefc<- naive(oiltrain, h=5)

#forecasting using naive method

oilsnaivefc<- snaive(oiltrain, h=5)
autoplot(oiltrain,series="Data")+autolayer(fitted(oilnaivefc),series= "Fitted")

autoplot(oiltrain,series="Data")+autolayer(fitted(oilsnaivefc),series= "Fitted")

oilexpsfc<- ses(oiltrain, h= 5)

oilholtfc<- holt(oiltrain, damped = TRUE, h=5)

#oilhwfc<- hw(oiltrain,seasonal = "multiplicative", h=5)

#you can check for an appropriate ETS model by applying ets function, and then using forecast
oiletsfc<- forecast(ets(oiltrain), h=5)

#check cross validation and also check mean squared values
cv1<- tsCV(oiltrain, oiletsfc, h=5)

#ARIMA models 
BoxCox.lambda(oiltrain)
#take lambda value from the box-cox transform value
oilarima<- auto.arima(oiltrain, lambda =0.1217709 , stepwise = FALSE)

summary(oilarima)

#check accuracy for all the models so far 
accuracy(oilnaivefc,oil)
accuracy(oilsnaivefc, oil)
accuracy(oilexpsfc,oil)
accuracy(oilholtfc,oil)
accuracy(forecast(oiletsfc,h=5),oil)
accuracy(forecast(oilarima,h=5),oil)

#plot all on each other
autoplot(oiltrain,series="Data")+autolayer(fitted(oilarima),series= "Fitted")

# predict values and compare
#the best model according to least RMSE value is the oilarima model

#forecast values
forecast(oilarima, h=5)


