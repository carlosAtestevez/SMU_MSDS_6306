library(fpp)
#2 Holt's Linear Trend Model for AUS AIR
fit1h = holt(air, alpha = .8, beta = .2, initial = "simple", h = 5)
fit2h = holt(air, alpha = .8, beta = .2, initial = "simple", exponential = TRUE, h = 5)

# Check out estiamted values of the "training" data from the first holt model 
fitted(fit1h)
# Check out the forecast value (h of them)
fit1h$mean

# Reset the Plot!
plot(air,ylab = "Airline Passegners", xlab = "Year", type = "o", xlim = c(1990, 2009),ylim = c(15,60))
#Plot each models estimated values of the training data (Do these one by one to see the differences)
lines(fitted(fit1h),col = "blue", type= "o")
lines(fitted(fit2h), col = "red", type= "o")
#Plot each models forecasts (Do these one by one to see the differences)
lines(fit1h$mean, col = "blue", type= "o")
lines(fit2h$mean,col = "red", type= "o")

# Fit another model ... damped!  
fit3h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", h = 5)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit3h), col = "darkgreen", type= "o")
# Plot the forecasts
lines(fit3h$mean,col = "darkgreen", type= "o")

# Fit another model ... what is the difference?  
fit4h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", exponential = TRUE, h = 5)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit4h), col = "cyan", type= "o")
#Plot the forecasts
lines(fit4h$mean,col = "cyan", type= "o")

# with implicit Test set... it figures out by the time which are training and which are test. 
accuracy(fit1h, ausair)
accuracy(fit2h, ausair)
accuracy(fit3h, ausair)

#with explicit Test set ... (same output)
airTest = window(ausair, start = 2005)
accuracy(fit1h, airTest)
accuracy(fit2h, airTest)
accuracy(fit3h, airTest)

#Add the actual values to visually compare forecasts to actual values
air2008 = window(ausair, start = 1990, end = 2009)
points(air2008, type = "o")

