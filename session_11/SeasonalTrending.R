library(fpp)

#3. Seasonal Trend

#Load the data
data("austourists")
# Read about the dataset!
?austourists


# Always plot the data first!
plot(austourists)

# returns a ts object.  
aust = window(austourists,start = 1999, end = 2004)

#fit an additive and multiplicative model
fit1s = hw(aust,seasonal = "additive",h = 40)
fit2s = hw(aust,seasonal = "multiplicative",h = 40)

#Plot the original data
plot(aust,ylab = "Australian Tourists", xlab = "Year", type = "o", xlim = c(1999, 2014),ylim = c(15,60))
#add the fitted values from the model (of the training data)
lines(fitted(fit1s),col = "blue", type= "o")
lines(fitted(fit2s), col = "red", type= "o")

#Now add the forecasts (add these one at a time)
lines(fit1s$mean, col = "blue", type= "o")
lines(fit2s$mean,col = "red", type= "o")

#Compare the accuracy
accuracy(fit1s,austourists)
accuracy(fit2s,austourists)

#add the actual values to visually compare the forecasts to the actual values. 
points(austourists, type = "o")
