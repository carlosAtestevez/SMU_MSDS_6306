xBarVec = c() #Global vector to hold the sample means
population = rnorm(10000000,0,1) #Simulating the population


xbarGenerator = function(sampleSize = 30,number_of_samples = 100)
{
  for(i in 1:number_of_samples)
  {
    theSample = sample(population,sampleSize)
    xbar = mean(theSample)
    xBarVec = c(xBarVec, xbar)
    print(length(xBarVec))
  }
 
  return(xBarVec)
}

xbars = xbarGenerator(30,1000)
length(xbars)
hist(xbars)