
x = seq(from = 1, to = 5, length.out = 10)
y = seq(from = 100, to = 1000, by=100)
plot(x,y)

pairs(~x+y)

dev.off()
pairs(~mpg+disp+drat+wt,data=mtcars,
      main="Simple Scatterplot Matrix")

#B-->Significant rel MPG and Weight
plot(mtcars$mpg,mtcars$wt,main="MPG&WT",xlab = "MPG",ylab="WT")

#C-->scatterplot drat vs mpg Drat Increase and MPG decrease? -->FALSE
plot(mtcars$drat,mtcars$mpg,main="DRAT&MPG",xlab = "DRAT",ylab="MPG")

#D--> scatterplot weigth vs Disp, W INC and D-->INC?
plot(mtcars$wt,mtcars$disp,main="W&D",xlab = "W",ylab="Disp")


