# File A: species 1 was prey. species 2 was predator 
library(vars)
library(lmtest)

###I re-arranged dataset to contain only first 50 time steps
data = read.csv("~/Desktop/A/output_pops.csv",header=FALSE)
data1 = data.frame(data[1:50,])
names(data1) = c("S1","S2")
Time_steps = 1:50
data2 = cbind(Time_steps,data1)

#x1 = log(data2[,2]) 
#y1 = log(data2[,3]) 

x = data2[,2]
y = data2[,3]

####plot data 

### I am not sure about the plot... Is that correct? 
## If it is right, does it mean that increasing of S2(red) at time 2 leads decreasing of S2(blue)?
## And then, can we conclude the prey-predator relationship by this result? or we need to find other evidence?
plot(Time_steps,x,ylim=c(min(x),max(y)),ylab="Numbers alive")
lines(Time_steps,x,col="blue",type="o",lwd=2,pch=16)
lines(Time_steps,y,col="red",type="o",lwd=2,pch=16)

####GC Test
### Here I got problem. It should be same in Python but it didn't...
## According to the result of lag selection, I think the best order here should set to 50.
## However, I couldn't set order to mor than 10, it gives error information:"Error in waldtest.lm(fm, 2, ...) : there are aliased coefficients in the model"
## Also when I try to set "order = 50", it gives different error information: "Error in dimnames(x) <- dn : length of 'dimnames' [2] not equal to array extent

grangertest(x,y,order=5) #test whether x causes y
grangertest(y,x,order=5) #test whether y causes x



