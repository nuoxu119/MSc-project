# File A: S1 is prey, S2 is predator 

library(vars)
library(lmtest)

# Open the original dataset (fileA: output_pops.csv) and add header (Iteration,S1,S2)
Ori_data = read.csv("~/Desktop/A/output_pops.csv",header=FALSE)
Time_steps = 1:10001
data = cbind(Time_steps,Ori_data)
names(data) = c("Iteration","S1","S2")


#### DATA TRANSFORMATION
### Step1: Divided 10000 time steps into several intervals.

data2000 = data.frame(data[1:2000,])
data4000 = data.frame(data[2001:4000,])
data6000 = data.frame(data[4001:6000,])
data8000 = data.frame(data[6001:8000,])
data10000 = data.frame(data[9200:9600,])

# X represents S1, Y represents S2
x2000 = data2000[,2] 
y2000 = data2000[,3]
x4000 = data4000[,2]
y4000 = data4000[,3]
x6000 = data6000[,2]
y6000 = data6000[,3]
x8000 = data8000[,2]
y8000 = data8000[,3]
x10000 = data10000[,2]
y10000 = data10000[,3]

### Step 2: lag/Model order selection -> Choose 'data10000' as the targeted interval. 

## Result: I have tested 'lag.max=2,10,100,500,1000' and the best selection based on SC(n) for each one were 2,8,5,4,1.

# Q1: Which is the best choice to set lag.max? 
# Q2: I used each of them as 'order' number in 'grangertest()', all results showed that p-value were quite smaller than 0.05.
#So under this situation, can I ignore the influence of model order? 
# Q3: Is that correct by doing data transformation after dividing intervals? or they are kind of 'repeated' steps?

newx6000 = x6000-mean(x6000)
newx6000 = log(data6000[,2])
newy6000 = y6000-mean(y6000)
newy6000 = log(data6000[,3])
newdata6000 = data.frame(cbind(newx6000,newy6000)) 
  
  
newx10000 = x10000-mean(x10000)
newx10000 = log(data10000[,2])
newy10000 = y10000-mean(y10000)
newy10000 = log(data10000[,3])
newdata10000 = data.frame(cbind(newx10000,newy10000))

IC=VARselect(y=newdata10000, type="none",lag.max=100)

### Step 3: Bivariate Granger causality test

## Null Hypothesis: Species 2 in every specific iteration interval does NOT Granger cause Species 1.
## Result: The species 2 Granger cause the species 1. To put it more generally, species 2 are predictive of future species 1. 
# I also play grangertest() for other 4 data sets,and get the same result. 

#Q4: How to detect the direction of effect?

grangertest(newx10000,newy10000,order=8) # p-value is 1.601e-11, Reject.
grangertest(x10000,y10000,order=8) # p-value is 2.058e-11, Reject.


### Step 4: plot

#Q5: Is this plot correct? If it is, why I couldn't read the prey-predator relationship directly from the plot? 
#If there is something wrong with my plot, could you help to figure out this part? 
# I tried different combination in ylim=, but it didn't help. 

plot(data10000$Iteration,x10000,ylim=c(min(x10000),max(y10000)),ylab="Numbers alive",
     main = 'General Plot for 9200-9600 iterations')
lines(data10000$Iteration,x10000,col="lightskyblue3",type="o",lwd=0.5,pch=19)
lines(data10000$Iteration,y10000,col="orange1",type="o",lwd=0.5,pch=19)

#Also, print out the result of instantaneous causality test 
#Granger p<0.05; Instant P<0.05(0.026)
var.A<-VAR(newdata10000,p=4,type = "const")
causality(var.A,cause = "newy10000")
#Granger p<0.05; Instant P >> 0.05(3.331e-16)
var.A<-VAR(newdata6000,p=4,type = "const")
causality(var.A,cause = "newy6000")
