install.packages("lmtest")
install.packages("urca")
install.packages("sandwich")
install.packages("strucchange")
install.packages("vars")
library(vars)

data = read.csv("~/Desktop/A/output_pops.csv")
x = log(data[,1])
x = x-mean(x)
y = log(data[,2])
y = y-mean(y)
newdata = data.frame(cbind(x,y))
names(newdata) = c("x","y")

#Here I test lag.max=20/50/100 and results show that the optimal lag number is almost 20,50,100 by all information criteria. 
#Except the SC(n) in 100, the optimal number here is 40. 

#So how to explain this? Should we ignore the SC(n)?
#it might be that performance of this model applied in the simulated data will be better when there are more time lags. 
IC=VARselect(y=newdata, type="none",lag.max=30)

criteria=scale(t(IC$criteria))
line_v=1:4 #line type 
plot_v=c(15,16,17,18) #plot type
col_v=c("wheat4","mediumvioletred","orange1","lightskyblue3") #color

plot(1:30,criteria[,1],ylab="Information criteriaeria",xlab="Number of lags",type="o",
     ylim=c(-2,2),col=col_v[1],pch=plot_v[1],lty=line_v[1])

for (i in 2:4){
  lines(1:30,criteria[,i],type="o",col=col_v[i],lty=line_v[i],pch=plot_v[i])
  }


