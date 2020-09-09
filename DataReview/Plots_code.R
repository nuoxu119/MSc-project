Ori_data = read.csv("~/Desktop/A/output_pops.csv",header=FALSE)
Time_steps = 1:10001
data = cbind(Time_steps,Ori_data)
names(data) = c("Iteration","S1","S2")


plot(data$Iteration,data$S1,ylim=c(min(data$S1),max(data$S2)),ylab="Numbers alive", xlab = "Time steps",type="l",col="red",
     lwd=2,main = 'Plot for original dataA')

lines(data$Iteration,data$S2,col="blue",type="l",lwd=2)
legend("topright",legend = c("S1", "S2"), col = c("red", "blue"),lwd = 1, inset = 0.02, bty="n",cex=1)

datasth = data.frame(data[9000:10000,])

plot(datasth$Iteration,datasth$S1,ylim=c(min(datasth$S1),max(datasth$S2)),ylab="Numbers alive", xlab = "Time steps",type="l",col="red",
     lwd=2,main = 'Plot for original dataA after 9000 time steps')

lines(datasth$Iteration,datasth$S2,col="blue",type="l",lwd=2)
legend("topright",legend = c("S1", "S2"), col = c("red", "blue"),lwd = 1, inset = 0.02, bty="n",cex=1)

newx10000 = x10000-mean(x10000)
newx10000 = log(data10000[,2])
newy10000 = y10000-mean(y10000)
newy10000 = log(data10000[,3])
TS = 9200:9601
newdata10000 = data.frame(cbind(TS,newx10000,newy10000))

plot(newdata10000$TS,newdata10000$newx10000,ylim=c(min(newdata10000$newx10000),max(newdata10000$newy10000)),ylab="Numbers alive", xlab = "Time steps",type="l",col="red",
     lwd=2,main = 'Plot for data10000')

lines(newdata10000$TS,newdata10000$newy10000,col="blue",type="l",lwd=2)
legend("bottomright",legend = c("S1", "S2"), col = c("red", "blue"),lwd = 1, inset = 0.02, bty="n",cex=1)
