
newx10000 = x10000-mean(x10000)
newx10000 = log(data10000[,2])
newy10000 = y10000-mean(y10000)
newy10000 = log(data10000[,3])
newdata10000 = data.frame(cbind(newx10000,newy10000))

IC=VARselect(y=newdata10000, type="none",lag.max=10)
#IC
#The optimistic lag order for data10000 is 4. 

#Plot for [9200,9600]
criteria=scale(t(IC$criteria))
line_v=1:4 #line type 
plot_v=c(15,16,17,18) #plot type
col_v=c("black","red","green","blue") #color vector

plot(1:10,criteria[,1],ylab="Information criteriaeria",xlab="Number of lags",type="o",
     ylim=c(-4,4),col=col_v[1],pch=plot_v[1],lty=line_v[1])

for (i in 2:4){
  lines(1:10,criteria[,i],type="o",col=col_v[i],lty=line_v[i],pch=plot_v[i])
}

legend(2.5,4.5,legend=c("AIC","HQ","BIC","FPE"),col=col_v,pch = plot_v,cex = 0.7,box.lty=0)

newx2000 = x2000-mean(x2000)
newx2000 = log(data2000[,2])
newy2000 = y2000-mean(y2000)
newy2000 = log(data2000[,3])
newdata2000 = data.frame(cbind(newx2000,newy2000))
IC2000=VARselect(y=newdata2000, type="none",lag.max=10)
IC2000
#The optimistic lag order for data2000 is
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#10      3      1     10 

newx4000 = x4000-mean(x4000)
newx4000 = log(data4000[,2])
newy4000 = y4000-mean(y4000)
newy4000 = log(data4000[,3])
newdata4000 = data.frame(cbind(newx4000,newy4000))
IC4000=VARselect(y=newdata4000, type="none",lag.max=10)
IC4000
#The optimistic lag order for data4000 is
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#6      1      1      6 

newx6000 = x6000-mean(x6000)
newx6000 = log(data6000[,2])
newy6000 = y6000-mean(y6000)
newy6000 = log(data6000[,3])
newdata6000 = data.frame(cbind(newx6000,newy6000))
IC6000=VARselect(y=newdata6000, type="none",lag.max=10)
IC6000
#The optimistic lag order for data6000 is
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#3      2      1      3 

newx8000 = x8000-mean(x8000)
newx8000 = log(data8000[,2])
newy8000 = y8000-mean(y8000)
newy8000 = log(data8000[,3])
newdata8000 = data.frame(cbind(newx8000,newy8000))
IC8000=VARselect(y=newdata8000, type="none",lag.max=10)
IC8000

#The optimistic lag order for data8000 is
#AIC(n)  HQ(n)  SC(n) FPE(n) 
# 3      3      2      3 