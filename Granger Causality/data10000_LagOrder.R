
newx10000 = x10000-mean(x10000)
newx10000 = log(data10000[,2])
newy10000 = y10000-mean(y10000)
newy10000 = log(data10000[,3])
newdata10000 = data.frame(cbind(newx10000,newy10000))

IC=VARselect(y=newdata10000, type="none",lag.max=10)

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

#IC
#
#$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
 #    4      4      4      4 
