#Try to test file.C: S2(predator) eat S1(prey), S3(pred) eat S2(prey), no relationship btw S1&S3
library(vars)
library(rEDM)

C_data = read.csv("~/Desktop/C/output_pops.csv",header=FALSE)
Time_steps = 1:10001
Cdata = cbind(Time_steps,C_data)
names(Cdata) = c("Iteration","S1","S2","S3")

Cdata10000= data.frame(Cdata[9000:10000,])
S1_10000 =Cdata10000[,2]
S2_10000 =Cdata10000[,3]
S3_10000 =Cdata10000[,4]

#Standardized data
S1new = (S1_10000-mean(S1_10000))/sd(S1_10000)
        
S2new = (S2_10000-mean(S2_10000))/sd(S2_10000)

S3new = (S3_10000-mean(S3_10000))/sd(S3_10000)

TS = 0:1000

Cdatanew1 = data.frame(cbind(S1new,S2new))
Cdatanew2 = data.frame(cbind(S1new,S3new))
Cdatanew3 = data.frame(cbind(S2new,S3new))
Cdatanew10000 = data.frame(cbind(TS,S1new,S2new,S3new))

#Plot for [9000,10000] before standardized data
plot(Cdata10000$Iteration,S1_10000,ylim=c(min(S3_10000),max(S2_10000)),ylab="Numbers alive", xlab="time steps",
     main ='Plot after transformation',type="l",lwd=2,col="green")
lines(Cdata10000$Iteration,S2_10000,col="red",type="l",lwd=2)
lines(Cdata10000$Iteration,S3_10000,col="blue",type="l",lwd=2)


#Plot for [9000,10000] after standardized data
plot(Cdatanew10000$TS,S1new,ylim=c(min(S1new),max(S2new)),ylab="Numbers alive", xlab="time steps",
     main ='Plot after transformation',type="l",lwd=2,col="green")
lines(Cdatanew10000$TS,S2new,col="red",type="l",lwd=2)
lines(Cdatanew10000$TS,S3new,col="blue",type="l",lwd=2)


#Granger Causality test
IC1=VARselect(y=Cdatanew1, type="none",lag.max=5) #lag order = 5
IC2=VARselect(y=Cdatanew2, type="none",lag.max=5) #lag order = 4
IC3=VARselect(y=Cdatanew3, type="none",lag.max=5) #lag order = 5

grangertest(S2new,S3new,order=5) # p-value is 0.004477,reject.
grangertest(S1new,S2new,order=5) # p-value is 0.001342,reject.
grangertest(S3new,S1new,order=4) # p-value is 0.09628, accept.

#Convergent Cross Mapping for S2 and S1
S2_S1= c(S1new,S2new)
lib_S1 = c(1,length(S1new))
lib_S2 = length(S1new) + c(1, length(S2new))

S2_interact_S1 = data.frame(1:length(S1new),S1new,S2new)
names(S2_interact_S1) = c("Time","prey","pred")

#Choose E: 6
simplex_out_S1 <- simplex(S2_S1, lib = lib_S1, pred = lib_S1, silent = TRUE)
best_E_S1 <- simplex_out_S1$E[which.max(simplex_out_S1$rho)]
copred_S1_to_S2 <- simplex(S2_S1, lib = lib_S1, pred = lib_S2, E = best_E_S1)
#Choose E: 2
simplex_out_S2 <- simplex(S2_S1, lib = lib_S2, pred = lib_S2, silent = TRUE)
best_E_S2 <- simplex_out_S2$E[which.max(simplex_out_S2$rho)]
copred_S2_to_S1 <- simplex(S2_S1, lib = lib_S2, pred = lib_S1, E = best_E_S2)

#To test whether Prey cause Pred (S2 causes S1)
S1_xmap_S2 <- ccm(S2_interact_S1, E =6, lib_col = "prey",
                      target_col = "pred", lib_sizes =seq(10,100,by=10), num_samples=100,replace=FALSE)
#To test whether Prey cause Pred (S1 causes S2)
S2_xmap_S1 <- ccm(S2_interact_S1, E =2, lib_col = "pred",
                      target_col = "pred", lib_sizes = seq(10,100,by=10), num_samples=100,replace=FALSE)

S1_xmap_S2_means <- data.frame(ccm_means(S1_xmap_S2),sd.rho = with(S1_xmap_S2,
                                                                           tapply(rho,lib_size,sd)))
S2_xmap_S1_means <- data.frame(ccm_means(S2_xmap_S1), sd.rho = with(S2_xmap_S1,
                                                                            tapply(rho, lib_size, sd)))
plot(S1_xmap_S2_means$lib_size, pmax(0, S1_xmap_S2_means$rho), type = "l", col = "red", ylab = "Cross Map Skill (rho)", ylim = c(0, 1.1), main = "CCM Plot for S2 and S1",xlab="Library size",xlim=c(10,100))                                   
lines(S2_xmap_S1_means$lib_size, pmax(0, S2_xmap_S1_means$rho), col = "blue")
legend("topright",legend = c("S1 xmap S2", "S2 xmap S1"), col = c("red", "blue"),lwd = 1, inset = 0.02, bty="n",cex=0.8)

lines(S1_xmap_S2_means$lib_size, S1_xmap_S2_means$rho + 2*S1_xmap_S2_means$sd.rho, col = "red",
      lty = 2, lwd = 2)
lines(S1_xmap_S2_means$lib_size, S1_xmap_S2_means$rho - 2*S1_xmap_S2_means$sd.rho, col = "red",
      lty = 2, lwd = 2)
lines(S2_xmap_S1_means$lib_size, S2_xmap_S1_means$rho + 2*S2_xmap_S1_means$sd.rho, col = "blue",
      lty = 2, lwd = 2)
lines(S2_xmap_S1_means$lib_size, S2_xmap_S1_means$rho - 2*S2_xmap_S1_means$sd.rho, col = "blue",
      lty = 2, lwd = 2)

#Convergent Cross Mapping for S3 and S2
S3_S2= c(S2new,S3new)
lib_S2 = c(1,length(S2new))
lib_S3 = length(S2new) + c(1, length(S3new))

S3_interact_S2 = data.frame(1:length(S2new),S2new,S3new)
names(S3_interact_S2) = c("Time","prey","pred")

#Choose E: 2
simplex_out_S22 <- simplex(S3_S2, lib = lib_S2, pred = lib_S2, silent = TRUE)
best_E_S22 <- simplex_out_S22$E[which.max(simplex_out_S22$rho)]
copred_S2_to_S3 <- simplex(S3_S2, lib = lib_S2, pred = lib_S3, E = best_E_S22)
#Choose E: 2
simplex_out_S3 <- simplex(S2_S1, lib = lib_S3, pred = lib_S3, silent = TRUE)
best_E_S3 <- simplex_out_S3$E[which.max(simplex_out_S3$rho)]
copred_S2_to_S1 <- simplex(S2_S1, lib = lib_S3, pred = lib_S2, E = best_E_S3)

#To test whether Prey cause Pred (S2 causes S1)
S2_xmap_S3 <- ccm(S3_interact_S2, E =2, lib_col = "prey",
                  target_col = "pred", lib_sizes =seq(10,100,by=10), num_samples=100,replace=FALSE)
#To test whether Prey cause Pred (S1 causes S2)
S3_xmap_S2 <- ccm(S3_interact_S2, E =2, lib_col = "pred",
                  target_col = "pred", lib_sizes = seq(10,100,by=10), num_samples=100,replace=FALSE)

S2_xmap_S3_means <- data.frame(ccm_means(S2_xmap_S3),sd.rho = with(S2_xmap_S3,
                                                                   tapply(rho,lib_size,sd)))
S3_xmap_S2_means <- data.frame(ccm_means(S3_xmap_S2), sd.rho = with(S3_xmap_S2,
                                                                    tapply(rho, lib_size, sd)))
plot(S2_xmap_S3_means$lib_size, pmax(0, S2_xmap_S3_means$rho), type = "l", col = "red", ylab = "Cross Map Skill (rho)", ylim = c(0, 1.1), main = "CCM Plot for S2 and S3",xlab="Library size",xlim=c(10,100))                                   
lines(S3_xmap_S2_means$lib_size, pmax(0, S3_xmap_S2_means$rho), col = "blue")
legend("topright",legend = c("S2 xmap S3", "S3 xmap S2"), col = c("red", "blue"),lwd = 1, inset = 0.02, bty="n",cex=0.8)

lines(S2_xmap_S3_means$lib_size, S2_xmap_S3_means$rho + 2*S2_xmap_S3_means$sd.rho, col = "red",
      lty = 2, lwd = 2)
lines(S2_xmap_S3_means$lib_size, S2_xmap_S3_means$rho - 2*S2_xmap_S3_means$sd.rho, col = "red",
      lty = 2, lwd = 2)
lines(S3_xmap_S2_means$lib_size, S3_xmap_S2_means$rho + 2*S3_xmap_S2_means$sd.rho, col = "blue",
      lty = 2, lwd = 2)
lines(S3_xmap_S2_means$lib_size, S3_xmap_S2_means$rho - 2*S3_xmap_S2_means$sd.rho, col = "blue",
      lty = 2, lwd = 2)

#Convergent Cross Mapping for S3 and S1
S3_S1= c(S1new,S3new)
lib_S1 = c(1,length(S1new))
lib_S3 = length(S1new) + c(1, length(S3new))

S3_interact_S1 = data.frame(1:length(S1new),S1new,S3new)
names(S3_interact_S1) = c("Time","prey","pred")

#Choose E: 6
simplex_out_S11 <- simplex(S3_S1, lib = lib_S1, pred = lib_S1, silent = TRUE)
best_E_S11 <- simplex_out_S11$E[which.max(simplex_out_S11$rho)]
copred_S1_to_S3 <- simplex(S3_S1, lib = lib_S1, pred = lib_S3, E = best_E_S11)
#Choose E: 2
simplex_out_S2 <- simplex(S2_S1, lib = lib_S3, pred = lib_S3, silent = TRUE)
best_E_S33 <- simplex_out_S2$E[which.max(simplex_out_S2$rho)]
copred_S3_to_S1 <- simplex(S2_S1, lib = lib_S3, pred = lib_S1, E = best_E_S33)

#To test whether Prey cause Pred (S2 causes S1)
S1_xmap_S3 <- ccm(S3_interact_S1, E =6, lib_col = "prey",
                  target_col = "pred", lib_sizes =seq(10,100,by=10), num_samples=100,replace=FALSE)
#To test whether Prey cause Pred (S1 causes S2)
S3_xmap_S1 <- ccm(S3_interact_S1, E =2, lib_col = "pred",
                  target_col = "pred", lib_sizes = seq(10,100,by=10), num_samples=100,replace=FALSE)

S1_xmap_S3_means <- data.frame(ccm_means(S1_xmap_S3),sd.rho = with(S1_xmap_S3,
                                                                   tapply(rho,lib_size,sd)))
S3_xmap_S1_means <- data.frame(ccm_means(S3_xmap_S1), sd.rho = with(S3_xmap_S1,
                                                                    tapply(rho, lib_size, sd)))
plot(S1_xmap_S3_means$lib_size, pmax(0, S1_xmap_S3_means$rho), type = "l", col = "red", ylab = "Cross Map Skill (rho)", ylim = c(0, 1.1), main = "CCM Plot for S1 and S3",xlab="Library size",xlim=c(10,100))                                   
lines(S3_xmap_S1_means$lib_size, pmax(0, S3_xmap_S1_means$rho), col = "blue")
legend("topright",legend = c("S1 xmap S3", "S3 xmap S1"), col = c("red", "blue"),lwd = 1, inset = 0.02, bty="n",cex=0.8)

lines(S1_xmap_S3_means$lib_size, S1_xmap_S3_means$rho + 2*S1_xmap_S3_means$sd.rho, col = "red",
      lty = 2, lwd = 2)
lines(S1_xmap_S3_means$lib_size, S1_xmap_S3_means$rho - 2*S1_xmap_S3_means$sd.rho, col = "red",
      lty = 2, lwd = 2)
lines(S3_xmap_S1_means$lib_size, S3_xmap_S1_means$rho + 2*S3_xmap_S1_means$sd.rho, col = "blue",
      lty = 2, lwd = 2)
lines(S3_xmap_S1_means$lib_size, S3_xmap_S1_means$rho - 2*S3_xmap_S1_means$sd.rho, col = "blue",
      lty = 2, lwd = 2)
