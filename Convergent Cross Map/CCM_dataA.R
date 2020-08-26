install.packages("rEDM")
library(vars)
library(rEDM)


X_ccm = log(data10000[,2])
Y_ccm = log(data10000[,3])
X_ccm = X_ccm - mean(X_ccm) #prey
Y_ccm = Y_ccm - mean(Y_ccm) #predator

pred_prey = data.frame(1:length(X_ccm),X_ccm,Y_ccm)
names(pred_prey) = c("Time","prey","pred")

#To test whether Pred(Y) causes Prey(X).
#Determine the best embedding dimension for cross mapping.
#1. chose a fixed libraru size: (lib_sizes = 400)
#2. use the time lages of X to predic the lagged on time set value of Yby setting the augment tp=-1.
#3. determine the optimal E.

prey_xmap_pred <- ccm(pred_prey, E =3, lib_col = "prey",
                      target_col = "pred", lib_sizes =seq(10,100,by=10), num_samples=100,replace=FALSE)
                      
                      
#To test whether Prey cause Pred
pred_xmap_prey <- ccm(pred_prey, E =3, lib_col = "pred",
                      target_col = "pred", lib_sizes = seq(10,100,by=10), num_samples=100,replace=FALSE)

prey_xmap_pred_means <- data.frame(ccm_means(prey_xmap_pred),sd.rho = with(prey_xmap_pred,
                                                                           tapply(rho,lib_size,sd)))
pred_xmap_prey_means <- data.frame(ccm_means(pred_xmap_prey), sd.rho = with(pred_xmap_prey,
                                                                            tapply(rho, lib_size, sd)))
                                   
plot(prey_xmap_pred_means$lib_size, pmax(0, prey_xmap_pred_means$rho), type = "l", col = "red", ylab = "Cross Map Skill (rho)", ylim = c(0, 1.1), main = "Cenvergent Cross Mapping",xlab="Library size",xlim=c(10,100))                                   
lines(pred_xmap_prey_means$lib_size, pmax(0, pred_xmap_prey_means$rho), col = "blue")
legend("bottomright",legend = c("prey xmap pred", "pred xmap prey"), col = c("red", "blue"),lwd = 1, inset = 0.02, bty="n",cex=0.8)


lines(prey_xmap_pred_means$lib_size, prey_xmap_pred_means$rho + 2*prey_xmap_pred_means$sd.rho, col = "red",
      lty = 2, lwd = 2)
lines(prey_xmap_pred_means$lib_size, prey_xmap_pred_means$rho - 2*prey_xmap_pred_means$sd.rho, col = "red",
      lty = 2, lwd = 2)
lines(pred_xmap_prey_means$lib_size, pred_xmap_prey_means$rho + 2*pred_xmap_prey_means$sd.rho, col = "blue",
      lty = 2, lwd = 2)
lines(pred_xmap_prey_means$lib_size, pred_xmap_prey_means$rho - 2*pred_xmap_prey_means$sd.rho, col = "blue",
      lty = 2, lwd = 2)
                                  
