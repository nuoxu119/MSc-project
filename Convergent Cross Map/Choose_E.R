install.packages("rEDM")
library(vars)
library(rEDM)

X_ccm = log(data10000[,2])
Y_ccm = log(data10000[,3])
X_ccm = X_ccm - mean(X_ccm) #prey
Y_ccm = Y_ccm - mean(Y_ccm) #predator

S2_S1= c(X_ccm,Y_ccm)
  
lib_xccm = c(1,length(X_ccm))
lib_yccm = length(X_ccm) + c(1, length(Y_ccm))

#Chose the best E for "S1 cauese S2", which is E = 8 and the corresponding rho is 0.9949898.
simplex_out_S1 <- simplex(S2_S1, lib = lib_xccm, pred = lib_xccm, silent = TRUE)
best_E_S1 <- simplex_out_S1$E[which.max(simplex_out_S1$rho)]
copred_S1_to_S2 <- simplex(S2_S1, lib = lib_xccm, pred = lib_yccm, E = best_E_S1)

#Chose the best E for "S1 cauese S2", which is E = 7 and the corresponding rho is 0.9965082.
simplex_out_S2 <- simplex(S2_S1, lib = lib_yccm, pred = lib_yccm, silent = TRUE)
best_E_S2 <- simplex_out_S2$E[which.max(simplex_out_S2$rho)]
copred_S2_to_S1 <- simplex(S2_S1, lib = lib_yccm, pred = lib_xccm, E = best_E_S2)

#Draw plot
groups <- c("prediction of S1 (from S2)", 
            "coprediction of S1 (from S2)", 
            "prediction of S2 (from S2)", 
            "coprediction of S2 (from S1)")
to_plot <- data.frame(label = factor(groups, levels = groups), 
                      rbind(simplex_out_S1[which.max(simplex_out_S1$rho), ], 
                            copred_S2_to_S1, 
                            simplex_out_S2[which.max(simplex_out_S2$rho), ], 
                            copred_S1_to_S2)
)
library(ggplot2)
ggplot(to_plot, aes(x = label, y = rho)) + 
  geom_col() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


