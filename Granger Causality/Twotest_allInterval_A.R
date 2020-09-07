grangertest(newx2000,newy2000,order=3)
#p-value is 0.078
grangertest(newx4000,newy4000,order=1)
#p-value is 3.101*10^-5
grangertest(newx6000,newy6000,order=3)
#p-value is 0.8637
grangertest(newx8000,newy8000,order=2)
#p-vallue is 0.1054
grangertest(newx10000,newy10000,order=4)
#p-value is 0.004068

var.A<-VAR(newdata2000,p=3,type = "const")
causality(var.A,cause = "newx2000")
#p-value is 0.03833
var.A<-VAR(newdata4000,p=1,type = "const")
causality(var.A,cause = "newx4000")
#p-value is 0.000795
var.A<-VAR(newdata6000,p=3,type = "const")
causality(var.A,cause = "newx6000")
#p-value is 0.002398
var.A<-VAR(newdata8000,p=2,type = "const")
causality(var.A,cause = "newx8000")
#p-value is 0.0005418
var.A<-VAR(newdata10000,p=4,type = "const")
causality(var.A,cause = "newx10000")
#p-value is 0.02678