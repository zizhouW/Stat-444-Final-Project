xTrain = seq(1,100)
yTrain = 2*xTrain#(xTrain-50)^2
xTest = seq(101,105)
yTest = 2*xTest
Traindata = data.frame(xTrain, yTrain)
library(gbm)
boost <-gbm(formula = yTrain~xTrain,data=Traindata,shrinkage =0.7,n.trees =1500)
pred= predict(object = boost,
                              newdata = list(xTrain = xTest),
                              n.trees = 1500,
                              type = "response")
pred

#??????work! fit?????????!