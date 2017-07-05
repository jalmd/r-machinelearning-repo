library(cvTools)

set.seed(1234) # set seed for reproducibility
cvFolds(20, K = 5, type = "random")
#cvFolds(20, K = 5, type = "consecutive")
#cvFolds(20, K = 5, type = "interleaved")
cvFolds(64, K = 10, R = 64)

folds <- cvFolds(10, K = 5, R = 50, type = "random")
print(folds)



x5 <- sample(1:10, 10, replace=F)
print (x5)