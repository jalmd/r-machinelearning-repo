data(iris)
attach(iris)
library(e1071)

source('functions.R')

numFeatures <- 128
population <- 'B-cell'

files <- list.files(path = "trainingFiles", full.names = T, recursive = F, pattern = '*.rds')

densXchanA <- matrix(NaN, ncol = numFeatures, nrow = length(files))
densYchanA <- matrix(NaN, ncol = numFeatures, nrow = length(files))
densXchanB <- matrix(NaN, ncol = numFeatures, nrow = length(files))
densYchanB <- matrix(NaN, ncol = numFeatures, nrow = length(files))
threshA <- matrix(NaN, ncol = 2, nrow = length(files))
threshB <- matrix(NaN, ncol = 2, nrow = length(files))

samples <- list()

#classification_matrix <- matrix(NaN, ncol= 2, nrow = numFeatures*length(files))
numFiles <- 200
numTest <- 20
classification_matrix <- matrix(NaN, ncol= 2, nrow = numFeatures*numFiles)
classification_matrix_test <- matrix(NaN, ncol= 2, nrow = numFeatures*numTest)

thresh_A <- matrix(NaN, ncol= 2, nrow = numTest)

#classification_matrix[1,2] <- 1
#print (classification_matrix)

class_vector <- c()
class_vector_test <- c()
i <- 1
k <- 1
l <- 1
for (f in files)
{
  print(i)
  
  s <- readRDS(f)
  samples[[f]] <- s
  densXchanA[i, ] <- s[[population]]@densitiesA[[as.character(numFeatures)]]$x
  densYchanA[i, ] <- s[[population]]@densitiesA[[as.character(numFeatures)]]$y
  
  
  densXchanB[i, ] <- s[[population]]@densitiesB[[as.character(numFeatures)]]$x
  densYchanB[i, ] <- s[[population]]@densitiesB[[as.character(numFeatures)]]$y
  
  
  threshA[i, ] <- c(s[[population]]@thresholdALow, s[[population]]@thresholdAHigh)
  threshB[i, ] <- c(s[[population]]@thresholdBLow, s[[population]]@thresholdBHigh)
  
  #plotGate(s[[population]], 128)
  lowA <- getLowThresholdA(s[[population]], numFeatures)
  highA <- getHighThresholdA(s[[population]], numFeatures)
  
  lowB <- getLowThresholdB(s[[population]], numFeatures)
  highB <- getHighThresholdB(s[[population]], numFeatures)
  
  #print ("LOWA")
  #print (lowA)
  #print ("HIGHA")
  #print (highA)
  
  #print ("LOWB")
  #print (lowB)
  #print ("HIGHB")
  #print (highB)
  
  xA <- s[[population]]@densitiesA[[as.character(numFeatures)]]$x
  yA <- s[[population]]@densitiesA[[as.character(numFeatures)]]$y
  
  xB <- s[[population]]@densitiesB[[as.character(numFeatures)]]$x
  yB <- s[[population]]@densitiesB[[as.character(numFeatures)]]$y
  
  
  if (i > numFiles){
    if (i > numTest+numFiles){
      break
    }
    print(thresh_A)
    thresh_A[i-numFiles,1] <- lowA
    thresh_A[i-numFiles,2] <- highA
    print("opa")
    j <- 1
    for (element in xA){
      if (element > lowA && element < highA){
        class_vector_test <- c(class_vector_test, "inside")
      }
      else if (element < lowA && element < highA){
        class_vector_test <- c(class_vector_test, "left")
      }
      else{
        class_vector_test <- c(class_vector_test, "right")
      }
      classification_matrix_test[l,1] <- element
      classification_matrix_test[l,2] <- yA[j]
      j <- j + 1
      l <- l + 1
    }

  }
  else{
    j <- 1
    for (element in xA){
      if (element > lowA && element < highA){
        class_vector <- c(class_vector, "inside")
      }
      else if (element < lowA && element < highA){
        class_vector <- c(class_vector, "left")
      }
      else{
        class_vector <- c(class_vector, "right")
      }
      classification_matrix[k,1] <- element
      classification_matrix[k,2] <- yA[j]
      j <- j + 1
      k <- k + 1
    }
    #print(classification_matrix)
    #print(class_vector)
  }
  i <- i + 1
}


#print(classification_matrix)
#print(class_vector)
class_vector <- factor(class_vector)
#print(class_vector)

#print (densYchanA)
perm <- sample(length(samples))

samples <- samples[perm]
densXchanA <- densXchanA[perm, ]
densYchanA <- densYchanA[perm, ]
densXchanB <- densXchanB[perm, ]
densYchanB <- densYchanB[perm, ]
threshA <- threshA[perm, ]
threshB <- threshB[perm, ]

print (thresh_A)


#================================================================================================================================



#dirName <- 'trainingFiles'
#sampleName <- 'BM 8,2f,19,2f,15_L000095420_001.labelled'
#sample <- readRDS(paste(dirName, '/', sampleName, '.rds', sep = ''))

# Accessing data

# Print list of populations
#print(names(sample))

#print(class(sample[[population]]))

#print(slotNames("TrainingGate"))

#plotGate(sample[[population]], numFeatures)

# Vectors:
# x = index
# y = value
#x <- sample[[population]]@densitiesA[[as.character(numFeatures)]]$x
#y <- sample[[population]]@densitiesA[[as.character(numFeatures)]]$y



#high <- getHighThreshold(sample[[population]], numFeatures)
#low <- getLowThreshold(sample[[population]], numFeatures)

#print (high)
#print (low)


#outside <- getOutsideThresholdVector(x, high, low)

#inside_y <- getInsideThresholdVector(x,y , high, low)
#features_inside <- length(inside_y)

#print (features_inside)
#print(inside_y)

#findPeaks(inside_y)

##print (x)
##print (y)
#print(high)
#print(low)
#class_vector1 <- getClassVector(x, y, high, low)
#print (class_vector1)
#get thresholds (high and low)
#get outside threshold
#get inside threshold vector
#count features inside and features outside
#find peaks inside threshold and outside (using previous functions together)





#==========================================================================================================================================





## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species

x <- classification_matrix
y <- class_vector



#cvfit = cv.glmnet(densYchanA[1:numTrain, ], threshA[1:numTrain, 1])
#predicted <- predict(cvfit, newx = densYchanA[-(1:numTrain),], s = "lambda.min")
numTrain <- numFiles
model <- svm(x, y,  probability = TRUE) 

#print (x)
#print (y)

print(model)
summary(model)


# test with train data
pred <- predict(model, classification_matrix_test)
# (same as:)
#pred <- fitted(model)

# Check accuracy:
#table(pred, y)
print(pred)
print(class_vector_test)
table(pred, class_vector_test)


low_p <- c()
high_p <- c()
highest <- -1000
lowest <- 1000
highest_index <- 0
lowest_index <- 0
i <- 1
j <- 1
for (factor_value in pred){
  if(as.character(factor_value) == "inside"){
    if(classification_matrix[i,1] < lowest){
      lowest <- classification_matrix[i,1]
      lowest_index <- j
    }
    else if(classification_matrix[i,1] > highest){
      highest <- classification_matrix[i,1]
      highest_index <- j
    }
  }
  i <- i + 1
  j <- j + 1
  if (i > numFeatures || j > length(pred)){
    high_p <- c(high_p, highest_index)
    low_p <- c(low_p, lowest_index)
    i <- 0
    highest <- -1000
    lowest <- 1000
  }
}


print (low_p)
print (high_p)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE, probability = TRUE)
attr(pred, "decision.values")[1:2,]
attr(pred, "probabilities")[1:2,]



par(mfrow = c(4,5))
i <- 1
#for (i in (numTrain+1):length(samples))
j <- 1
while(i < numTest+1)
{
  plot(
    samples[[i+numFiles]][[population]]@densitiesA[[as.character(numFeatures)]]$x,
    samples[[i+numFiles]][[population]]@densitiesA[[as.character(numFeatures)]]$y,
    type = 'o'
  );
  predicted_low <- classification_matrix[low_p[j],1]
  predicted_high <- classification_matrix[high_p[j],1]
  abline(v = thresh_A[j, 1], col = 'red', lwd = 2, lty = 2)
  abline(v = thresh_A[j, 2], col = 'red', lwd = 2, lty = 2)
  abline(v = predicted_low, col = 'blue', lwd = 2, lty = 2)
  abline(v = predicted_high, col = 'blue', lwd = 2, lty = 2)
  #abline(v = predicted[i-299], col = 'blue', lwd = 2, lty = 2)
  
  if ((i - numTrain) %% 20 == 0)
  {
    readline()
  }
  i <- i + 1
  j <- j + 1
}

