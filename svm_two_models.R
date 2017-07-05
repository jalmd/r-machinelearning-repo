args = commandArgs(trailingOnly=TRUE)
print (args)
# 1 = cell population
# 2 = num features
# 3 = num train
# 4 = num test
# 5 = outpout_plot_name

library(methods)
library(e1071)
source('functions.R')

population <- args[1]
numFeatures <- as.numeric(args[2])
numTrain <- as.numeric(args[3])
numTest <- as.numeric(args[4])
numFiles <- numTrain + numTest

output_plot <- args[5]

files <- list.files(path = "trainingFiles", full.names = T, recursive = F, pattern = '*.rds')

densXchanA <- matrix(NaN, ncol = numFeatures, nrow = length(files))
densYchanA <- matrix(NaN, ncol = numFeatures, nrow = length(files))
densXchanB <- matrix(NaN, ncol = numFeatures, nrow = length(files))
densYchanB <- matrix(NaN, ncol = numFeatures, nrow = length(files))
threshA <- matrix(NaN, ncol = 2, nrow = length(files))
threshB <- matrix(NaN, ncol = 2, nrow = length(files))

sample_matrix <- matrix(NaN, ncol = 2, nrow = numFeatures * numTrain * 2) # training set
sample_matrixA <- matrix(NaN, ncol= 2, nrow = numFeatures * numTrain)
sample_matrixB <- matrix(NaN, ncol= 2, nrow = numFeatures * numTrain)
sample_matrix_test <- matrix(NaN, ncol = 2, nrow = numFeatures * numTest * 2) # training set
sample_matrix_testA <- matrix(NaN, ncol = 2, nrow = numFeatures * numTest) # training set
sample_matrix_testB <- matrix(NaN, ncol = 2, nrow = numFeatures * numTest) # training set


samples <- list()

# ========================== GET DATA FROM FILES =================================

i <- 1
for (f in files)
{
  print(i)
  
  s <- readRDS(f)
  samples[[f]] <- s
  densXchanA[i, ] <- s[[population]]@densitiesA[[as.character(numFeatures)]]$x
  densYchanA[i, ] <- s[[population]]@densitiesA[[as.character(numFeatures)]]$y
  densXchanB[i, ] <- s[[population]]@densitiesB[[as.character(numFeatures)]]$x
  densYchanB[i, ] <- s[[population]]@densitiesB[[as.character(numFeatures)]]$y
  
  lowA <- getLowThresholdA(s[[population]], numFeatures)
  highA <- getHighThresholdA(s[[population]], numFeatures)
  
  lowB <- getLowThresholdB(s[[population]], numFeatures)
  highB <- getHighThresholdB(s[[population]], numFeatures)
  
  threshA[i, ] <- c(lowA, highA)
  threshB[i, ] <- c(lowB, highB)
  
  i <- i + 1
  if (i > numFiles){
    break
  }
}

perm <- sample(length(samples))

samples <- samples[perm]
densXchanA <- densXchanA[perm, ]
densYchanA <- densYchanA[perm, ]
densXchanB <- densXchanB[perm, ]
densYchanB <- densYchanB[perm, ]
threshA <- threshA[perm, ]
threshB <- threshB[perm, ]



# ========================== Classify ===============================
# --------------------- Training set ------------------------
i <- 1
j <- 1
k <- 1
class_vectorA <- c()
class_vectorB <- c()

while (i <= numTrain){ # (row)
  j <- 1
  flag_leftB <- FALSE
  flag_rightB <- FALSE
  while(j <= numFeatures){ # x,y value for each sample
    sample_matrix[k, ] <- c(densXchanA[i,j],densYchanA[i,j])
    sample_matrix[k+(numFeatures*numTrain), ] <- c(densXchanB[i,j],densYchanB[i,j])
    sample_matrixA[k, ] <- c(densXchanA[i,j],densYchanA[i,j])
    sample_matrixB[k, ] <- c(densXchanB[i,j],densYchanB[i,j])
    
    #dealWith sampleA
    if(densXchanA[i,j] < threshA[i,1]){
      # left
      class_vectorA <- c(class_vectorA, "left")
    }
    else if(densXchanA[i,j] > threshA[i,2]){
      # right
      class_vectorA <- c(class_vectorA, "right")
    }
    else{ 
      # inside
      class_vectorA <- c(class_vectorA, "inside")
    }
    
    #dealWith sampleB
    if(densXchanB[i,j] < threshB[i,1]){
      # left
      flag_leftB <- TRUE
      class_vectorB <- c(class_vectorB, "left")
    }
    else if(densXchanB[i,j] > threshB[i,2]){
      # right
      flag_rightB <- TRUE
      class_vectorB <- c(class_vectorB, "right")
    }
    else{ 
      # inside
      if (flag_leftB == FALSE){
        flag_leftB <- TRUE
        class_vectorB <- c(class_vectorB, "left") 
      }
      else{
        class_vectorB <- c(class_vectorB, "inside") 
      }
    }
    j <- j + 1
    k <- k + 1
  }
  if (flag_rightB == FALSE){
    class_vectorB <- class_vectorB[1:length(class_vectorB)-1]
    class_vectorB <- c(class_vectorB, "right")
  }
  i <- i + 1
}


# --------------------- Test set -------------------------
class_vector_testA <- c()
class_vector_testB <- c()
k <- 1
while (i <= numFiles){
  j <- 1
  flag_leftB <- FALSE
  flag_rightB <- FALSE
  while(j <= numFeatures){ # x,y value for each sample
    print(k)
    sample_matrix_test[k, ] <- c(densXchanA[i,j],densYchanA[i,j])
    sample_matrix_test[k+(numFeatures*numTest), ] <- c(densXchanB[i,j],densYchanB[i,j])
    sample_matrix_testA[k, ] <- c(densXchanA[i,j],densYchanA[i,j])
    sample_matrix_testB[k, ] <- c(densXchanB[i,j],densYchanB[i,j])
    #dealWith chanA
    if(densXchanA[i,j] < threshA[i,1]){
      # left
      class_vector_testA <- c(class_vector_testA, "left")
    }
    else if(densXchanA[i,j] > threshA[i,2]){
      # right
      class_vector_testA <- c(class_vector_testA, "right")
    }
    else{ 
      # inside
      class_vector_testA <- c(class_vector_testA, "inside")
    }
    
    #dealWith chanB
    if(densXchanB[i,j] < threshB[i,1]){
      # left
      flag_leftB <- TRUE
      class_vector_testB <- c(class_vector_testB, "left")
    }
    else if(densXchanB[i,j] > threshB[i,2]){
      # right
      flag_rightB <- TRUE
      class_vector_testB <- c(class_vector_testB, "right")
    }
    else{ 
      # inside
      if (flag_leftB == FALSE){
        flag_leftB <- TRUE
        class_vector_testB <- c(class_vector_testB, "left") 
      }
      else{
        class_vector_testB <- c(class_vector_testB, "inside") 
      }
    }
    j <- j + 1
    k <- k + 1
  }
  if (flag_rightB == FALSE){
    class_vector_testB <- class_vector_testB[1:length(class_vector_testB)-1]
    class_vector_testB <- c(class_vector_testB, "right")
  }
  i <- i + 1
}

class_vector <- c(class_vectorA,class_vectorB)
class_vector <- factor(class_vector)
class_vector_test <- c(class_vector_testA, class_vector_testB)
class_vector_test <- factor(class_vector_test)


class_vectorA <- factor(class_vectorA)
class_vectorB <- factor(class_vectorB)
class_vector_testA <- factor(class_vector_testA)
class_vector_testB <- factor(class_vector_testB)


# ================================ CLASSIFICATION ==================================

x <- sample_matrixA
y <- class_vectorA

xB <- sample_matrixB
yB <- class_vectorB

numTrain <- numFiles - numTest
model <- svm(x, y,  probability = TRUE, cross = 10) 
modelB <- svm(xB, yB, probability = TRUE, cross = 10)
print(model)
summary(model)
print(modelB)
summary(modelB)

# test with test data
predA <- predict(model, sample_matrix_testA)
predB <- predict(modelB, sample_matrix_testB)
# test with train data
#pred <- fitted(model)

# Check accuracy:
#table(pred, y)
table(predA, class_vector_testA)
table(predB, class_vector_testB)

# ======================= Find Thresholds ==================================
pred_A <- predA
pred_B <- predB

pred_threshA <- matrix(NaN, nrow = length(pred_A)/numFeatures, ncol = 2)
pred_threshB <- matrix(NaN, nrow = length(pred_B)/numFeatures, ncol = 2)
pred_threshA_index <- matrix(NaN, nrow = length(pred_A)/numFeatures, ncol = 2)
pred_threshB_index <- matrix(NaN, nrow = length(pred_B)/numFeatures, ncol = 2)

highestA <- -1000
lowestA <- 1000
highestB <- -1000
lowestB <- 1000
i <- 1
k <- 1
while(k <= numTest){
  j <- 1
  while (j <= numFeatures){
    if (as.character(pred_A[i]) == "inside"){ # dealing with A
      if (sample_matrix_test[i,1] < lowestA){
        lowestA <- sample_matrix_test[i,1]
        lowestA_index <- i
      } 
      else if (sample_matrix_test[i,1] > highestA){
        highestA <- sample_matrix_test[i,1]
        highestA_index <- i
      }
    }
    
    if (as.character(pred_B[i]) == "inside"){ # dealing with B
      if (sample_matrix_test[i+(numFeatures*numTest),1] < lowestB){
        lowestB <- sample_matrix_test[i+(numFeatures*numTest),1]
        lowestB_index <- i+(numFeatures*numTest)
      }
      else if (sample_matrix_test[i+(numFeatures*numTest),1] > highestB){
        highestB <- sample_matrix_test[i+(numFeatures*numTest),1]
        highestB_index <- i+(numFeatures*numTest)
      }
    }
    j <- j + 1
    i <- i + 1
  }
  pred_threshA[k, ] <- c(lowestA, highestA)
  pred_threshB[k, ] <- c(lowestB, highestB)
  pred_threshA_index[k, ] <- c(lowestA_index, highestA_index)
  pred_threshB_index[k, ] <- c(lowestB_index, highestB_index)
  highestA <- -1000
  lowestA <- 1000
  highestB <- -1000
  lowestB <- 1000
  k <- k + 1
}


# =================================== PLOT ========================================
pdf(output_plot)
par(mfrow = c(4,5))
i <- numTrain + 1
j <- 1
k <- 1
#i <- 1 #plot everything
#while(j < 1+numFiles*2){ 
while(j < 1+numTest*2){ # plot test data
  if(j %% 2 != 0){ # A
    plot(
      samples[[i]][[population]]@densitiesA[[as.character(numFeatures)]]$x,
      samples[[i]][[population]]@densitiesA[[as.character(numFeatures)]]$y,
      type = 'o'
    );
    abline(v = threshA[i, 1], col = 'red', lwd = 2, lty = 2)
    abline(v = threshA[i, 2], col = 'red', lwd = 2, lty = 2)
    abline(v = pred_threshA[k, 1], col = 'blue', lwd = 2, lty = 2)
    abline(v = pred_threshA[k, 2], col = 'blue', lwd = 2, lty = 2)
  }
  else{ # B
    plot(
      samples[[i]][[population]]@densitiesB[[as.character(numFeatures)]]$x,
      samples[[i]][[population]]@densitiesB[[as.character(numFeatures)]]$y,
      type = 'o'
    );
    abline(v = threshB[i, 1], col = 'red', lwd = 2, lty = 2)
    abline(v = threshB[i, 2], col = 'red', lwd = 2, lty = 2)
    abline(v = pred_threshB[k, 1], col = 'blue', lwd = 2, lty = 2)
    abline(v = pred_threshB[k, 2], col = 'blue', lwd = 2, lty = 2)
    i <- i + 1
    k <- k + 1
  }
  j <- j + 1
}


# ============================== computing error ===============================

i <- 1
score <- 0

for (element in predA){
  if (as.character(element) == as.character(class_vector_testA[i])){
    score <- score + 1
    #print("hit")
  }
  else{
    #print("miss")
  }
  i <- i + 1
}
i <- 1
for (element in predB){
  if (as.character(element) == as.character(class_vector_testB[i])){
    score <- score + 1
    #print("hit")
  }
  else{
    #print("miss")
  }
  i <- i + 1
}
accuracy <- score/(length(pred_A)*2)
error <- 1 - accuracy
print (error)


# =========================== computing f-measure ==============================
# specificity = TN/(TN+FP)
# accuracy = (TP + TN)/(TP + FN + FP + TN)

# precision = true_pos / (true_pos + false_pos)
# recall = true_pos / (true_pos + false_neg)
# F = 2/((1/precision) + (1/recall))

# ----------- compute TP, FP, TN, FN and each precision/recall/fmeasure---------------

f_measures <- c()
precisions <- c()
recalls <- c()

precisionsA <- c()
precisionsB <- c()
recallsA <- c()
recallsB <- c()
f_measuresA <- c()
f_measuresB <- c()

i <- 1
j <- 1
k <- 1 # counter for threshold vectorB
l <- 1 # counter for threshold vectorA
low <- pred_threshA_index[1,1]
TP <- 0
FP <- 0
TN <- 0
FN <- 0
for (element in predA){
  string_predicted_value <- as.character(element)
  string_true_value <- as.character(class_vector_test[i]) 
  if (string_predicted_value == string_true_value){
    if (string_predicted_value == "inside" && i >= low){
      TP <- TP + 1
    }
    else{ # left left or right right
      TN <- TN + 1
    }
  }
  else{
    if (string_predicted_value == "left" || string_predicted_value == "right"){
      if(string_true_value == "inside"){
        FN <- FN + 1
      }
      else{ # left right or right left
        TN <- TN + 1
      }
    }
    else { # string_predicted_value == inside and string_true_value == left or right -> false positive
      FP <- FP + 1
    }
  }
  i <- i + 1
  j <- j + 1
  if (j > numFeatures){
    j <- 1
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    f_measure <- 2/((1/precision) + (1/recall))
    precisions <- c(precisions, precision)
    recalls <- c(recalls, recall)
    f_measures <- c(f_measures, f_measure)
    l <- l + 1
    if(l < nrow(pred_threshA_index)){
      low <- pred_threshA_index[l,1]
    }
    precisionsA <- c(precisionsA, precision)
    recallsA <- c(recallsA, recall)
    f_measuresA <- c(f_measuresA, f_measure)
  }
}
i <- 1
j <- 1
k <- 1 # counter for threshold vectorB
l <- 1 # counter for threshold vectorA
for (element in predA){
  string_predicted_value <- as.character(element)
  string_true_value <- as.character(class_vector_test[i]) 
  if (string_predicted_value == string_true_value){
    if (string_predicted_value == "inside" && i >= low){
      TP <- TP + 1
    }
    else{ # left left or right right
      TN <- TN + 1
    }
  }
  else{
    if (string_predicted_value == "left" || string_predicted_value == "right"){
      if(string_true_value == "inside"){
        FN <- FN + 1
      }
      else{ # left right or right left
        TN <- TN + 1
      }
    }
    else { # string_predicted_value == inside and string_true_value == left or right -> false positive
      FP <- FP + 1
    }
  }
  i <- i + 1
  j <- j + 1
  if (j > numFeatures){
    j <- 1
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    f_measure <- 2/((1/precision) + (1/recall))
    precisions <- c(precisions, precision)
    recalls <- c(recalls, recall)
    f_measures <- c(f_measures, f_measure)
    k <- k + 1
    if(k < nrow(pred_threshA_index)){
      low <- pred_threshB_index[k,1]
    }
    precisionsB <- c(precisionsB, precision)
    recallsB <- c(recallsB, recall)
    f_measuresB <- c(f_measuresB, f_measure)
  }
}

print(precisions)
print(recalls)
print(f_measures)

# ------------- compute precision and recall (global) ------------------

# precision = true_pos / (true_pos + false_pos)
# recall = true_pos / (true_pos + false_neg)
# F = 2/((1/precision) + (1/recall))

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)


# -------------- compute f-measure (global) -----------------
f_measure <- 2/((1/precision) + (1/recall))


# ============================== box plot ======================================


boxplot(precisions)
title(main="precision", sub="", xlab="", ylab="")
boxplot(recalls)
title(main="recall", sub="", xlab="", ylab="")
boxplot(f_measures)
title(main="f_measures", sub="", xlab="", ylab="")


#boxplot(precisionsA)
#title(main="precisionA", sub="", xlab="", ylab="")
#boxplot(recallsA)
#title(main="recallA", sub="", xlab="", ylab="")
#boxplot(f_measuresA)
#title(main="f_measuresA", sub="", xlab="", ylab="")


#boxplot(precisionsB)
#title(main="precisionB", sub="", xlab="", ylab="")
#boxplot(recallsB)
#title(main="recallB", sub="", xlab="", ylab="")
#boxplot(f_measuresB)
#title(main="f_measuresB", sub="", xlab="", ylab="")
dev.off()
