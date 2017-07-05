library(e1071)
source('functions.R')

numFeatures <- 64
population <- 'B-cell'
numFiles <- 25
numTrain <- 20
numTest <- numFiles - numTrain

files <- list.files(path = "trainingFiles", full.names = T, recursive = F, pattern = '*.rds')

densXchanA <- matrix(NaN, ncol = numFeatures, nrow = length(files))
densYchanA <- matrix(NaN, ncol = numFeatures, nrow = length(files))
densXchanB <- matrix(NaN, ncol = numFeatures, nrow = length(files))
densYchanB <- matrix(NaN, ncol = numFeatures, nrow = length(files))
threshA <- matrix(NaN, ncol = 2, nrow = length(files))
threshB <- matrix(NaN, ncol = 2, nrow = length(files))

sample_matrix <- matrix(NaN, ncol = 2, nrow = numFeatures * numTrain * 2) # training set
sample_matrix_test <- matrix(NaN, ncol = 2, nrow = numFeatures * numTest * 2) # training set

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
class_vector <- c()
class_vectorB <- c()
print (densXchanA)
while (i <= numTrain){ # (row)
  j <- 1
  while(j <= numFeatures){ # x,y value for each sample (col)
    sample_matrix[k, ] <- c(densXchanA[i,j],densYchanA[i,j])
    sample_matrix[k+(numFeatures*numTrain), ] <- c(densXchanB[i,j],densYchanB[i,j])
    if(densXchanA[i,j] < threshA[i,1]){
      # left
      class_vector <- c(class_vector, "left")
    }
    else if(densXchanA[i,j] > threshA[i,2]){
      # right
      class_vector <- c(class_vector, "right")
    }
    else{ 
      # inside
      class_vector <- c(class_vector, "inside")
    }
    
    if(densXchanB[i,j] < threshB[i,1]){
      # left
      class_vectorB <- c(class_vectorB, "left")
    }
    else if(densXchanB[i,j] > threshB[i,2]){
      # right
      class_vectorB <- c(class_vectorB, "right")
    }
    else{ 
      # inside
      class_vectorB <- c(class_vectorB, "inside")
    }
    j <- j + 1
    k <- k + 1
  }
  i <- i + 1
}
class_vector <- c(class_vector,class_vectorB)
print(sample_matrix)
# --------------------- Test set -------------------------
class_vector_test <- c()
class_vector_testB <- c()
k <- 1
while (i <= numFiles){
  j <- 1
  while(j <= numFeatures){ # x,y value for each sample
    print(k)
    sample_matrix_test[k, ] <- c(densXchanA[i,j],densYchanA[i,j])
    sample_matrix_test[k+(numFeatures*numTest), ] <- c(densXchanB[i,j],densYchanB[i,j])
    if(densXchanA[i,j] < threshA[i,1]){
      # left
      class_vector_test <- c(class_vector_test, "left")
    }
    else if(densXchanA[i,j] > threshA[i,2]){
      # right
      class_vector_test <- c(class_vector_test, "right")
    }
    else{ 
      # inside
      class_vector_test <- c(class_vector_test, "inside")
    }
    if(densXchanB[i,j] < threshB[i,1]){
      # left
      class_vector_testB <- c(class_vector_testB, "left")
    }
    else if(densXchanB[i,j] > threshB[i,2]){
      # right
      class_vector_testB <- c(class_vector_testB, "right")
    }
    else{ 
      # inside
      class_vector_testB <- c(class_vector_testB, "inside")
    }
    j <- j + 1
    k <- k + 1
  }
  i <- i + 1
}

print (sample_matrix_test)

#class_vector <- factor(class_vector)
#class_vector_test <- factor(class_vector_test)
print (class_vector)
print (class_vectorB)
print (class_vector_test)
print (class_vector_testB)

class_vector_test <- c(class_vector_test, class_vector_testB)
class_vector <- factor(class_vector)
class_vector_test <- factor(class_vector_test)

# ================================ CLASSIFICATION ==================================

x <- sample_matrix
y <- class_vector

numTrain <- numFiles - numTest
model <- svm(x, y,  probability = TRUE, cross = 10) 

print(model)
summary(model)

# test with test data
pred <- predict(model, sample_matrix_test)
# test with train data
#pred <- fitted(model)

# Check accuracy:
#table(pred, y)
print(class_vector_test)
print(pred)
table(pred, class_vector_test)


# ======================= Find Thresholds ==================================
cut <- (numFeatures*numTest)
cut_B <- cut + 1
pred_A <- pred[1:cut]
pred_B <- pred[cut_B:length(pred)]
print(pred_A)
print(pred_B)


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
  flag_inside <- "False"
  flag_insideB <- "False"
  left_counter <- 1
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
      flag_inside <- "True"
      left_counter <- 1
    }
    else{
      if(flag_inside == "True"){
        left_counter <- left_counter + 1
        if(left_counter > 5){
          lowestA <- 1000
          flag_inside <- "False"
          left_counter <- 1
        }
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
      flag_insideB <- "True"
      left_counterB <- 1
    }
    else{
      if(flag_insideB == "True"){
        left_counterB <- left_counterB + 1
        if(left_counterB > 5){
          lowestB <- 1000
          flag_insideB <- "False"
          left_counterB <- 1
         }
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
print(pred_threshA)
print(pred_threshB)
print(pred_threshA_index)
print(pred_threshB_index)




# =================================== PLOT ========================================

par(mfrow = c(4,5))
i <- numTrain + 1
j <- 1
k <- 1
#i <- 1 #plot everything
#while(j < 1+numFiles*2){ 
while(j < 1+numTest*2){ # plot test data
  if(j %% 2 != 0){ # A
    print("A")
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
    print("B")
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

print (threshA)
print (threshB)



# ============================== computing error ===============================

i <- 1
score <- 0
for (element in pred){
  if (as.character(element) == as.character(class_vector_test[i])){
    score <- score + 1
    #print("hit")
  }
  else{
    #print("miss")
  }
  i <- i + 1
}
accuracy <- score/length(pred)
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
for (element in pred){
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
    if (i > length(pred)/2 && k <= nrow(pred_threshB_index)){ # chanB predictions
      low <- pred_threshB_index[k,1]
      k <- k + 1
      precisionsB <- c(precisionsB, precision)
      recallsB <- c(recallsB, recall)
      f_measuresB <- c(f_measuresB, f_measure)
    }
    else if (l < nrow(pred_threshA_index)){ # chanA predctions
      l <- l + 1
      low <- pred_threshA_index[l,1]
      precisionsA <- c(precisionsA, precision)
      recallsA <- c(recallsA, recall)
      f_measuresA <- c(f_measuresA, f_measure)
    }
    else{
      precisionsA <- c(precisionsA, precision)
      recallsA <- c(recallsA, recall)
      f_measuresA <- c(f_measuresA, f_measure)
    }
  }
}

print(precisions)
print(recalls)
print(f_measures)

# ------------- compute precision and recall (global) ------------------

# precision = true_pos / (true_pos + false_pos)
# recall = true_pos / (true_pos + false_neg)
# F = 2/((1/precision) + (1/recall))

print (TP)
print (FP)
print (FN)

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

print (precision)
print (recall)


# -------------- compute f-measure (global) -----------------
f_measure <- 2/((1/precision) + (1/recall))
print (f_measure)

# ============================== box plot ======================================


boxplot(precisions)
title(main="precision", sub="", xlab="", ylab="")
boxplot(recalls)
title(main="recall", sub="", xlab="", ylab="")
boxplot(f_measures)
title(main="f_measures", sub="", xlab="", ylab="")


boxplot(precisionsA)
title(main="precisionA", sub="", xlab="", ylab="")
boxplot(recallsA)
title(main="recallA", sub="", xlab="", ylab="")
boxplot(f_measuresA)
title(main="f_measuresA", sub="", xlab="", ylab="")


boxplot(precisionsB)
title(main="precisionB", sub="", xlab="", ylab="")
boxplot(recallsB)
title(main="recallB", sub="", xlab="", ylab="")
boxplot(f_measuresB)
title(main="f_measuresB", sub="", xlab="", ylab="")


