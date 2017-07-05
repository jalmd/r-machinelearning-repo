#K is 5 by default, It should work for other values, however I have not tested it yet.
#X is the data matrix consisting of the predictors.
#Y is the response, it can be continuous or categorical (if it is categorical it must be a factor)


createKFolds_Binary = function(Y,X,k=5,SeedValue=1){
  n = length(Y)
  val = SeedValue
  
  YX.Data = data.frame(Y,X,check.names = FALSE)
  
  stopifnot(is.factor(Y))
  level.of.response = levels(Y)
  num.of.levels = length(level.of.response)
  stopifnot(num.of.levels==2)
  
  Success = level.of.response[1]
  Failure = level.of.response[2]
  
  SuccessYX = YX.Data[Y==Success,]
  FailureYX = YX.Data[Y==Failure,]
  
  num_succ = sum(Y==Success)
  num_fail = sum(Y==Failure)
  
  set.seed(val)
  pool_succ = sample(1:num_succ,num_succ,rep=FALSE)
  set.seed(val)
  pool_fail = sample(1:num_fail,num_fail,rep=FALSE)
  
  num_fail_remainder = num_fail %% k 
  num_succ_remainder = num_succ %% k
  
 
  # The size of each partitions
  partitions_fail = c(rep(floor(num_fail/k),k)) + c(rep(1,num_fail_remainder),rep(0,k-num_fail_remainder))
  stopifnot(sum(partitions_fail)==num_fail)
    
  # Starting and Ending Index for each Failed Partion
  fail_ei = cumsum(partitions_fail)
  fail_si = c(1,(fail_ei+1)[1:length(fail_ei)-1])

  # The size of each partitions
  partitions_succ =  c(rep(floor(num_succ/k),k)) + c(rep(1,num_succ_remainder),rep(0,k-num_succ_remainder))
  stopifnot(sum(partitions_succ)==num_succ)
    
  # Starting and Ending Index for each Successfull Partion
  succ_ei = cumsum(partitions_succ)
  succ_si = c(1,(succ_ei+1)[1:length(succ_ei)-1])

  
  folds = list()
  for(l in seq_len(k)){
    failure_index = pool_fail[fail_si[l]:fail_ei[l]]
    success_index = pool_succ[succ_si[l]:succ_ei[l]]
    
    P1 = SuccessYX[success_index,]
    P2 = FailureYX[failure_index,]
    
    Part = rbind(P1,P2)
    # Randomize Ordering
    set.seed(val+l)
    Part = Part[sample(1:dim(Part)[1],dim(Part)[1],rep=FALSE),]
    folds[[paste0("K",l)]] = Part  
  }
  
  final_folds = list()
  parts = seq_len(k)
  for(l in seq_len(k)){
    trn = folds[parts[-l]]
    trn = Reduce(rbind,trn)
    tst = folds[[l]]
    
    final_folds[[paste0("K",l)]] = list(train=trn,test=tst)
  }
return(final_folds)
}


createKFolds_Continous = function(Y,X,k=5,SeedValue=1){
  n = length(Y)
  val = SeedValue
  
  YX.Data = data.frame(Y,X,check.names = FALSE)
  
  set.seed(val)
  pool = sample(1:n,n,rep=FALSE)
  
  num_remainder = n %% k
  

    # The size of each partitions
  partitions = c(rep(floor(n/k),k)) + c(rep(1,num_remainder),rep(0,k-num_remainder))
  stopifnot(sum(partitions)==n)
    
  # Starting and Ending Index for each Failed Partion
  ei = cumsum(partitions)
  si = c(1,(ei+1)[1:length(ei)-1])
  
  

  folds = list()
  for(l in seq_len(k)){
    pindex = pool[si[l]:ei[l]]
    Part = YX.Data[pindex,]
    
    # Randomize Ordering
    set.seed(val+l)
    Part = Part[sample(1:dim(Part)[1],dim(Part)[1],rep=FALSE),]
    folds[[paste0("K",l)]] = Part  
  }
  
  final_folds = list()
  parts = seq_len(k)
  for(l in seq_len(k)){
    trn = folds[parts[-l]]
    trn = Reduce(rbind,trn)
    tst = folds[[l]]
    
    final_folds[[paste0("K",l)]] = list(train=trn,test=tst)
  }
  return(final_folds)
}


createKFolds = function(Y,X,k=5,SeedValue=1){
  if(is.factor(Y)){
    level.of.response = levels(Y)
    num.of.levels = length(level.of.response)
    stopifnot(num.of.levels==2)
    createKFolds_Binary(Y,X,k,SeedValue)
    
  } else{
    createKFolds_Continous(Y,X,k,SeedValue)
  }
}


#FD = createKFolds(Y,X.genes.Avg)
#names(X.genes.Avg)[names(X.genes.Avg) != names(FD$K1$train[,-1])]




