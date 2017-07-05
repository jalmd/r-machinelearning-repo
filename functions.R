TrainingGate <- setClass("TrainingGate",
                         slots = c(
                                   parentName = "character",
                                   channelA = "numeric",
                                   channelB = "numeric",
                                   thresholdALow = "numeric",
                                   thresholdAHigh = "numeric",
                                   thresholdBLow = "numeric",
                                   thresholdBHigh = "numeric",
                                   negate = "logical",
                                   gateAssignments = "logical",
                                   densitiesA = "list",
                                   densitiesB = "list"
                                   )
                         )

estimateDensity <- function(data, n)
{
  dens <- density(data[which(!is.na(data))], n = n)
  dens <- smooth.spline(dens$x, dens$y, spar=0.4)
  dens$y[which(dens$y<0)] <- 0
  return(dens)
}

plotGate <- function(trainGate, numFeatures)
{
    par(mfrow = c(2, 1))

    neg <- trainGate@negate

    minX <- min(trainGate@densitiesA[[as.character(numFeatures)]]$x)
    maxX <- max(trainGate@densitiesA[[as.character(numFeatures)]]$x)
    minY <- min(trainGate@densitiesA[[as.character(numFeatures)]]$y)
    maxY <- max(trainGate@densitiesA[[as.character(numFeatures)]]$y)
    low <- if (!is.nan(trainGate@thresholdALow)) trainGate@thresholdALow else (if (!neg) minX else maxX)
    high <- if (!is.nan(trainGate@thresholdAHigh)) trainGate@thresholdAHigh else (if (!neg) maxX else minX)
    plot(
         trainGate@densitiesA[[as.character(numFeatures)]]$x,
         trainGate@densitiesA[[as.character(numFeatures)]]$y,
         xlab = 'Channel A',
         ylab = 'Density',
         type = 'o'
        );
    abline(v = low, col = 'red', lwd = 2, lty = 2)
    abline(v = high, col = 'red', lwd = 2, lty = 2)

    minX <- min(trainGate@densitiesB[[as.character(numFeatures)]]$x)
    maxX <- max(trainGate@densitiesB[[as.character(numFeatures)]]$x)
    minY <- min(trainGate@densitiesB[[as.character(numFeatures)]]$y)
    maxY <- max(trainGate@densitiesB[[as.character(numFeatures)]]$y)
    low <- if (!is.nan(trainGate@thresholdBLow)) trainGate@thresholdBLow else (if (!neg) minX else maxX)
    high <- if (!is.nan(trainGate@thresholdBHigh)) trainGate@thresholdBHigh else (if (!neg) maxX else minX)
    plot(
         trainGate@densitiesB[[as.character(numFeatures)]]$x,
         trainGate@densitiesB[[as.character(numFeatures)]]$y,
         xlab = 'Channel B',
         ylab = 'Density',
         type = 'o'
        );
    abline(v = low, col = 'red', lwd = 2, lty = 2)
    abline(v = high, col = 'red', lwd = 2, lty = 2)

}


rotate.data <- function(data, chans=NULL, theta=NULL)
{
    if (class(data)== "flowFrame" & !is.null(chans))
    {
        data.new <- exprs(data)[,chans]
        if (is.null(theta))
        {
            reg.slope <- atan(lm(data.new[,1] ~ data.new[,2])$coefficients[2])
            theta <- pi/2 - reg.slope
        }
        data.new <- data.new %*% matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),2,2,byrow=T)
        exprs(data)[,chans] <- data.new
    }else{
        data <- data %*% matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),2,2,byrow=T)
    }
    return(list(data=data,theta=theta))
}



















getHighThresholdA <- function(trainGate, numFeatures){
  neg <- trainGate@negate
  
  minX <- min(trainGate@densitiesA[[as.character(numFeatures)]]$x)
  maxX <- max(trainGate@densitiesA[[as.character(numFeatures)]]$x)
  
  high <- if (!is.nan(trainGate@thresholdAHigh)) trainGate@thresholdAHigh else (if (!neg) maxX else minX)
  
  return (high)
}

getLowThresholdA <- function(trainGate, numFeatures){
  neg <- trainGate@negate
  
  minX <- min(trainGate@densitiesA[[as.character(numFeatures)]]$x)
  maxX <- max(trainGate@densitiesA[[as.character(numFeatures)]]$x)
  
  low <- if (!is.nan(trainGate@thresholdALow)) trainGate@thresholdALow else (if (!neg) minX else maxX)
  
  return (low)
}

getHighThresholdB <- function(trainGate, numFeatures){
  neg <- trainGate@negate
  
  minX <- min(trainGate@densitiesB[[as.character(numFeatures)]]$x)
  maxX <- max(trainGate@densitiesB[[as.character(numFeatures)]]$x)
  
  high <- if (!is.nan(trainGate@thresholdBHigh)) trainGate@thresholdBHigh else (if (!neg) maxX else minX)
  
  return (high)
}

getLowThresholdB <- function(trainGate, numFeatures){
  neg <- trainGate@negate
  
  minX <- min(trainGate@densitiesB[[as.character(numFeatures)]]$x)
  maxX <- max(trainGate@densitiesB[[as.character(numFeatures)]]$x)
  
  low <- if (!is.nan(trainGate@thresholdBLow)) trainGate@thresholdBLow else (if (!neg) minX else maxX)
  
  return (low)
}

getOutsideThresholdVector <- function(x, highThreshold, lowThreshold){
  outside_vector <- c()
  inside_vector <- c()
  
  print ("===================================================")
  print ("getOutsideThresholdPlots function\n")
  print ("===================================================")
  print (x)
  
  print("High Threshold")
  print (highThreshold)
  print("Low Threshold")
  print (lowThreshold)
  print("***********************END*************************")
  
  
  for (value in x){
    if (value > highThreshold && value < lowThreshold){
      inside_vector <- c(inside_vector, value)
    }
    else{
      outside_vector <- c(outside_vector, value)
    }
  }
  
  print (inside_vector)
  print (outside_vector)
  
  return (outside_vector)
}

# y vector
getInsideThresholdVector <- function(x,y, high, low){
  inside_vector <- c()
  outside_vector <- c()
  
  i <- 1
  for (value in x){
    if (x[i] > high && x[i] < low){
      inside_vector <- c(inside_vector, value)
    }
    else{
      outside_vector <- c(outside_vector, value)
    }
    i <- i + 1
  }
  
  return (inside_vector)
}

getClassVector <- function(x,high,low){
  class_vector <- c()
  
  i <- 1
  for (value in x){
    if (x[i] > low && x[i] < high){
      class_vector <- c(class_vector, "inside")
    }
    else if (x[i] < low && x[i] < high){
      class_vector <- c(class_vector, "left")
    }
    else{
      class_vector <- c(class_vector, "right")
    }
    i <- i + 1
  }
  
  return (class_vector)
}

# identify the peaks first using running average of 3 consecutive points to
# smooth the data ever so slightly. Also employ the above mentioned control against flat then drop-off.
# filter these candidates by comparing, for a loess-smoothed version, 
# the average inside a window centered at each peak with the average of local terms outside.

findPeaks <-function (x, thresh=0.05, span=0.25, lspan=0.05, noisey=TRUE){
  n <- length(x)
  y <- x
  mu.y.loc <- y
  if(noisey)
  {
    mu.y.loc <- (x[1:(n-2)] + x[2:(n-1)] + x[3:n])/3
    mu.y.loc <- c(mu.y.loc[1], mu.y.loc, mu.y.loc[n-2])
  }
  y.loess <- loess(x~I(1:n), span=span)
  y <- y.loess[[2]]
  sig.y <- var(y.loess$resid, na.rm=TRUE)^0.5
  DX.1 <- sign(diff(mu.y.loc, na.pad = FALSE))
  pks <- which(diff(DX.1, na.pad = FALSE) < 0 & DX.1[-(n-1)] > 0) + 1
  out <- pks
  if(noisey)
  {
    n.w <- floor(lspan*n/2)
    out <- NULL
    for(pk in pks)
    {
      inner <- (pk-n.w):(pk+n.w)
      outer <- c((pk-2*n.w):(pk-n.w),(pk+2*n.w):(pk+n.w))
      mu.y.outer <- mean(y[outer])
      if(!is.na(mu.y.outer)) 
        if (mean(y[inner])-mu.y.outer > thresh*sig.y) out <- c(out, pk)
    }
  }
  out
}






