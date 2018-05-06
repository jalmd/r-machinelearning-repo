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

