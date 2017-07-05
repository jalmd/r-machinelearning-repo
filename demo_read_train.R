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
    threshA[i, ] <- c(s[[population]]@thresholdALow, s[[population]]@thresholdAHigh)
    threshB[i, ] <- c(s[[population]]@thresholdBLow, s[[population]]@thresholdBHigh)

    i <- i + 1
    if (i > 300){
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

print (densYchanA)



