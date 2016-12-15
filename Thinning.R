library(dplyr)
source("imageHelpers.R")
source("helpers.R")

## ---- HashCalculation ----

CalculateLayerHash <- function(layer){
  maxH = getHeight(layer)
  maxW = getWidth(layer)
  
  weights = c(128,64,32, 1,0,16, 2,4,8)
  
  indexes = as.vector(matrix(1:(maxH*maxW),ncol = maxW, nrow = maxH)[1:(maxH), 1:(maxW)])
  
  dataIndexes = cbind(indexes-maxH-1, indexes-maxH, indexes-maxH+1,
                      indexes-1, indexes+0, indexes+1, 
                      indexes+maxH-1, indexes+maxH, indexes+maxH+1)
  
  dataIndexes[which(dataIndexes[,5] %% maxH == 1),c(1,4,7)] = NA
  dataIndexes[which(dataIndexes[,5] %% maxH == 0),c(3,6,9)] = NA
  
  dataIndexes[dataIndexes < 1] = NA
  dataIndexes[dataIndexes > maxH*maxW] = NA
  
  windows = matrix(layer[dataIndexes], nrow=length(indexes))
  windows[is.na(windows)] = 0
  windows = sign(windows)
  
  pixelHashes = windows %*% weights
  matrix(pixelHashes, ncol = maxW, nrow = maxH)
}

## ---- ----

printToConsole <- function(iteration){
  Sys.sleep(0.1)
  print(iteration)
  flush.console() 
}

loadImage.forThinning <- function(filename){
  readPNG(filename) %>% grayscaled %>% binarized(function(x){x < 0.45})
}


readFingerprint <- function(i){
  filename = paste("Fingers/", "Finger", formatC(i, width = 4, flag = 0), ".png", sep="")  
  readPNG(filename) %>% grayscaled %>% binarized(function(x){x < 0.45}) %>% K3M 
}

test.finger <- function(iterations){
  img = finger
  for(i in 1:iterations)
    img = K3M(finger)
  img
}


