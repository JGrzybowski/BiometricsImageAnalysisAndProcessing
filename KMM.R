library(dplyr)
source("imageHelpers.R")
source("helpers.R")

REMOVED <- 0
BORDER <- 2
ELBOWS <- 3
STICKY <- 4

elbowsHashes = 255 - (2^c(1,3,5,7))

stickyNeighboursHashes = c()
for(length in 3:4)
  for(start in 0:7)
    stickyNeighboursHashes = c(stickyNeighboursHashes, sum(2^((seq_len(length)+start-1) %% 8)))

## ---- KMM ----
KMM <- function(layer, save.steps = F, verbose = F){
  counter = 1
  newLayer = KMM.Iteration(layer, ifelse(save.steps, counter, 0))
  newLayer %>% paint
  
  while(!identical(newLayer, layer)){
    if(verbose)
      printToConsole(counter)
    layer = newLayer
    counter = counter + 1
    newLayer = KMM.Iteration(layer, ifelse(save.steps, counter, 0))
    newLayer %>% paint
  }
  
  layer
}


## ---- KMM Iteration ----
KMM.Iteration <- function(layer, iteration = 0){
  # Oznaczanie 2
  layerHash = CalculateLayerHash(layer)
  borderIndexes = which((layer == 1) & (layerHash < 255) & (layerHash > 0))
  layer[borderIndexes] <- BORDER
  # Oznaczanie 3
  layer[layerHash %in% elbowsHashes] <- ELBOWS
  # Oznaczanie 4
  layer[(layer > 0) & (layerHash %in% stickyNeighboursHashes)] = STICKY
  # Usuwanie 4
  layer[layer==STICKY] = REMOVED;
  
  # Wybieranie 
  indicesToCheck = which(layer %in% c(BORDER,ELBOWS))
  
  for(deletedVal in c(BORDER, ELBOWS))
  {
    for(index in indicesToCheck)
    {
      if(layer[index] == deletedVal & layerHash[index] %in% deletionArray)
      {
        layer[index] <- REMOVED
        layerHash = CalculateLayerHash(layer)
      }
      else
        layer[index] = 1
    }
  }
  if(iteration > 0)
    layer %>% writePNG(paste("lab3/KMM", formatC(iteration, width = 2, flag = 0), ".png", sep=""))
  
  layer
}

## ---- Deletion Array ----

deletionArray <- c(  3,  5,  7, 12, 13, 14, 15, 20,
                    21, 22, 23, 28, 29, 30, 31, 48,
                    52, 53, 54,55,56,60,61,62,
                    63,65,67,69,71,77,79,80,
                    81,83,84,85,86,87,88,89,
                    91,92,93,94,95,97,99,101,
                    103,109,111,112,113,115,115,116,117,
                    118, 119,120,121,123,124,125,126,
                    127, 131,133,135,141,143,149,151,
                    157,159,181,183,189,191,192,193,
                    195,197,199,205,207,208,209,211,
                    212,213,214,215,216,217,219,220,
                    221,222,223,224,225,227,229,231,
                    237,239,240,241,243,244,245,246,
                    247,248,249,251,252,253,254,255
                   )
