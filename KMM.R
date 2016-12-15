library(dplyr)
source("imageHelpers.R")
source("helpers.R")

REMOVED <- 0
BORDER <- 2
ELBOWS <- 3
STICKY <- 4

## ---- KMM ----
KMM <- function(layer){
  counter = 1
  newLayer = KMM.Iteration(layer)
  newLayer %>% paint
  while(!identical(newLayer, layer)){
    printToConsole(counter)
    counter = counter + 1
    layer = newLayer
    newLayer = KMM.Iteration(layer)
    newLayer %>% paint
  }
  layer
}

KMM.Iteration <- function(layer){
  maxH = getHeight(layer)
  maxW = getWidth(layer)
  
  # Oznaczanie 2
  layerHash = CalculateLayerHash(layer)
  borderIndexes = which((layer == 1) & (layerHash < 255) & (layerHash > 0))
  layer[borderIndexes] = BORDER
  
  # Oznaczanie 3
  elbowsHashes = 255 - (2^c(1,3,5,7))
  layer[layerHash %in% elbowsHashes] <- ELBOWS
  
  # Oznaczanie 4
  stickyNeighboursHashes = c()
  for(length in 2:4)
    for(start in 0:7)
      stickyNeighboursHashes = c(stickyNeighboursHashes, sum(2^((seq_len(length)+start-1) %% 8)))
  layer[(layer > 0) & (layerHash %in% stickyNeighboursHashes)] = STICKY
  
  # Usuwanie 4
  layer[layer==STICKY] = REMOVED;
  
  # Wybieranie 
  indicesToCheck = which(layer %in% c(BORDER,ELBOWS))
  
  for(deletedVal in c(BORDER, ELBOWS))
  {
    for(index in indicesToCheck)
    {
      if(layer[index] == deletedVal)
      {
        layer[index] = 0
        layerHash = CalculateLayerHash(layer)
      }
      else
        layer[index] = 1
    }
  }
  layer
}
