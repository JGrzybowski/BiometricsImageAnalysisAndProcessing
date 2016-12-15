source("Thinning.R")

## ---- K3M ----
K3M <- function(layer){
  newLayer = K3M.Iteration(layer)
  while(!identical(newLayer, layer)){
    layer = newLayer
    newLayer = K3M.Iteration(layer)
  }
  layer
  # IterativePhase(layer, which(layer>0), LUT = A1pix)
}

K3M.Iteration <- function(layer){
  borderLUT = c(3, 6, 7, 12, 14, 15, 24, 28, 30, 31, 48, 56, 60,
                62, 63, 96, 112, 120, 124, 126, 127, 129, 131, 135,
                143, 159, 191, 192, 193, 195, 199, 207, 223, 224,
                225, 227, 231, 239, 240, 241, 243, 247, 248, 249,
                251, 252, 253, 254)
  maxH = getHeight(layer)
  maxW = getWidth(layer)
  
  layerHash = layer %>% CalculateLayerHash %>% matrix(ncol = maxW, nrow = maxH)
  borderIndexes = which(layerHash %in% borderLUT & layer == 1)
  layer %>% A1(borderIndexes) %>% A2(borderIndexes) %>% A3(borderIndexes) %>% A4(borderIndexes) %>% A5(borderIndexes)
}

A0 <- function(layer){
  LUT = c(3, 6, 7, 12, 14, 15, 24, 28, 30, 31, 48, 56, 60,
          62, 63, 96, 112, 120, 124, 126, 127, 129, 131, 135,
          143, 159, 191, 192, 193, 195, 199, 207, 223, 224,
          225, 227, 231, 239, 240, 241, 243, 247, 248, 249,
          251, 252, 253, 254)
  IterativePhase(layer, LUT)
}

A1 <- function(layer,indexes){
  LUT = c(7, 14, 28, 56, 112, 131, 193, 224)
  # printToConsole(1)
  IterativePhase(layer, indexes, LUT)
}

A2 <- function(layer, indexes){
  LUT = c(7, 14, 15, 28, 30, 56, 60, 112, 120, 131, 135,
          193, 195, 224, 225, 240)
  # printToConsole(2)
  IterativePhase(layer, indexes, LUT)
}

A3 <- function(layer, indexes){
  LUT = c(7, 14, 15, 28, 30, 31, 56, 60, 62, 112, 120,
          124, 131, 135, 143, 193, 195, 199, 224, 225, 227,
          240, 241, 248)
  # printToConsole(3)
  IterativePhase(layer, indexes, LUT)  
}

A4 <- function(layer, indexes){
  LUT = c(7, 14, 15, 28, 30, 31, 56, 60, 62, 63, 112, 120,
          124, 126, 131, 135, 143, 159, 193, 195, 199, 207,
          224, 225, 227, 231, 240, 241, 243, 248, 249, 252)
  # printToConsole(4)
  IterativePhase(layer, indexes, LUT)
}

A5 <- function(layer, indexes){
  LUT = c(7, 14, 15, 28, 30, 31, 56, 60, 62, 63, 112, 120,
          124, 126, 131, 135, 143, 159, 191, 193, 195, 199,
          207, 224, 225, 227, 231, 239, 240, 241, 243, 248,
          249, 251, 252, 254)
  # printToConsole(5)
  IterativePhase(layer, indexes, LUT)  
}

A1pix <- 
  # function(layer, indexes){
  # LUT = 
  c(3, 6, 7, 12, 14, 15, 24, 28, 30, 31, 48, 56,
          60, 62, 63, 96, 112, 120, 124, 126, 127, 129, 131,
          135, 143, 159, 191, 192, 193, 195, 199, 207, 223,
          224, 225, 227, 231, 239, 240, 241, 243, 247, 248,
          249, 251, 252, 253, 254)
#   IterativePhase(layer, indexes, LUT)
# }


IterativePhase <- function(layer, borderIndexes, LUT){
  pixelHashes = CalculateLayerHash(layer)
  
  for(index in borderIndexes){
    # printToConsole(index)
    removed = pixelHashes[index] %in% LUT
    if(removed){
      layer[index] = 0
      pixelHashes = CalculateLayerHash(layer)
    }
  }
  layer %>% paint
  layer
}



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