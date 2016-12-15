source("Thinning.R")

## ---- K3M ----
K3M <- function(layer, save.steps = F){
  counter = 1
  newLayer = K3M.Iteration(layer, ifelse(save.steps,counter,0))
  
  while(!identical(newLayer, layer)){
    layer = newLayer
    counter = counter+1
    newLayer = K3M.Iteration(layer, ifelse(save.steps,counter,0))
  }
  
  layer
  IterativePhase(layer, which(layer>0), LUT = A1pix)
}
## ---- K3M Iteration ----
K3M.Iteration <- function(layer, iteration = 0){
  borderLUT = c(3, 6, 7, 12, 14, 15, 24, 28, 30, 31, 48, 56, 60,
                62, 63, 96, 112, 120, 124, 126, 127, 129, 131, 135,
                143, 159, 191, 192, 193, 195, 199, 207, 223, 224,
                225, 227, 231, 239, 240, 241, 243, 247, 248, 249,
                251, 252, 253, 254)
  maxH = getHeight(layer)
  maxW = getWidth(layer)
  
  layerHash = layer %>% CalculateLayerHash %>% matrix(ncol = maxW, nrow = maxH)
  borderIndexes = which(layerHash %in% borderLUT & layer == 1)
  layer = layer %>% A1(borderIndexes) %>% A2(borderIndexes) %>% A3(borderIndexes) %>% A4(borderIndexes) %>% A5(borderIndexes)
  
  if(iteration > 0)
    layer %>% writePNG(paste("lab3/K3M", formatC(iteration, width = 2, flag = 0), ".png", sep=""))
  layer
}

## ---- Phases helpers ----

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

## ---- IterativePhase ----

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