source("ImageHelpers.R")
source("Normalization.R")
source("Thinning.R")

mark_minutiae <- function(layer){
  layerHash = CalculateLayerHash(layer)
  
  endpoints = which(layerHash %in% endpointsLUT)
  triplets = which(layerHash %in% tripletsLUT)
  
  img = array(data = layer, dim = c(getHeight(layer), getWidth(layer), 3))
  
  img[,,1][endpoints] = 1
  img[,,2][endpoints] = 0
  img[,,3][endpoints] = 0
  
  img[,,1][triplets] = 0
  img[,,2][triplets] = 1
  img[,,3][triplets] = 0
  
  img
}

endpointsLUT = 2^(1:8)
tripletsLUT = colSums(combn(c(1:8),3) ^ 2)

