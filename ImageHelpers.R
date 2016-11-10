library(jpeg)
library(dplyr)

clip <- function(x, min, max){
  ifelse(x <= min,  min, ifelse(x >= max, max, x))
}

## ---- Image Loading ----
loadImage <- function(filename) { readJPEG(filename) }
getHeight <- function(image) { dim(image)[1] }
getWidth <- function(image) { dim(image)[2] }
getChannels <- function(image) { 
  if(is.na(dim(image)[3])) 
    1
  else 
    dim(image)[3]  
}
saveImage <- function(image, filename) { writeJPEG(image, filename, quality = 1) }

## ---- Color Convertion ----
grayscaled <- function(image) {
  if (getChannels(image) == 1)
    return(image)
  else 
    return(apply(image,c(1,2),sum)/getChannels(image))
}

inverted <- function(image) {
  1 - image
}

brightned <- function(image, delta){
  clip(image+delta, 0, 1)
}

contrasted <- function(image, K = 0.85, contrastColor = 0.5){
  if(length(contrastColor)==1 && getChannels(image) == 3)
    contrastColor = rep(contrastColor,3)
  if(length(contrastColor) != getChannels(image))
    error("Wrong length of contrast Color or number of image channels")
  
  imgt = image
  imgt[,,1] = K * (imgt[,,1] - contrastColor[1]) + contrastColor[1]
  if(getChannels(imgt) > 1)
  {
    imgt[,,2] = K * (imgt[,,2] - contrastColor[2]) + contrastColor[2]
    imgt[,,3] = K * (imgt[,,3] - contrastColor[3]) + contrastColor[3]
  }
  
  clip(imgt , 0, 1)
}

## ---- Binariztion ----
binarized <- function(image, predicate){
  ifelse(predicate(image), 0, 1)
}

verticalProjection <- function(image){
  if(getChannels(image) == 3)
    image %>% apply(c(2,3), sort)
  else
    image %>% apply(2, sort)
}

horizontalProjection <- function(image){
  if(getChannels(image) == 3) 
    image %>% apply(c(1,3), sort) %>% aperm(c(2,1,3))
  else
    image %>% apply(1, sort) %>% aperm(c(2,1))
}

separateChannel <- function(image, layerIndex) { image[,,layerIndex] }

filterChannel <- function(image, layerIndex) { 
  channels = 1:getChannels(image)
  channels = channels[channels != layerIndex]
  image[,,channels] = 0
  image
}
