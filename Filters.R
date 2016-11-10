source("ImageHelpers.R")

## ---- Filtering ----
filterLayer <- function(layer, filtr){
  resultLayer <- matrix(0, ncol = getWidth(layer), nrow = getHeight(layer))
  for(w in 1: getWidth(layer))
  for(h in 1: getHeight(layer))      
  {
      hrange = ((h-floor(getHeight(filtr)/2)) : (h+floor(getHeight(filtr)/2))) %>% 
                  clip(1, getHeight(layer))
      wrange = ((w-floor(getWidth(filtr)/2)) : (w+floor(getWidth(filtr)/2))) %>%
                  clip(1, getWidth(layer))
      resultLayer[h,w] = subfilter(layer[hrange,wrange], filtr)
  }
  resultLayer
}

subfilter <- function(image, filtr){
  m = image * filtr
  sum(m)/sum(filtr)
}

filterImage <- function(image, filtr){
  result = array(0 ,dim = dim(image))
  
  if(getChannels(image) == 1)
    result = image %>% filterLayer(filtr)
  else
    for(channel in 1:getChannels(image))
      result[,,channel] = image %>% separateChannel(channel) %>% filterLayer(filtr)
  result
}

## ---- Filters ----

filter.Gaussian <- function(image, size = 3){
  
  if (size == 3)
    filtr = matrix(c(1,2,1,2,4,2,1,2,1), ncol = 3)
  else
    stop("NOT IMPLEMENTED!")
  
  filterImage(image, filtr)
}

filter.mean <- function(image, size = 3){
  image %>% filterImage(matrix(1, size, size))
}

filter.Sobel <- function(image, edgeDirection, k = 1){
  filtr = matrix(c(1,0,-1,2,0,-2,1,0,-1), ncol=3)
  image %>% filterImage(filtr)
}

filter.sharper <- function(image, k = 1){
  filtr = matrix(c(-k/8),nrow = 3,ncol=3)
  filtr[2,2] = k+1
  
  image %>% filterImage(filtr)
}

## ---- Tests ----
test.subfiltering <- function(){
  i <- matrix(c(50,75,100,25,75,100,50,25,75,100,50,75)/100, byrow=T, nrow= 3)
  filtr <- matrix(c(-1,1,0,1,2,1,0,1,-1), nrow = 3, byrow = T)
  
  subresult = subfilter(i[1:3,1:3], filtr)
  if(subresult != sum(c(-50,75,0,75,200,50,0,100,-50)/100)/4)
    warning("TEST FAILED!")
}
test.filtering <- function(){
  i <- matrix(c(1,2,1,3,4,3,1,2,4),ncol = 3, byrow = T)
  fil <- matrix(c(0,1,0,1,2,1,0,1,0), ncol=3)
  expected <- matrix(c(9,12,9,15,18,18,9,15,21)/6,ncol= 3, byrow=T)
  result = filterImage(i,fil)
  if(T == all.equal(result, expected))
  {  }
  else {
    warning("filtr TEST FAILED")
    print(result)
    print(expected)
  }
}

test.subfiltering()
test.filtering()