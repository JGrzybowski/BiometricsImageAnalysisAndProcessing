source("ImageHelpers.R")

## ---- Filtering ----
filterLayer <- function(layer, filtr){
  filter2H = floor(getHeight(filtr)/2)
  filter2W = floor(getWidth(filtr)/2)
  
  maxH = getHeight(layer)
  maxW = getWidth(layer)
  
  args = expand.grid(1:maxH, 1:maxW)
  
  resultLayer <- matrix(apply(args, 1, 
    function(hw){
      hrange = ((hw[1]-filter2H) : (hw[1]+filter2H)) %>% clip(1, maxH)
      wrange = ((hw[2]-filter2W) : (hw[2]+filter2W)) %>% clip(1, maxW)
      subfilter(layer[hrange,wrange], filtr)
    }),
    nrow = maxH
  )
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

filter.sharper <- function(image, g= 3, k = 6){
  filtr = laplasian(g,k)
  filtr[2,2] = k+1
  
  image %>% filterImage(filtr)
}

laplasian <- function(g,k){
       if (g == 1) matrix(c(0,-k,0,-k,4*k,-k,0,-k,0)/4, nrow=3)
  else if (g == 2) matrix(c(k,k,k,k, -8*k, k,k,k,k)/-8, nrow=3)
  else if (g == 3) matrix(c(k,2*k,k,2*k, -12*k, 2*k,k,2*k,k)/-12, nrow=3)
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