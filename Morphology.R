source("Filters.R")
## ---- Dilation ----

dilation <- function(layer, filtr = matrix(c(1,1,1,1,0,1,1,1,1), nrow =3, ncol=3), repetitions  = 1){
  maxH = getHeight(layer)
  maxW = getWidth(layer)
  
  indexes = as.vector(matrix(1:(maxH*maxW),ncol = maxW, nrow = maxH)[1:(maxH), 1:(maxW)])
  
  dataIndexes = cbind(indexes-maxH-1, indexes-maxH, indexes-maxH+1,
                      indexes-1, indexes+0, indexes+1, 
                      indexes+maxH-1, indexes+maxH, indexes+maxH+1)
  
  dataIndexes[which(dataIndexes[,5] %% 5 == 1),c(1,4,7)] = NA
  dataIndexes[which(dataIndexes[,5] %% 5 == 0),c(3,6,9)] = NA
  
  dataIndexes[dataIndexes<1] = NA
  dataIndexes[dataIndexes>maxH*maxW] = NA
  
  windows = matrix(layer[dataIndexes], nrow=length(indexes))
  windows[is.na(windows)] = FALSE
  (windows[,which(filtr==1)] == 1) %>% rowSums() %>% matrix(nrow = maxH, ncol = maxW) %>% "+"(layer) %>% sign()
}

## ---- Erosion ----

erosion <-function(layer, filtr = matrix(c(1,1,1,1,0,1,1,1,1), nrow =3, ncol=3), repetitions = 1){
  maxH = getHeight(layer)
  maxW = getWidth(layer)
  
  indexes = as.vector(matrix(1:(maxH*maxW),ncol = maxW, nrow = maxH)[1:(maxH), 1:(maxW)])
  
  dataIndexes = cbind(indexes-maxH-1, indexes-maxH, indexes-maxH+1,
                      indexes-1, indexes+0, indexes+1, 
                      indexes+maxH-1, indexes+maxH, indexes+maxH+1)
  
  dataIndexes[which(dataIndexes[,5] %% 5 == 1),c(1,4,7)] = NA
  dataIndexes[which(dataIndexes[,5] %% 5 == 0),c(3,6,9)] = NA
  
  dataIndexes[dataIndexes<1] = NA
  dataIndexes[dataIndexes>maxH*maxW] = NA
  
  windows = matrix(layer[dataIndexes], nrow=length(indexes))
  windows[is.na(windows)] = FALSE
  leaveOriginaValuesIndexes = (windows[,which(filtr==1)] == 1) %>% rowSums() %>% "=="(sum(filtr)) %>% ">"(0)
  
  result = matrix(0,ncol = maxW, nrow = maxH)
  result[leaveOriginaValuesIndexes] = layer[leaveOriginaValuesIndexes]
  result
}  

erode <- function(patch, pattern, v){
  if((patch - pattern)[pattern == 1] %>% "<"(0) %>% na.omit %>% sum > 0)
    0
  else
    v
}

## ---- Open Close ----

closing <- function(image, repetitions = 1){
  result = image
  for(i in 1:repetitions)
    result = result %>% dilation() %>% erosion() 
  result
}

opening <- function(image, repetitions = 1){
  result = image
  for(i in 1:repetitions)
    result = result %>% erosion() %>% dilation()
  result
}

## ---- Time ----
test.dilation <- function(){
  i = matrix(c(0,0,0,0,0, 0,1,0,0,0, 0,0,1,1,0, 1,0,0,1,1, 0,1,1,0,0), ncol=5)
  fil = matrix(c(0,0,0,1,0,0,0,1,1), ncol=3)
  expected <- matrix(c(1,1,0,0,0, 0,1,1,1,0, 1,0,1,1,1, 1,1,1,1,1, 0,1,1,1,0), ncol=5)
  result = dilation(i,fil)
  if(T == all.equal(result, expected))
  {  }
  else {
    warning("Dilation TEST FAILED")
    print(result)
    print(expected)
    print(expected-result)
  }
}

test.erosion <- function(){
  i = matrix(c(0,0,0,0,0, 0,1,0,0,0, 0,0,1,1,0, 1,0,0,1,1, 0,1,1,0,0), ncol=5)
  fil = matrix(c(0,0,0,1,0,0,0,1,1), ncol=3)
  expected <- matrix(c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,1,0, 0,0,0,0,0, 0,0,0,0,0), ncol=5)
  result = erosion(i,fil)
  if(T == all.equal(result, expected))
  {  }
  else {
    warning("Erosion TEST FAILED")
    print(result)
    print(expected)
    print(expected-result)
  }
  
}

TimeTest <- function(fn){
  ptm = proc.time()
  v = fn
  time =  proc.time() - ptm
  list("time" = time, "fnResults" = v)
}

test.dilation()
test.erosion()