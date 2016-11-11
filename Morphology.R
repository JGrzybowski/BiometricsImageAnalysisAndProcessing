source("Filters.R")
## ---- Dilation ----

dilation <- function(layer, filtr = matrix(c(1,1,1,1,NA,1,1,1,1), nrow =3, ncol=3), repetitions  = 1){
  filter2H = floor(getHeight(filtr)/2)
  filter2W = floor(getWidth(filtr)/2)
  
  maxH = getHeight(layer)
  maxW = getWidth(layer)
  
  args = expand.grid(1:maxH, 1:maxW)
  midResult = layer
  
  for(i in 1:repetitions)
  {
    resultLayer <- matrix(
      apply(args, 1, 
        function(hw){
          hrange = ((hw[1]-filter2H) : (hw[1]+filter2H)) %>% clip(1, maxH)
          wrange = ((hw[2]-filter2W) : (hw[2]+filter2W)) %>% clip(1, maxW)
          dilate(midResult[hrange,wrange], filtr, midResult[hw[1],hw[2]])
      }), nrow = maxH)
    midResult = resultLayer
  }
  resultLayer
}  

dilate <- function(patch, pattern, v){
  if((patch - pattern)[pattern == 1] %>% "=="(0) %>% na.omit %>% sum > 0)
    1
  else
    v
}

## ---- Erosion ----

erosion <-function(layer, filtr = matrix(c(1,1,1,1,NA,1,1,1,1), nrow =3, ncol=3), repetitions = 1){
  filter2H = floor(getHeight(filtr)/2)
  filter2W = floor(getWidth(filtr)/2)
  
  maxH = getHeight(layer)
  maxW = getWidth(layer)
  
  args = expand.grid(1:maxH, 1:maxW)
  midResult = layer
  
  for(i in 1:repetitions)
  {
    resultLayer <- matrix(
      apply(args, 1, 
            function(hw){
              hrange = ((hw[1]-filter2H) : (hw[1]+filter2H)) %>% clip(1, maxH)
              wrange = ((hw[2]-filter2W) : (hw[2]+filter2W)) %>% clip(1, maxW)
              erode(midResult[hrange,wrange], filtr, midResult[hw[1],hw[2]])
            }), nrow = maxH)
    midResult = resultLayer
  }
  resultLayer
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

TimeTest <- function(fn){
  ptm = proc.time()
  v = fn
  time =  proc.time() - ptm
  list("time" = time, "fnResults" = v)
}