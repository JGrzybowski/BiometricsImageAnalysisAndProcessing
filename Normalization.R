source("ImageHelpers.R")

## ---- Histograms ----
verticalHistogram <- function(image, threshold = 0.5){
  if(getChannels(image) == 3)
    image %>% binarized(function(x) { x > threshold}) %>% apply(c(3,2), sum)
  else
    image %>% binarized(function(x) { x > thrteshold}) %>% apply(2, sum)
}

horizontalHistogram <- function(image, threshold = 0.5){
  if(getChannels(image) == 3)
    image %>% binarized(function(x) { x > threshold}) %>% apply(c(3,1), sum)
  else
    image %>% binarized(function(x) { x > threshold}) %>% apply(1, sum)
}

## ---- Histogram normalization ----
extrudeHistogram <- function(image, global = TRUE){
  if(global)
  {
    vmax = max(image)
    vmin = min(image)
    
    (image - vmin)/(vmax-vmin)
  } 
  else{
    t_img = image
    for(i in 1:getChannels(image))
      t_img[,,i] = t_img[,,i] %>% extrudeHistogram 
    t_img
  }
}