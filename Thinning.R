library(dplyr)
source("imageHelpers.R")
source("helpers.R")

printToConsole <- function(iteration){
  Sys.sleep(0.1)
  print(iteration)
  flush.console() 
}

loadImage.forThinning <- function(filename){
  readPNG(filename) %>% grayscaled %>% binarized(function(x){x < 0.45})
}

readFingerprint <- function(i){
  filename = paste("Fingers/", "Finger", formatC(i, width = 4, flag = 0), ".png", sep="")  
  readPNG(filename) %>% grayscaled %>% binarized(function(x){x < 0.45}) %>% K3M 
}

test.finger <- function(iterations){
  img = finger
  for(i in 1:iterations)
    img = K3M(finger)
  img
}


