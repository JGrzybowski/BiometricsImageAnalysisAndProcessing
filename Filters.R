source("ImageHelpers.R")

## ---- filtring ----
filterImage <- function(image, filtr){
  result <- matrix(image, nrow = getWidth(image))
  for(w in 1: getWidth(image))
  for(h in 1: getHeight(image))      
  {
      hrange = ((h-floor(getHeight(filtr)/2)) : (h+floor(getHeight(filtr)/2))) %>% 
                  clip(1, getHeight(image))
      wrange = ((w-floor(getWidth(filtr)/2)) : (w+floor(getWidth(filtr)/2))) %>%
                  clip(1, getWidth(image))
      result[h,w] = subfilter(image[hrange,wrange], filtr)
  }
  result
}

subfilter <- function(image, filtr){
  m = image * filtr
  sum(m)/sum(abs(filtr))
}

## ---- Filters ----



## ---- Tests ----
test.subfiltering <- function(){
  i <- matrix(c(50,75,100,25,75,100,50,25,75,100,50,75)/100, byrow=T, nrow= 3)
  filtr <- matrix(c(-1,1,0,1,2,1,0,1,-1), nrow = 3, byrow = T)
  
  subresult = subfilter(i[1:3,1:3], filtr)
  if(subresult != sum(c(-50,75,0,75,200,50,0,100,-50)/100)/8)
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