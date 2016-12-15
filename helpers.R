getPowSet <- function(set) {     
  n <- length(set)
  keepBool <- sapply(2^(1:n - 1), function(k) 
    rep(c(FALSE, TRUE), each=k, times=(2^n / (2*k))))
  lapply(1:2^n, function(j) set[keepBool[j, ]])
}

TimeTest <- function(fn){
  ptm = proc.time()
  v = fn
  time =  proc.time() - ptm
  list("time" = time, "fnResults" = v)
}