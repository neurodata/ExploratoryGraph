opt_d<-function(v){
  n<- length(v)
  
  profiled_lik <- function(v1, v2){
    mu1 <- mean(v1)
    mu2 <- mean(v2)
    d1<- v1- mu1
    d2<- v2- mu2
    n<- length(v1) + length(v2)
    sigma2 <- (sum(d1^2) + sum(d2^2))/(n-1)
    -0.5*log(sigma2*2*pi)*n- (n-1)/2
  }
  
  list_p_lik<- do.call("c",lapply(c(1:(n-1)), function(i){
    profiled_lik(v[1:i],v[(i+1):n])
  }))
  
  which.max(list_p_lik)
  
}