g = function(x,v=1){
  p = dnorm(x,0,v)
  return(p)
}
f = function(x){
  p = dnorm(x,0,1)
  return(p)
}
w_x = function(x,v=1){
  return(f(x)/g(x,v))
}

#choosing sigma for g
#in part a we know that sigma need to be larger than sqrt(2)
#and the var get its min when sigma equal to 1
#so let sigma in (0.707,1.414)
sigmas <- 70.7:300/100
xs <- c()
varmiu <- c()
varw_x <- c()
x <- matrix(0,1000,length(sigmas))
w <- matrix(0,1000,length(sigmas))
#generate x from g
#then generate w, miu = mean(w*x)
miu <- c()
adtmiu <- c()
for ( i in 1:length(sigmas)) {
  xs <- rnorm(1000,0,sigmas[i])
  x[,i] <- xs
  w[,i] <- w_x(xs,sigmas[i])
  ex <- mean(x)
  miu[i] <- mean(x[,i]*w[,i])
  adtmiu[i] <- sum(x[,i]*w[,i])/sum(w[,i]) #adjusted miu
  varmiu[i] <- var(x[,i]*w[,i])/1000
  varw_x[i] <- var(w[,i])/1000
}

#show the relation between sigma and mean/var
par(mfrow =c(2,2))
plot(x=sigmas,y=miu)
plot(x=sigmas,y=adtmiu)
plot(x=sigmas,y=varmiu)
plot(x=sigmas,y=varw_x)

