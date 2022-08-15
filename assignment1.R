F_1 <- function(u){
u <- sqrt(8*sqrt(8*u+1)-8)
return(u)
}

f <- function(x){
  x <- (x^3+8*x)/128
  return(x)
}

m <- f(4)

xs <- c()
count <- 0

for (i in 1:1000) {
  u <- 2
  x <- 0
  while (u > f(x)/m) {
    u <- runif(1)
    x <- runif(1,min=0,max = 4)
    count <- count + 1
  }
  xs[i] <- x
}
x1 <- 1:400/100
hist(xs, breaks = 20,prob=TRUE, main='Empirical Histogram and Density for f(x)',xlab='x',ylab='f(x)', xlim=c(0,4),ylim=c(0,0.8))
lines(x1, f(x1), lwd=2, col='red')

ex <- mean(xs)
varx <- var(xs)
