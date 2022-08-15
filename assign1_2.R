g <- function(y,c=1){
  y <- c*(sin(pi*y))^2*((y^3)+8*y)
  return(y)
}

x <- 1:400/100
y <- g(x,0.5)
y1 <- g(x,1)
plot(x,g(x))


# allx <- c()
# ally <- c()
# allg <- c()
xs <- c()
r <- c()
for (j in 1:20) {
  countx <- 0
  for(i in 1:1000){
    u <- 100
    x <- 0
    while (u>g(x)) {
      u <- runif(1,0,100)
      x <- runif(1,0,4)
      countx <- countx + 1
          # allx <- append(allx,x)
          # ally <- append(ally,u)
          # allg <- append(allg,g(x))
    }
    xs[i] <- x
  }
  r[j] <- 1000/countx
}
rates <- mean(r)
c <- 1/(400*rates)
print(c)
x <- 1:400/100
hist(xs, breaks = 50,prob=TRUE, main='Empirical Histogram and Density for g(x)',xlab='x',ylab='g(x)', xlim=c(0,4))
lines(x, c*g(x), lwd=2, col='red')
# df <- data.frame(allx,ally,allg,xs)
# ggplot(df,aes(x=allx,y=allg))
# geom_density(aes(x=xs))+
# geom_point(aes(x=allx,y=ally))
#layer(geom_point(x=allx,y=ally,position = 'identity',stat = 'identity'))


x <- 1:400/100
y <- g(x,0.5)
y1 <- g(x,1)
plot(x,g(x))


# allx <- c()
# ally <- c()
# allg <- c()
xs <- c()
r <- c()
u <- 1
x <- 0
for(i in 1:10000){
  u <- runif(1,0,1)
  x <- runif(1,0,4)
  countx <- countx + 1    
  if(u <= g(x)/100){
      xs <- append(xs,x)
    }
}
mean(xs)
var(xs)
propotion <- length(xs)/10000
c <- 1/(propotion*4*100)

