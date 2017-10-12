#week4 r codes

# montreal protocal example/ dummy vairable/two sample t-test

set.seed(3) # pseudo random generator
y1 <- rnorm(12,1,1);y2 <-rnorm(10,2,1) # y1 is after, y2 is before, y is the CFC content
n1 <- length(y1); n2 <- length(y2)
y<- c(y1,y2)
x <- c(rep(0,n1),rep(1,n2)) # for y1, x =0, for y2 x=1
print(t(matrix(sort(round(y1,2)),ncol=2)))  
# just a fancy way to represent y1 y as a matrix
# t() is used to transpose a matrix or a data frame
# round(n, digit), sort() sort in ascending order
# rep () replicate
print(round(sort(y2),2))  # show data y2

#Method 1(a) Two-sample t-test ( the hard way)
s <-sqrt((var(y1)*(n1-1)+var(y2)*(n2-1))/(n1+n2-2))  # pooled sample standard deviation
tstar <- ((mean(y1)-mean(y2))/(s*sqrt(1/n1 + 1/n2)))# the test statistic
round(tstar,2)
pval <- 2*pt(-abs(tstar), n1+n2-2)
print( round(pval,5))
#method 1(b) two sample test by R founction t.test
t.test(y1,y2, var.equal=TRUE)
#Method 2(a): Dummy variable regression (the hard way)  
n <- length(x); mx <-mean(x) ;my <- mean(y)
Sxx<- sum((x-mx)^2) ;Sxy <- sum((x-mx)(y-my))
b1 <- Sxy/Sxx; b0 <- mean(y)-b1*mean(x)
yhat <- b0+b1*x
RSS <- sum((y-yhat)^2) ;S<-sqrt(RSS/(n-2))
seB0 <- S*sqrt(1/n+mx^2/Sxx)
seB1<- S/sqrt(Sxx)
t0 <-b0/seB0 #test statistic for intercept
t1 <-b1/seB1# test statistic for slope
pval0 <- 2*pt(-abs(t0),n-2); pval1 <- 2*pt(-abs(t1),n-2)
print(c(b0,b1,pval0,pval1))

#Method 2(b); dummy-variable regerssion by R function

myFit <- lm(y~x); summary(myFit) # with free F-test too! this is the most advanced

# you can use cor(x,y) to get the corelation coeffecient
# or cor(z) where z is a matrix or data frame. 



