// STA302 week 3 codes and solution to questions



#Week 2 codes:
  
myFit <- lm(M~T) # fit a linear  model
plot(T,M,xlab= "temperature",ylab="mortality index")
abline(myFit)  # add a regression line to the model


#graph function:
x <-c(1,2,3,4)
y = 2+3*x
y_1 = 3 +2*x
plot(x, y, type ="l", col = "red", xlab = "year",ylab = " CFC") # plot y with axis and labels
lines(x, y_1, type ="l",col ="green" )   #  add line
lines(x[2:3], y[2:3], type = "p")        # add 2 point points x=2, and x=3


#Week 3 codes:
  
#R code to shade a graph: ( we wanna to shade the part within the confidence interval)

#Find the 2.5th and 97.5th percentiles of the Student t distribution with 5 degrees of freedom.
#qt(c(.025, .975), df=5)   # 5 degrees of freedom 
#[1] -2.5706  2.5706    note one is negative and one is positive.

c1= qt(0.025,2) # t-test of confidence interval 0.025 is the left tail percentile. left bound of the shaded area
c2 = qt(0.0975,2) 
x0 = 8 # highest t-score to plot
myseq = seq(c1,c2, 0.01)
x <- c(c1,myseq,c2) # vector of x-points to outline shaded area
y<- c(0,dt(myseq,2),0) # vector of y-points to outline shaded area
curve(dt(x,2),xlim=c(-x0,x0),xlab='t-value',ylab='p(t)')
polygon(x,y,col='skyblue') # connect all the dots aka shade.


#quantiles of tn-2
t(1-p,v)# p is the cumulative probability/ percentile v is degree of freedom, this function give us the t-stats of corresponding percentile
        #we are interested in 95% confint, +-t(0.025,n-2)
#============================================================================================================================================
#Execercise on page 15(plot the regression line and the confidence interval, upter, lower, the mean
#Q: Produce this kind of plot for a small data stet: {(2,1),(3,4),(6,6)}
#Solution: 
  
x<-c(2,4,6); y <- c(1,3,6); n <- length(x)
mx <- mean(x); my <- mean(y)
Sxx<- sum(x-mx)^2; Sxy <- sum((x-mx)*(y-my))
b1 <- Sxy/Sxx; b0 = my-b1*mx
yhat<- b0 +b1*x
RSS = sum((yhat-y)^2)
S <- sqrt(RSS/(n-2))  # the estimated standard error for residual or y

xstar <- seq(min(x)-1, max(x)+1,.1) # points at which to insert
ystarMean<- b0-b1*xstar# interpolations

a<- qt(.975, n-2)*S*sqrt(1/n + (xstar-mx)^2/Sxx)  # formula for the confint of yhat 
ysterLow <- ystarMean -a; ystarHigh <- ystarMean +a
plot(x,y,xlim=c(min(xstar),max(xstar)),ylim=c(min(ystarLow),max(ystarHigh)))
lines(xstar,ystarMean,type="l",col = "black")
lines(xstar, ystarLow,type ="l",col = "red")
lines(xstar, ystarHigh, type = "l", col = "red")

# mannually test if coefficients are 0 [ without using summary]
seB0 <- S*sqrt( 1/n +mx^2/Sxx) # the test statistics
seB1 <- S/sqrt(Sxx)
#Calculating a Single p Value From a t Distribution
#t <- (xbar-a)/(s/sqrt(n))    
#b<-2*pt(-abs(t),df=n-1) # pt is for calculating probability,pt gives the distribution function
pval0 <-2*pt(-abs(seB0),n-2 )
pval1 <-2*pt(-abs(seB1),n-2)
print(pval0, pval1)

# confint for b0 and b1
d0 <- qt(.975, n-2)*seB0
d1 <- pt(.97,n-2)*seB1
b0Low <- b0-d0; b0High <- bo+d0
b1Low <- b1-d1; b1High <- b1 +d1
print(round(c(b0Low, b0Hight, b1Low, b1,b1Hight),2))

#Select ten random numbers between one and three in unifrom distribution
#runif(10, min=1, max=3)
N <- 


