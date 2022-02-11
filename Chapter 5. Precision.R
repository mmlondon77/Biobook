###CHAPTER 5 precision, accuracy measures of central tendency 
##m.moscarelli@imperial.ac.uk

#Sampling distribution

x <- rnorm(10000, mean=90, sd=5)
sample(x, size=10, replace=TRUE)

sample10<-c(90.6, 89, 89, 89.7, 91.5, 89.4, 86.9, 89.2, 89.7, 91.8, 88.7, 88.2, 87.11, 91.2, 91.2, 87.6, 88.9, 91.3, 88.7, 90.4, 89, 90.6, 90, 91.9, 89.3, 91.3, 89.6, 89, 88.1, 89.5, 91.8, 89.7, 88.4, 87.8, 90.1, 89.6, 89.6, 90.7, 89.6, 90.7)
truehist(sample10)
lines(density(sample10))
shapiro.test(sample10)
shapiro.qqnorm(sample10)
length(sample10)
mean(sample10)
mean(x)
sd(sample10)
sd(x)

sample1000<-c(89.9, 90, 90.2, 90.1, 89.7, 89.8, 90, 89.9, 89.7, 89.8, 90.1, 89.8,90, 90, 89.9, 90, 90, 90, 89.8, 89.9, 89.8, 89.9,89.6)
truehist(sample1000, ylim=c(0,3), xlim=c(88, 92))
lines(density(sample1000))
sd(sample1000)


truehist(sample10, ylim=c(0, 3))
lines(density(sample10))
truehist(sample1000, ylim=c(0,3), xlim=c(85, 93))
lines(density(sample1000))

#ggplot

ggplot(Sampledistribution, aes(mean, fill =Sample)) + geom_density(alpha = 0.2)


#Curves with different parameters

par(mfrow=c(1,3))

curve(dnorm(x, 0,1),-6, 10, ylab='', xlab='', axes=F, lwd=2, col='blue')
curve(dnorm(x, 4,1),-6, 10, ylab='', xlab='', lwd=2,axes=F, add=T, lty=2,col='red')
title('Different mean')#Different mean

#
curve(dnorm(x, 0,1),-9, 10, ylab='', xlab='', lwd=2, axes=F, col='blue')
curve(dnorm(x, 0,2),-9, 10, ylab='', xlab='', lwd=2, axes=F, add=T, lty=2, col='red')
title('Different variance')#Different variance

#
curve(dnorm(x,0,1),-12, 13, ylab='', xlab='', axes=F, lwd=2, col='blue')
curve(dnorm(x,4,2),-12, 13, ylab='', xlab='', axes=F, lwd=2, add=T, lty=2, col='red')
title('Different mean and variance')#Different mean and variance


# How to compute SEM
std_mean <- function(sample10) sd(sample10)/sqrt(length(sample10))

#get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]}
getmode(MMstat$Age)
  
  