###CHAPTER 7 Hypothesis testing 
##m.moscarelli@imperial.ac.uk


#Drawing a normal distribution with mean o 90 and sd of f
x <- rnorm(100000, mean=90, sd=5)
hist(x, breaks=500)
truehist(x,xaxt='n', yaxt='n', xlab='', col='white')

# Comparison between normal distribution and t distribution

# Generate a vector of 100 values between -6 and 6
x <- seq(-6, 6, length = 100)

# Degrees of freedom
df = c(2,5,10,30)
colour = c("red", "orange", "blue", "yellow","black")

# Plot a normal distribution
plot(x, dnorm(x), type = "l", lty = 2, xlab = "t-value", ylab = "Density", 
     main = "Comparison of t-distributions vs. Normal", col = "black")

# Add the t-distributions to the plot
for (i in 1:4){
  lines(x, dt(x, df[i]), col = colour[i])
}

# Add a legend
legend("topright", c("d.f. = 2", "d.f. = 5", "d.f. = 10", "d.f. = 30", "Normal"), 
       col = colour, title = "t-distributions", lty = c(1,1,1,1,2))

#Finding the critical value for t distribution

#Left side
qt(p=.05, df=202, lower.tail=TRUE)
#Right side
qt(p=.05, df=202, lower.tail=FALSE)
#Two tail
qt(p=.05/2, df=202, lower.tail=FALSE)

#t.test 1-sample lower tail 
t.test(broccoli$Bleeding, mu=670, lower.tail=TRUE, conf.level = 0.95)

#Independent t-test
bartlett.test(list(broccoli$Bleeding, nobroccoli$Bleeding))
t.test(broccoli$Bleeding, nobroccoli$Bleeding, paired = FALSE,alternative = 'two.sided',  var.equal = FALSE)

#Checking for outliers
ggplot(MMstat, aes(x=Broccoli, y=Bleeding, color=Broccoli)) +theme_bw()+ geom_boxplot() + geom_jitter(shape = 16, position=position_jitter(0.3)) 

#Non parametric test
wilcox.test(broccoli$Bleeding, nobroccoli$Bleeding, paired=FALSE)

#Paired t test
sBP1<-c(140, 138, 129, 135, 145, 173, 183, 130, 101, 101)
sBP2<-c(120, 133, 123, 125, 125, 133, 123, 120, 99, 100)
boxplot(sBP1, sBP2, col='yellow', main='systolic blood pressure')
t.test(sBP1, sBP2,paired=TRUE,alternative = 'two.sided' )
#finding the critical value for alpha 0.05/2 and df=9
qt(p=.05/2, df=9, lower.tail=FALSE)

####Chi square

table(MMstat$Male, MMstat$Broccoli)
chisex<-table(MMstat$Male, MMstat$Broccoli)
barplot(chisex, beside=T, legend=c("Female", "Male"), col=c('green', 'yellow'), main='Sex distribution in the broccoli vs no-broccoli')
chisex
chisq.test(chisex)

#fisher test
fisher.test(chisex)

#Matrix
chisexmat= matrix(c(158,118, 139,85),nrow=2,ncol=2,dimnames=list(c('Female', 'Male'), c('no-Broccoli', 'Broccoli')))
chisexmat
chisq.test(chisexmat)

