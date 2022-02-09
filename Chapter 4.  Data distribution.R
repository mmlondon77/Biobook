###CHAPTER 4 Data distribution in R 
##m.moscarelli@imperial.ac.uk

#Create normal distribution
x = seq(-15, 15, by=0.1)
y = dnorm(x, mean(x), sd(x))
plot(x,y, col='red', ylab='', xlab='', axes=FALSE, main='The normal distribution', lwd=3, type='l')

#Histogram for normal distribution
x <- rnorm(10000, mean=90, sd=5)
hist(x, breaks=50)
truehist(x, axes=FALSE, ylab='', xlab='')#from the library MASS
lines(density(x))

#Normality test
shapiro.test(MMstat$Age)
library(epiDisplay) 
shapiro.qqnorm(MMstat$Age)

#QQ plot
library(ggpubr)
ggqqplot(MMstat$Age)

#Density plot
ggdensity(MMstat$Age, main = "Density plot of Age whole cohort", xlab = "Age", col='darkgreen', lwd=2)

#boxplot
boxplot(MMstat$LVEF,  boxwex=1, col='yellow', ylim=c(20,80), xlim=c(0,2), main='Left ventricle ejection fraction')
text(x=0.2, y=30, 'Outliers')
arrows(y0=30, x0=0.4, x1 = 0.9, lwd=1, col="blue", lty=1,angle=30)
text(x=0.2, y=72, 'Outliers')
arrows(y0=72, x0=0.4, x1 = 0.9, lwd=1, col="blue", lty=1,angle=30)

text(x=0.2, y=66, 'Maximum')
arrows(y0=65, x0=0.45, x1 = 0.8, lwd=1, col="black", lty=1,angle=0)

text(x=0.2, y=55.5, 'Median')
arrows(y0=55, x0=0.41, x1 = 0.7, lwd=1, col="black", lty=1,angle=0)

text(x=0.2, y=44.5, 'Minimum')
arrows(y0=44.2, x0=0.45, x1 = 0.8, lwd=1, col="black", lty=1,angle=0)

text(x=1.3, y=57, 'Q3')
text(x=1.3, y=49.5, 'Q1')

text(x=0.2, y=40, '(Q1-1.5*IQR)', cex=0.8)
text(x=0.2, y=62, '(Q3+1.5*IQR)', cex=0.8)

arrows(y0=57, x0=1.4, x1 = 1.5, lwd=1, col="black", lty=1,angle=0)
arrows(y0=49.5, x0=1.4, x1 = 1.5, lwd=1, col="black", lty=1,angle=0)

arrows(y0=49.5, x0=1.44, y1 = 57, lwd=1, col="black", lty=1,angle=0)
text(x=1.75, y=53.5, 'Interquartile range', cex=0.8)

text(x=1.3, y=46, '(25th percentile)', cex=0.8)
text(x=1.3, y=60, '(75th percentile)', cex=0.8)

### at glance
par(mfrow=c(2,2))
hist(MMstat$LVEF, col='yellow', xlab='LVEF%', main='LVEF histogram')
boxplot(MMstat$LVEF, col='green', main= 'LVEF boxplot', boxwex=0.5)
shapiro.qqnorm(MMstat$LVEF, title='Q-Q')
densityplot(MMstat$LVEF, xlab='LVEF', main='Densityplot')

##
boxplot(MMstat$Age~MMstat$Broccoli, col=c('lightgreen', 'darkgreen'), main='Age Broccoli vs no-Broccoli', ylim=c(0,100), boxwex=0.5, xlab='Group: Broccoli=y, no-Broccoli=n', ylab='Age y/o')
wilcox.test(Broccoli$Age, noBroccoli$Age, paired=FALSE)


### ggplot

ggplot(MMstat, aes(x=Broccoli, y=Age, color=Broccoli)) +theme_bw()+ geom_boxplot() + geom_jitter(shape = 16, position=position_jitter(0.3)) +xlab("Group") + ylab("Age y/o")+labs(title='Boxplot with jittering')+theme(axis.title.x = element_text(size=14),axis.text.x = element_text(size=14), axis.text.y=element_text(size=14),axis.title.y=element_text(size=14), legend.position = "none")+ scale_x_discrete(labels=c("n" = "no-Broccoli", "y" = "Broccoli"))


#Barplot
par(mfrow= c(1,2))

class<-c('no-Broccoli', 'Broccoli')
barplot(table(MMstat$Diabetes,MMstat$Broccoli),main='Diabetic patients', col=c('lightgreen', 'yellow', 'red'), ylim=c(0,400), ylab='Number',names.arg=class, beside=TRUE, border=NA)
legend("topright", c('none', 'NIDDM', 'IDDM'), bty='n',lty=c(1,1), lwd=3, col=c('lightgreen', 'yellow', 'red'),cex=1.5)

class<-c('no-Broccoli', 'Broccoli')
barplot(table(MMstat$Diabetes,MMstat$Broccoli),main='Diabetic patients', col=c('lightgreen', 'yellow', 'red'), ylim=c(0,400), ylab='Number',names.arg=class, beside=FALSE, border=NA)
legend("topright", c('none', 'NIDDM', 'IDDM'), bty='n',lty=c(1,1), lwd=3, col=c('lightgreen', 'yellow', 'red'),cex=1.5)


        