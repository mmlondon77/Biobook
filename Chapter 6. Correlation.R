###CHAPTER 6 Correlation in R 
##m.moscarelli@imperial.ac.uk

#correlation
#positive and negative and no correation (from mtcars, in built dataset in R)

par(mfrow=c(1,2))

plot(wt~disp, data=mtcars, col=c('blue', 'green'), xlab='x', ylab='y', main='Positive correlation')
abline(lm(wt~disp, data=mtcars), col='grey')

plot(wt~mpg, data=mtcars, col=c('blue', 'green'), xlab='x', ylab='y', main='Negative correlation')
abline(lm(wt~mpg, data=mtcars), col='grey')

plot(qsec~drat, data=mtcars, col=c('blue', 'green'), xlab='x', ylab='y', main='No linear correlation')
abline(lm(qsec~drat, data=mtcars), col='grey')

cor.test(mtcars$qsec,mtcars$drat)

#
plot(MMstat$Bleeding~MMstat$CC, col=c('blue', 'green'), xlab='Cross clamp or ischemic time min', ylab='Bleeding ml')
abline(lm(MMstat$Bleeding~MMstat$CC), col="red")

cor.test(MMstat$Bleeding, MMstat$CC, method='pearson')

#Correlation using ggplot ans shoothing function

ggplot(MMstat, aes(x=CC, y=Bleeding)) + geom_point()+ geom_smooth()

#Log transformation

plot(log(MMstat$Bleeding)~log(MMstat$CC), col=c('blue', 'green'), xlab='log CC time', ylab='log Bleeding')
abline(lm(log(MMstat$Bleeding)~log(MMstat$CC), col="red"))

#Spearman correlation

cor(MMstat$Bleeding, MMstat$CC, method = 'spearman') 

#Pearson correlation

cor(MMstat$Bleeding, MMstat$CC, method = 'pearson')

#Correlation Matrix

MMnumeric<-MMstat[,c(2,4,5,8,9,12,13,14,16)]#numbers indicate the columns that contains numerical variables 

MMnumeric <- MMstat %>%
  dplyr::select_if(is.numeric)#or with tidyverse 

round(cor(MMnumeric), 2)

#library(corrplot)

pairs(MMnumeric, col="darkgreen")
mycor<-(cor(MMnumeric))
corrplot(mycor, method="ellipse", na.label = 'NA')
corrplot(mycor, method=c( 'number'))

#
corrplot(mycor, method=c( 'number'))

#ggcorrplot

library(ggcorrplot)
ggcorrplot(mycor, method = "circle")

ggcorrplot(mycor, type = "lower", lab = TRUE)

p.mat<-cor_pmat(mycor)
ggcorrplot(mycor, hc.order=TRUE, type = "lower", lab = TRUE, p.mat=p.mat)

#Grouped by Broccoli

ggplot(MMstat, aes(x=CC, y=Bleeding, color=Broccoli, fill=Broccoli)) +  geom_point(shape=1)+ geom_smooth(method = 'lm', alpha=0.1)+theme_bw()+labs(y='Bleeding', x='CC',title='Correlation')


ggplot(MMstat, aes(x=CC, y=Bleeding, color=Broccoli, fill=Broccoli)) +  geom_point(shape=1)+ geom_smooth(col='grey')+theme_bw()+labs(y='Bleeding', x='CC',title='Correlation / facet')+facet_grid(~Broccoli)








         
  