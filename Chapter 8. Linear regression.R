###CHAPTER 8. Linear regression 
##m.moscarelli@imperial.ac.uk

#What linear regression is and differences from correaltion

par(mfrow=c(1,2))

plot(MMstat$Bleeding~MMstat$CC, col=c('blue', 'green'), xlab='Cross clamp or ischemic time min', ylab='Bleeding ml')
abline(lm(MMstat$Bleeding~MMstat$CC), col="red")

#Residual
plot(MMstat$Bleeding~MMstat$CC, col=c('blue', 'green'), xlab='Cross clamp or ischemic time min', ylab='Bleeding ml')
abline(lm(MMstat$Bleeding~MMstat$CC), col="red")
segments(x0=105, y0=916, y1=1085, lwd=2)

abline(lm(MMstat$Bleeding~MMstat$CC), col="red")
segments(x0=107, y0=814, y1=924, lwd=2)
segments(x0=120, y0=1004, y1=1186, lwd=2)
segments(x0=140, y0=1015, y1=1117, lwd=2)
segments(x0=154, y0=705, y1=1196, lwd=2)
segments(x0=168, y0=1214, y1=1285, lwd=2)

model<-lm(MMstat$Bleeding~MMstat$CC)

# Obtain fitted values 
model$fitted.values
fitted(model)
predict(model)

# Add fitted values to regression line 
points(MMstat$CC, fitted(model), col = "blue", pch = 16)

library(broom)
model.diag.metrics <- augment(model)
head(model.diag.metrics)

ggplot(model.diag.metrics , aes(MMstat$CC, MMstat$Bleeding)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = MMstat$CC, yend = .fitted), color = "red", size = 0.3)

mymodel<-lm(Bleeding~CC, data=MMstat)
summary(mymodel)
plot_model(mymodel, type='slope')


#The linear regression formula y=α+βx
mymodel<-lm(MMstat$Bleeding~MMstat$CC)
summary(mymodel)
confint(mymodel)

#Package sjPlot checking assumptions with model diagnostic univariable regression
install.packages('sjPlot')
library(sjPlot)
plot_model(mymodel, type='diag')

#multivariable linear regression
broccolimodel<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CC)
summary(broccolimodel)

#Broccoli as factor or number?
MMstat$Broccoli<-ifelse(test=MMstat$Broccoli=='y', yes='1', no='0')#using the function ifelse
table(MMstat$Broccoli)
MMstat$Broccoli<-as.factor(MMstat$Broccoli)# converting Broccoli 0/1 to factor


#Working with ordinal variables (with more than two levels)
MMstat$Diabetes<-as.factor(MMstat$Diabetes)
myordinalmodel<-lm(MMstat$Bleeding~MMstat$Diabetes)
summary(myordinalmodel)

#relev (changing the reference point)
MMstat$Diabetes<-relevel(MMstat$Diabetes, ref=2)# in this case we set the reference level as MMstat$Diabetes2
mymodelR<-lm(MMstat$Bleeding~MMstat$Diabetes)
summary(mymodelR)

#Collinearity
library(rms)#we need this library
vifmodel<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CC+MMstat$CPB)
vif(vifmodel) #using the function vif
plot_model(vifmodel, type='diag')

#interaction with library gridExtra
intermodel1<-lm(Bleeding~CC*Aspirin, data=MMstat)
a<-plot_model(intermodel1, type='int') #no interaction
intermodel2<-lm(Bleeding~CPB*Aspirin, data=MMstat)
b<-plot_model(intermodel2, type='int')#yes interaction
intermodel3<-lm(Bleeding~CPB*Broccoli, data=MMstat)
c<-plot_model(intermodel3, type='int')#no interaction
intermodel4<-lm(Bleeding~CC*Broccoli, data=MMstat)
d<-plot_model(intermodel4, type='int')#yes interaction
grid.arrange(a,b,c,d)


#Including the variables of interest
bmi<-MMstat$Weight/(MMstat$Height/100)^2# bmi formula = weight/kilogram^2
MMstat$bmi<-bmi # adding the bmi to MMstat
analysisfull<-lm(Bleeding~Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+ LVEF+bmi, data=MMstat)
summary(analysisfull)
confint(analysisfull)
plot_model(analysisfull, type='diag')

#Addressing non-linearity

#LOG
analysisfull<-lm(log(Bleeding)~Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+ LVEF+bmi, data=MMstat)#log transformation dependent variable

analysisfull<-lm(log(Bleeding)~Broccoli+log(CPB)+log(Age)+Male+Diabetes+Aspirin+ log (Creatinine)+COPD+log(LVEF)+log(bmi),data=MMstat)#log transformation dependent and independent variables

attach(MMstat)
plot(CPB, Bleeding, las=2, col=c('darkgreen', 'lightgreen'))#attached MMstat
lineanalysis<-lm(Bleeding~CPB)
abline(lineanalysis, col='red', lwd=2)#plotting after log transformation

#POLYNOMIAL
polyanalysis<-lm(Bleeding~poly(CPB, degree = 2, raw=T))#polynomial transformation
#similar to
polyanalysis<-lm(Bleeding~CPB+I(CPB^2))
lines(smooth.spline(CPB, predict(polyanalysis)), col='blue', lwd=3)
cubeanalysis<-lm(Bleeding~CPB+I(CPB^2)+I(CPB^3))
anova(polyanalysis,cubeanalysis)#comparing models with anova


#BOX-COX
attach(MMstat)
library(MASS)
bc<-boxcox(Bleeding~Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+LVEF+bmi, data=MMstat)
lambda <- bc$x[which.max(bc$y)]
lambda

new_model <- lm(((Bleeding^lambda-1)/lambda) ~ Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+LVEF+bmi, data=MMstat)

plot(new_model)# This time I used the basic function ‘plot’ to check the model. I could also use the function plot_model


#REMOVING OUTLIERS
hist(MMstat$Bleeding, col='lightgreen', xlab='Bleeding', main='Bleeding')
ggdensity(MMstat$Bleeding, main = "Density plot of response variable Bleeding", xlab = "Bleeding ml", col='darkgreen', lwd=1)
MMstat$Bleeding[MMstat$Bleeding>800]<-NA
analysisfull<-lm(Bleeding~Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+
                   LVEF+bmi, data=MMstat)
plot_model(analysisfull, type='diag')


#Excluding outliers both y and x variables 
boxplot(MMstat$CC)
hist(MMstat$CC)
MMstat$CC[MMstat$CC>75]<-NA

boxplot(MMstat$Bleeding, col='lightgreen', xlab='CPB')
hist(MMstat$Bleeding, col='lightgreen', xlab='Bleeding', main='Bleeding')
MMstat$Bleeding[MMstat$Bleeding>800]<-NA


#More on linear regression: detecting influential point
cd<-cooks.distance(vifmodel)
plot(cd, ylim=c(0,0.6))

#gam
model<-gam(Bleeding~Broccoli+s(CPB)+s(Age)+Male+Diabetes+Aspirin+s(Creatinine)+COPD+s(LVEF)+s(bmi), data=MMstat2, method='REML')
summary(model)

model<-gam(Bleeding~Broccoli+s(CPB, k=2)+s(Age)+Male+Diabetes+Aspirin+s(Creatinine)+COPD+s(LVEF)+s(bmi), data=MMstat, method='REML')

model<-gam(Bleeding~Broccoli+s(CPB, by=Aspirin)+s(Age)+Male+Diabetes+Aspirin+s(Creatinine)+COPD+s(LVEF)+s(bmi), data=MMstat, method='REML')#factor smooth interaction by argument
summary(model)

modelinteraction<-gam(Bleeding~Broccoli+s(CPB, Age)+Male+Diabetes+s(Creatinine)+COPD+s(LVEF)+s(bmi), data=MMstat, method='REML')
summary(modelinteraction)
plot(modelinteraction, scheme=1)
plot(modelinteraction, scheme=2)
vis.gam(x=modelinteraction, view=c('CPB', 'Age'), plot.type ='persp')
vis.gam(x=modelinteraction, view=c('CPB', 'Age'), plot.type ='contour')
vis.gam(x=modelinteraction, view=c('CPB', 'Age'), plot.type ='persp', se=2)
vis.gam(x=modelinteraction, view=c('CPB', 'Age'), plot.type ='persp', se=2, theta=220)
vis.gam(x=modelinteraction, view=c('CPB', 'Age'), plot.type ='persp', se=2, phi=50)
vis.gam(x=modelinteraction, view=c('CPB', 'Age'), plot.type ='persp', se=2, r=0.1)

#factor interaction
model<-gam(Bleeding~s(CPB, Male, bs='fs'), data=MMstat, method='REML')
#no additional linear term aspirin
summary(model)
vis.gam(x=model, plot.type ='persp', se=2, theta=220)

#tensor
modeltensor<-gam(Bleeding~Broccoli+te(CPB, Age)+Male+Diabetes+s(Creatinine)+COPD+s(LVEF)+s(bmi), data=MMstat, method='REML')
vis.gam(x=modeltensor, view=c('CPB', 'Age'), plot.type ='persp', se=2, theta=220)

model<-gam(Bleeding~Broccoli+s(CPB)+s(Age)+Male+Diabetes+Aspirin+s(Creatinine)+COPD+s(LVEF)+s(bmi), data=MMstat, method='REML')

plot.gam(model, pages=1)
plot.gam(model, rug=TRUE, residuals = TRUE, pch=1, cex=1, shade=TRUE, col='blue', shift=(coef(model)[1]))

gam.check(model)

concurvity(model, full=TRUE)

concurvity(modelgam, full=TRUE)

model<-gam(MMstat$Bleeding~s(MMstat$CPB, by=MMstat$Aspirin)+MMstat$Aspirin,method='REML')#factor smooth interaction by argument

plot_smooths(model, series=MMstat$CPB)
predict(model)

library(tidymv)
plot_smooths(model=model,series=CPB, comparison=Broccoli)
AIC(model)
anova(analysisfull, model, test='Chisq')
coef(model)

#homoscedasticity
attach(LungCapData)
newmodel<-lm(LungCap~Age, data=LungCapData)
summary(newmodel)
plot(newmodel, page=1)
plot_model(newmodel, type='diag')

#Stepwise linear regression direction 'both'
newaboth<-step(analysisfull, trace=T, direction = 'both')
summary(newaboth)
regress.display(newaboth, decimal = 2)
plot_model(newaboth, type='diag')

#Stepwise linear regression direction 'forward'
newaforward<-step(analysisfull, trace=TRUE, direction = 'forward')
summary(newaforward)
regress.display(newa, decimal = 2)
plot_model(newa, type='diag')

#Stepwise linear regression direction 'backward'
MMnomiss= na.omit(MMstat)
analysisfull<-lm( Bleeding~Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+LVEF+bmi, data=MMnomiss)
newabackward<-step(analysisfull, trace=FALSE, direction = 'backward')
summary(newabackward)

plot_model(newabackward, type='diag')

