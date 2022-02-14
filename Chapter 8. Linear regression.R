###CHAPTER 8 time to event analysis 
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

#Package sjPlot checking assumptions with model diagnostic with multivariables regression

mymultimodel1<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CPB+MMstat$CC)
sjPlot::plot_model(mymultimodel1, type='diag')


mymultimodel2<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CPB+MMstat$Age+MMstat$Male+MMstat$Diabetes+MMstat$Aspirin+MMstat$Creatinine+MMstat$CC+MMstat$COPD)
sjPlot::plot_model(mymultimodel2, type='diag')


mymultimodel3<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CC+MMstat$CPB)
sjPlot::plot_model(mymultimodel3, type='diag')


#stat test for residual normality distribution
mymodel<-lm(MMstat$Bleeding~MMstat$CC)
res=rstandard(mymodel)
qqnorm(res)
qqline(res)
shapiro.test(res)

#Excluding outliers both y and x variables 
boxplot(MMstat$CC)
hist(MMstat$CC)
MMstat$CC[MMstat$CC>75]<-NA

boxplot(MMstat$Bleeding, col='lightgreen', xlab='CPB')
hist(MMstat$Bleeding, col='lightgreen', xlab='Bleeding', main='Bleeding')
MMstat$Bleeding[MMstat$Bleeding>800]<-NA

mymodel<-lm(MMstat$Bleeding~MMstat$CC)
res=rstandard(mymodel)
qqnorm(res)
qqline(res)
shapiro.test(res)

plot_model(mymodel, type='diag')

hist(MMstat$CPB, col='green', xlab='CPB', main='CPB')
MMstat$CPB[MMstat$CPB>125]<-NA
mymodel<-lm(MMstat$Bleeding~MMstat$CPB)
plot(MMstat$CPB, MMstat$Bleeding)
plot(MMstat$CPB, MMstat$Bleeding, las=2, col=c('darkgreen', 'lightgreen'))
linearanalysis<-lm(MMstat$Bleeding~MMstat$CPB)
abline(linearanalysis, col='red', lwd=2)
plot(mymodel)

#log
logmymodel<-lm(log(MMstat$Bleeding)~log(MMstat$CC))
res=rstandard(logmymodel)
qqnorm(res)
qqline(res)
shapiro.test(res)
plot_model(logmymodel, type='diag')

#mymultimodel3
res=rstandard(mymultimodel3)
qqnorm(res)
qqline(res)
shapiro.test(res)

#Inlcuding not numeric \ binary variable
broccolimodel<-lm(MMstat$Bleeding~MMstat$Broccoli)
summary(broccolimodel)#predicted bleeding mean in the broccoli group
confint(broccolimodel)
#y=α+βx, 581.7(intercept alpha)-106.3(beta)*1(broccoli=1)

#Converting to factor
MMstat$Broccoli<-ifelse(test=MMstat$Broccoli=='y', yes='1', no='0')
table(MMstat$Broccoli)
MMstat$Broccoli<-as.factor(MMstat$Broccoli)
MMstat$Broccoli<-ifelse(test=MMstat$Broccoli=='1', yes='y', no='n')
table(MMstat$Broccoli)

MMstat[MMstat$Broccoli=='y',]$Broccoli<-'1'
MMstat[MMstat$Broccoli=='n',]$Broccoli<-'0'
table(MMstat$Broccoli)
MMstat$Broccoli<-as.factor(MMstat$Broccoli)
broccolimodel10<-lm(MMstat$Bleeding~MMstat$Broccoli)
summary(broccolimodel10)

MMstat[MMstat$Broccoli=='1',]$Broccoli<-'y'
MMstat[MMstat$Broccoli=='0',]$Broccoli<-'n'


#Multivariable linear regression y=α+β1x1+β2x2…+ε
broccolimodel<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CC)
summary(broccolimodel)
confint(broccolimodel)


#Relevel the reference for ordinal explanatory variable
MMstat$Diabetes<-as.factor(MMstat$Diabetes)
myordinalmodel<-lm(MMstat$Bleeding~MMstat$Diabetes)
summary(myordinalmodel)

#the reference is 0 = no diabetes, now I change the reference to 2, IDDM
MMstat$Diabetes<-relevel(MMstat$Diabetes, ref=2)
mymodelR<-lm(MMstat$Bleeding~MMstat$Diabetes)
summary(mymodelR)


#Collinearity VIF #Multicollinearity check and variables with variance inflation factor above 5 were excluded since considered poor regression estimats. 

library(rms)
vifmodel<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CC+MMstat$CPB)
vif(vifmodel)
plot_model(vifmodel, type='diag')


#Some good practice again: describe the MMstat.csv
install.packages("Hmisc")
library(Hmisc)
describe(MMstat)

#Excluding outliers
boxplot(MMstat$CPB)
hist(MMstat$CPB)
MMstat$CPB[MMstat$CPB>150]<-NA
hist(MMstat)
boxplot(MMstat$CPB)


#Interaction 
intermodel1<-lm(Bleeding~CC*Aspirin, data=MMstat)
summary(intermodel1)
a<-plot_model(intermodel1, type='int')#no interaction

intermodel2<-lm(Bleeding~CPB*Aspirin, data=MMstat)
summary(intermodel2)
b<-plot_model(intermodel2, type='int')#yes interaction

intermodel3<-lm(Bleeding~CPB*Broccoli, data=MMstat)
summary(intermodel3)
c<-plot_model(intermodel3, type='int')#no interaction

intermodel4<-lm(Bleeding~CC*Broccoli, data=MMstat)
summary(intermodel4)
d<-plot_model(intermodel4, type='int')#yes interaction

grid.arrange(a,b,c,d)


#Splitting data MMstat into training and test data
set.seed(2)
library(caTools)#splitting function
split<-sample.split(MMstat, SplitRatio = 0.7)
split
train<-subset(MMstat, split='TRUE')
test<-subset(MMstat, split='FALSE')
train#0.7%
test#0.3%
Model<-lm(Bleeding~Broccoli+CC+CPB, data=train)
summary(Model)

#Prediction
pred<-predict(Model, test)
pred
#comparing predicted vs actual 
plot(test$Bleeding, type='l', lty=1.8, col='red')
lines(pred, type='l', col='blue')
plot(pred, type='l', col='blue' )


#More on linear regression: detecting influential point
cd<-cooks.distance(vifmodel)
plot(cd, ylim=c(0,0.6))

#Full model
#linear regression
varsToFactor <- c("Male","Diabetes","Mortality",'ID', "Aspirin", 'COPD')
MMstat[varsToFactor] <- lapply(MMstat[varsToFactor], factor)

analysisfull<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CPB+MMstat$Age+MMstat$Male+MMstat$Diabetes+MMstat$Aspirin+MMstat$Creatinine+MMstat$CC+MMstat$COPD+MMstat$LVEF+MMstat$Height+MMstat$Weight, na.action=na.exclude)

#similar to
analysisfull<-lm(Bleeding~CC*Broccoli+CPB*Aspirin+Age+Male+Diabetes+Creatinine+COPD+LVEF+Height+Weight, data=MMstat, na.action='na.omit')
summary(analysisfull)
plot_model(analysisfull, type = 'diag')
res=rstandard(analysisfull)
qqnorm(res)
qqline(res)
shapiro.test(res)


#Removing missing values
MMnomiss= na.omit(MMstat)

analysisfull<-lm( Bleeding~Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+CC+COPD+LVEF+Height+Weight, data=MMnomiss)

summary(analysisfull)
regress.display(analysisfull)
confint(analysisfull)
vif(analysisfull)

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

#


analysis<-lm(log(Bleeding)~Broccoli+Aspirin+CPB,data=MMstat)
summary(analysis)
res=rstandard(analysis)
shapiro.test(res)

#reducing height and weight to bmi
bmi<-MMstat$Weight/(MMstat$Height/100)^2# bmi formula = weight/kilogram^2
MMstat$bmi<-bmi # adding the bmi to MMstat
View(MMstat)

attach(MMstat)
analysisfull<-lm(Bleeding~Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+LVEF+bmi, data=MMstat)
summary(analysisfull)

#dropping also the CC
analysisfull<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CPB+MMstat$Age+MMstat$Male+MMstat$Diabetes+MMstat$Aspirin+MMstat$Creatinine+MMstat$COPD+MMstat$LVEF+MMstat$bmi, na.action=na.exclude)
summary(analysisfull)

analysisfull<-lm(Bleeding~Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+ LVEF+bmi, data=MMstat)

res=rstandard(analysisfull)
shapiro.test(res)
plot_model(analysisfull, type='diag')

#try to log the Bleeding and CPB
analysisfull<-lm(log(MMstat$Bleeding)~MMstat$Broccoli+log(MMstat$CPB)+MMstat$Age+MMstat$Male+MMstat$Diabetes+MMstat$Aspirin+MMstat$Creatinine+MMstat$COPD+MMstat$LVEF+MMstat$bmi)
summary(analysisfull)

res=rstandard(analysisfull)
shapiro.test(res)

#removing outliers
boxplot(MMstat$Bleeding)
hist(MMstat$Bleeding)
MMstat$Bleeding[MMstat$Bleeding>900]<-NA
hist(MMstat$CPB)
MMstat$CPB[MMstat$CPB>150]<-NA
hist(MMstat$CPB)
hist(MMstat$Creatinine)
MMstat$Creatinine[MMstat$Creatinine>1.5]<-NA
hist(MMstat$bmi)
MMstat$bmi[MMstat$bmi>35]<-NA
hist(MMstat$LVEF)
MMstat$LVEF[MMstat$LVEF<40]<-NA
hist(MMstat$Age)
MMstat$Age[MMstat$Age<40]<-NA

analysisfull<-lm(MMstat$Bleeding~MMstat$Broccoli+MMstat$CPB+MMstat$Age+MMstat$Male+MMstat$Diabetes+MMstat$Aspirin+MMstat$Creatinine+MMstat$COPD+MMstat$LVEF+MMstat$bmi)

summary(analysisfull)
res=rstandard(analysisfull)
shapiro.test(res)
plot_model(analysisfull, type='diag')

#polynomial

attach(MMstat)
plot(CPB, Bleeding, las=2, col=c('darkgreen', 'lightgreen'))
linearanalysis<-lm(Bleeding~CPB)
summary(linearanalysis)
abline(linearanalysis, col='red', lwd=2)
polyanalysis<-lm(Bleeding~poly(CPB, degree = 2, raw=T))
#similar to
polyanalysis<-lm(Bleeding~CPB+I(CPB^2))
summary(polyanalysis)
lines(smooth.spline(CPB, predict(polyanalysis)), col='blue', lwd=3)
#partial f test to compare the model (null hypothesis no difference)
anova(linearanalysis, polyanalysis)
plot_model(polyanalysis, type='diag')
shapiro.qqnorm(polyanalysis$residuals)

cubeanalysis<-lm(Bleeding~CPB+I(CPB^2)+I(CPB^3))
summary(cubeanalysis)
lines(smooth.spline(CPB, predict(cubeanalysis)), col='green', lwd=3, lty=3)
anova(polyanalysis, cubeanalysis)

plot(CPB, Bleeding, las=2, col=c('darkgreen', 'lightgreen'))
polyanalysis<-lm(Bleeding~Broccoli+CPB+I(MMstat$CPB^2)+Aspirin, data=MMstat)
summary(polyanalysis)
res=rstandard(analysisfull)
shapiro.test(res)
plot_model(polyanalysis, type='diag')




#boxcox transformation

attach(MMstat)
library(MASS)
bc<-boxcox(Bleeding~Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+LVEF+bmi, data=MMstat)
lambda <- bc$x[which.max(bc$y)]
lambda
new_model <- lm(((Bleeding^lambda-1)/lambda) ~ Broccoli+CPB+Age+Male+Diabetes+Aspirin+Creatinine+COPD+LVEF+bmi, data=MMstat)
summary(new_model)
plot_model(new_model, type='diag')

res=rstandard(new_model)
shapiro.test(res)

new_model <- lm(((MMstat$Bleeding^1.11-1)/lambda) ~ MMstat$Broccoli+MMstat$CPB+MMstat$Age+MMstat$Male+MMstat$Diabetes+MMstat$Aspirin+MMstat$Creatinine+MMstat$COPD+MMstat$LVEF)

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

