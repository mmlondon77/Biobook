###CHAPTER 11 time to event analysis 
##m.moscarelli@imperial.ac.uk

#selecting the patients who survived after surgery for follow-up

mmstat<-MMstat[MMstat$Mortality=='0',]#selecting the patients who survived the operation
View(mmstat)

#Creating the object myfit for survival analysis

library(survival)
myfit <- survfit(Surv(FUtime, FUmortality) ~ 1, type='kaplan-meier', data = mmstat)#overall cohort

#Plot the survival function with K-M

ggsurvplot(myfit, linetype=1, conf.int = TRUE, xlim=c(0,25), ylim=c(0.9,1),risk.table = TRUE,tables.theme=theme_cleantable(),risk.table.title='Number of patients at risk', tables.height=0.18, surv.scale='percent', xlab='Months')

#summary life table

summary(myfit, time=c(5,10,15,20))#life table

#Cumulative hazard function

ggsurvplot(myfit, fun='cumhaz')

#Adding categorical variable 'Broccoli'

Bfit <- survfit(Surv(FUtime, FUmortality) ~ Broccoli, data = mmstat) 

ggsurvplot(Bfit, linetype=1,  pval = TRUE, pval.method=TRUE, conf.int = TRUE, surv.median.line='hv',xlim=c(0,25),xlab='Months',risk.table =TRUE, tables.theme=theme_cleantable(), risk.table.title='Number of patients at risk', tables.height=0.18, surv.scale='percent', palette=c("#E7B800", "#2E9FDF"), legend.labs = c("noBroccoli","Broccoli"))

summary(Bfit, time=c(5,10,15,20))#life table

#Cox Proportional Hazard (PH) model (or cox regression)

coxfit<-coxph(Surv(FUtime, FUmortality) ~ Broccoli, data=mmstat)
summary(coxfit)# simple regression

# Multiple Cox regression and analysis of the residuals

multifit <- coxph(Surv(FUtime, FUmortality) ~ Broccoli+Male+Age+Bleeding, data=mmstat) 
temp <- cox.zph(multifit)
print(temp)
ggcoxzph(temp)

ggcoxdiagnostics(multifit, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())#Martingale

ggcoxdiagnostics(multifit, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())#Deviance

ggcoxfunctional(Surv(FUtime, FUmortality) ~ CPB + log(CPB) + sqrt(CPB), data=mmstat)#Linear assumptions

#Interaction with time
fit <- coxph(Surv(FUtime, FUmortality) ~ Broccoli + tt(Broccoli), data=MMstat) 
summary(fit)#Time transformed


