###Chapter 9. Logistic regression
#m.moscarelli@imperial.ac.uk

#Differences between linear and logistic regression
par(mfrow=c(1,2))

MMstat$Mortality<-as.numeric(MMstat$Mortality)
plot(Mortality~CC,data=MMstat, ylim=c(-0.2,1.2), main='Logit regression')
abline(lm(Mortality~CC, data=MMstat), lw=2, col='red')

plot(Bleeding~CC,data=MMstat, main='Linear regression')
abline(lm(Bleeding~CC, data=MMstat), col='red')

#
install.packages('ISLR')
(default <- as_tibble(ISLR::Default))

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(default), replace = T, prob = c(0.6,0.4))
train <- default[sample, ]
test <- default[!sample, ]

model1 <- glm(default ~ balance, family = "binomial", data = train)

default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression ") +
  xlab("CC") +
  ylab("Probability of Dying")

#Cross tabulation
Table<-table(MMstat$Mortality)
Table
addmargins(Table)
round(prop.table(Table),digits=3)
round(100*prop.table(Table),digits=1) # get %s rounded to 1dp

#Table one for 
varsToFactor <- c("Male","Diabetes","Mortality","Aspirin", "COPD")
MMstat[varsToFactor] <- lapply(MMstat[varsToFactor], factor)
vars<- c('Age', 'Weight', 'Height', 'LVEF', 'Creatinine', 'CPB', 'CC', 'Bleeding', 'Broccoli', "Male","Diabetes","Aspirin", 'LOS', 'COPD')
tableOne <- CreateTableOne(vars = vars, strata = c("Mortality"), data = MMstat, includeNA = TRUE)
tableOne
summary(tableOne)
Nonnormal<-c('Age', 'CPB', 'CC', 'LVEF', 'LOS')
print(tableOne, nonnormal=Nonnormal)

tab3Mat <- print(tableOne,  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

#Simple logistic regression, with function glm( )
str(MMstat$Mortality)#checking the structure of the outcome of interest
attach(MMstat)
simplemodel<-glm(Mortality~Age, family = 'binomial'(link=logit), data=MMstat)
summary(simplemodel)

# create a cross tabulation of age and mortality status
mortbybleed<- table(Age, Mortality) 

# output the frequencies of mortality status by age
freqtable <- prop.table(mortbybleed, margin = 1) 
freqtable

# calculate the odds of mortality 
odds <- freqtable[, "1"]/freqtable[, "0"] 

# calculate the log odds 
logodds <- log(odds) 

# plot the Age found in the sample against the log odds of dying 
plot(rownames(freqtable), logodds, col='darkgreen', ylab='Mortality log odds', xlab='Age') 

#Summary simple model with continuous predictor Age
summary(simplemodel)

#Summary simple model with categorical predictor Broccoli
simplemodel2<-glm(Mortality~Broccoli, family = 'binomial'(link=logit), data=MMstat)
summary(simplemodel2)

#Logistic display (epiDisplay)
logistic.display(simplemodel, alpha = 0.05, crude = FALSE, crude.p.value = FALSE, decimal = 2, simplified = FALSE)

#Linearity 
bmi<-MMstat$Weight/(MMstat$Height/100)^2# bmi formula = weight/kilogram^2
MMstat$bmi<-bmi#adding BMI

MMstatL<- na.omit(MMstat)
model <- glm(Mortality~Age+CPB+CC+bmi+LVEF+Creatinine+Bleeding+LOS, data= MMstatL, family = binomial)
summary(model)

probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

MMnumeric<-MMstatL[,c(2,8,9,12,13,14,16,19)]#numbers indicate the columns that contains numerical variables 
MMnumeric <- MMstatL %>%
  dplyr::select_if(is.numeric)#or with tidyverse

predictors <- colnames(MMnumeric)
mylineardata <- MMnumeric %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mylineardata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Leverege, outliers, influential points
par(mfrow = c(1, 3))
set.seed(42)
ex_data  = data.frame(x = 1:10,
                      y = 10:1 + rnorm(n = 10))
ex_model = lm(y ~ x, data = ex_data)


#low leverage, large residual, small influence
point_1 = c(5.4, 11)
ex_data_1 = rbind(ex_data, point_1)
model_1 = lm(y ~ x, data = ex_data_1)
plot(y ~ x, data = ex_data_1, cex = 2, pch = 20, col = "darkgreen",
     main = "A) Low Leverage & Large Residual & Small Influence")
points(x = point_1[1], y = point_1[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_1, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original regression line", "Regression line with added point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, small residual, small influence
point_2 = c(18, -5.7)
ex_data_2 = rbind(ex_data, point_2)
model_2 = lm(y ~ x, data = ex_data_2)
plot(y ~ x, data = ex_data_2, cex = 2, pch = 20, col = "darkgreen",
     main = "B) High Leverage & Small Residual & Small Influence")
points(x = point_2[1], y = point_2[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_2, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c('Original regression line','Regression line with added point'),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, large residual, large influence
point_3 = c(14, 5.1)
ex_data_3 = rbind(ex_data, point_3)
model_3 = lm(y ~ x, data = ex_data_3)
plot(y ~ x, data = ex_data_3, cex = 2, pch = 20, col = "darkgreen", ylim = c(-3, 12),
     main = "C) High Leverage & Large Residual & Large Influence")
points(x = point_3[1], y = point_3[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_3, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original regresison line", "Regression line with added point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))


#Influential values
simplemodel<-glm(Mortality~Age,family='binomial'(link=logit), data=MMstat)
plot(simplemodel, which = 4, id.n = 5)
model.data <- augment(simplemodel) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(5, .cooksd)

model.data %>% 
  filter(abs(.std.resid) > 3)


#Multivariable logistic regression, same command glm( ), multiple predictors
multimodel<-glm(Mortality~Broccoli+Age+Male+Diabetes,family='binomial'(link=logit), data=MMstat)
summary(multimodel)
tab_model(multimodel)

OR<-plot_model(multimodel, transform = 'exp', show.values = TRUE)
lOR<-plot_model(multimodel, transform = NULL, show.values = TRUE)
grid.arrange(OR, lOR)


#McFaden library pscl
pR2(multimodel)

#ROC and c-statistic Library ROCR
v<-c(predict(multimodel,type='response'))
labels=MMstat$Mortality #Mortality is the dependent variable
pred=prediction(v,labels)
perf=performance(pred, 'tpr', 'fpr')
plot(perf, lwd=3, type='l', colorize=T)
performance(pred, 'auc')
abline(0, 1)


#calculate probability of default for each individual in test dataset
predicted <- predict(multimodel, MMstat, type="response")
#calculate AUC
#library(pROC)
auc(MMstat$Mortality, predicted)

#GOF: Deviance
anova(multimodel,test = "Chisq")

#Likelihood Ratio Test
reducedmodel<-glm(Mortality~Broccoli+Age,family='binomial'(link=logit), data=MMstat)
multimodel<-glm(Mortality~Broccoli+Age+Male+Diabetes,family='binomial'(link=logit), data=MMstat)
install.packages('lmtest')
library(lmtest)
anova(reducedmodel, multimodel, test='Chisq')
lrtest(reducedmodel, multimodel)#Given that H0 holds that the reduced model is true, a p-value for the overall model fit statistic that is less than 0.05 would compel us to reject the null hypothesis

#Hosmer-Lemeshow test, library(ResourceSelection)
hl <- hoslem.test(multimodel$y, fitted(multimodel), g=5)
hl#The null hypothesis holds that the model fits the data and in the below example we would reject H0


#Train data set
sample <- sample(c(TRUE, FALSE), nrow(MMstat), replace=TRUE, prob=c(0.7,0.3))

#The training set will be 2/3’s of the full data set, and the testing set will be 1/3

train <- MMstat[sample, ]
test <- MMstat[!sample, ] 

trainanalysis1<-glm(Mortality~Broccoli+Diabetes+Age+Male, data=train,family='binomial')
summary(trainanalysis1)

probabs <- predict(trainanalysis1, test, type='response') 
prediction <- ifelse(probabs > 0.5, 1, 0)
confusionMatrix(factor(prediction), factor(test$Mortality))


