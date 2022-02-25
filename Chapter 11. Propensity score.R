###Chapter 11. Propensity score matching
#m.moscarelli@imperial.ac.uk

#Pre PSM table
attach(MMstat)
varsToFactor<-c("Male","Diabetes","Mortality","Aspirin", 'COPD')
MMstat[varsToFactor] <- lapply(MMstat[varsToFactor], factor)
vars<- c('Age', 'Weight','Height', 'LVEF', 'Creatinine', 'Male', 'Diabetes','COPD', 'Aspirin', 'LOS','CPB', 'CC', 'Bleeding', 'Mortality')
tableOne <- CreateTableOne(vars = vars, strata = c("Broccoli"), data = MMstat, includeNA = TRUE)
print(tableOne, smd=TRUE)

#propensity score matching
#STEP 1: removing missing values
propMMstat=as.data.frame(na.omit(MMstat))
#STEP 2: converting the treatment to number (0 / 1) and back to factor
propMMstat$Broccoli<-ifelse(propMMstat$Broccoli=='y', 1, 0)
propMMstat$Broccoli<-as.factor(propMMstat$Broccoli)
#STEP 3: calculate the propensity score
p<-matchit(Broccoli~Age+Male+Aspirin+Diabetes+Weight+Height+LVEF+Creatinine+COPD, methods='nearest', ratio=1, caliper=0.2, data=propMMstat)
summary(p, standardize=T)
#STEP 4: perform the match
mm.prop<- match.data(p)
#STEP 5: visualize the results
plot(p, type = "hist")
plot(p, type = "jitter", col='darkgreen')
#STEP 6: create the post PSM table
varsToFactor <- c("Male","Diabetes","Mortality","Aspirin", 'COPD')
mm.prop[varsToFactor] <- lapply(mm.prop[varsToFactor], factor)
vars<- c('Age', 'Weight','Height', 'LVEF', 'Creatinine', 'Male', 'Diabetes','COPD', 'Aspirin')
tableOne <- CreateTableOne(vars = vars, strata = c("Broccoli"), data = mm.prop, includeNA = TRUE)
print(tableOne, smd=TRUE)


#Estimate the treatment effect in the conditioned sample
vars2<- c('LOS','CPB', 'CC', 'Bleeding', 'Mortality')
tableOne <- CreateTableOne(vars = vars2, strata = c("Broccoli"), data = mm.prop, test=FALSE)
print(tableOne, smd=F)

#paired t.test
propbroccoliy<-mm.prop[mm.prop$Broccoli=='1',]
propbroccolin<-mm.prop[mm.prop$Broccoli=='0',]
t.test(propbroccoliy$CPB,propbroccolin$CPB, paired=TRUE)

#mcnemar.test()
Mc<-table(mm.prop$Mortality, mm.prop$Broccoli)
Mc
mcnemar.test(Mc)

#treatment effect estimate using doubly robust adjustment (glm R command)
doubly<-glm(Mortality~Broccoli+Age+Male+Aspirin+Diabetes+Weight+Height+LVEF+Creatinine+COPD+CC+CPB, family='binomial',data=mm.prop)
summary(doubly)


