###CHAPTER 3 data types in R 
##m.moscarelli@imperial.ac.uk

#Function structure or class
str(MMstat$Creatinine)
class(MMstat$Creatinine)

#Function levels# How many? 
therapy <- factor(c("no", "surgery", "stent", 'no', 'surgery', 'surgery'))
levels(therapy)

#Converting to factor
str(MMstat)
varsToFactor <- c("Male","Diabetes","Mortality","Aspirin", 'COPD','FUmortality')
MMstat[varsToFactor] <- lapply(MMstat[varsToFactor], factor)

#Data transformation and the benefit of continuous variables
MMstat$newage<-ifelse(MMstat$Age>='60', yes='Over', no='Under')

#Missing variables
install.packages('Amelia')
library(Amelia) 
missmap(MMstat, y.at=500, col=c('red', 'green'), rank.order = FALSE)


#Table one 

varsToFactor <- c("Male","Diabetes","Mortality",'ID', "Aspirin", "COPD")
MMstat[varsToFactor] <- lapply(MMstat[varsToFactor], factor)

vars<- c('Age', 'Weight', 'Height', 'LVEF', 'Creatinine', 'CPB', 'CC', 'Bleeding', "Male","Diabetes","Mortality","Aspirin", 'LOS', 'COPD')

tableOne <- CreateTableOne(vars = vars, strata = c("Broccoli"), data = MMstat, addOverall = TRUE, includeNA = TRUE)
tableOne

summary(tableOne)

Nonnormal<-c('Age', 'CPB', 'CC', 'LVEF')
print(tableOne, nonnormal=Nonnormal)

tab3Mat <- print(tableOne,  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

## Save to a CSV file
write.csv(tab3Mat, file = "myTable2.csv")



