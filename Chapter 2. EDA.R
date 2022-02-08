###CHAPTER 2 Exploratory data analysis 
##m.moscarelli@imperial.ac.uk
#Start to exploring the dataset MMstat

#To obtain the file MMstat.csv, you should download it from GitHub (https://github.com/mmlondon77/Biobook.git),  As a remember, to locate or to change your working directory you may use the function getwd( )

View(MMstat)#View the dataset
dim(MMstat)#dimension of the dataset
names(MMstat)#variables names
ls(MMstat) #list alphabetic order
head(MMstat)#first six rows
tail(MMstat)#last six rows
str(MMstat)#data structure


#Extracting variables 
# $ or attaching

attach(MMstat)

#subsetting with brackets [ ] some examples

MMstat$Male[1:10]#first 10 
head(MMstat$Male)#head
tail(MMstat$Male)#tail

Ageover50<-MMstat$Age[MMstat$Age>50]
print(Ageover50)#see the object
length(Ageover50)#how many
boxplot(Ageover50, col='green')
which(MMstat$Age>50)
Ageover50<-MMstat$Age[MMstat$Age>=50]#equal or above 50 y/o
length(Ageover50)

Ageover50<-MMstat[MMstat$Age>=50,]
View(Ageover50)

Ageover50<-MMstat[MMstat$Age>=50 & MMstat$Broccoli=='y',]
Ageover50<-MMstat[MMstat$Age>=50 & MMstat$Broccoli=='y' & MMstat$Aspirin=='1',]
View(Ageover50)

Ageover50broccoli<-MMstat[MMstat$Age>50 & MMstat$Broccoli=='y' & MMstat$Aspirin=='1',]

Ageover50nobroccoli<-MMstat[MMstat$Age>50 & MMstat$Broccoli=='n' & MMstat$Aspirin=='1',]

boxplot(Ageover50broccoli$Age, Ageover50nobroccoli$Age, col=c('red', 'blue'), boxwex=0.4, ylim=c(50,100), ylab='years old', main= 'Patients over 50 with Aspirin')
axis(1, at=1:2, labels=c('Broccoli','noBroccoli'))

#What is the mean age of the Broccoli group? And subpopulation mean

mean(MMstat$Age[MMstat$Broccoli== 'y'])

mean(MMstat$Age[MMstat$Broccoli== 'y' & MMstat$Male==1])

mean(MMstat$Age[MMstat$Broccoli== 'y' & MMstat$Male==1 & MMstat$Aspirin==0])


#creating the two groups Broccoli vs. no.Broccoli

broccoli<-MMstat[MMstat$Broccoli=='y',]
View(broccoli)
nobroccoli<-MMstat[MMstat$Broccoli=='n',]

mean(broccoli$Age)
mean(nobroccoli$Age)

broccoli<-MMstat[MMstat$Broccoli=='y' & MMstat$Male=='1' & MMstat$Aspirin=='0',]

View(broccoli)

broccoli<-MMstat[MMstat$Broccoli=='y' & MMstat$Male=='1' & MMstat$Aspirin=='0' & MMstat$Age>='80',]

View(broccoli)

broccoli<-MMstat[MMstat$Broccoli=='y' & MMstat$Male=='1' & MMstat$Aspirin=='0' & MMstat$Age>='60',]

View(broccoli)


#Some boxplot of the age of the broccoli vs noBroccoli

boxplot(broccoli$Age, nobroccoli$Age,col=c('red', 'blue'), xaxt='n', main='Age y/o in the two groups')
axis(1, at=1:2, labels=c('Broccoli','noBroccoli'))


broccoli<-MMstat[MMstat$Broccoli=='y' & MMstat$Age>='80' & MMstat$Age<='85',]

#similar to 

broccoli<-MMstat[c(MMstat$Broccoli=='y' & MMstat$Male=='1' & MMstat$Aspirin=='0' & MMstat$Age<='60' & MMstat$Age>'50'),]

View(broccoli)

#How to remove missing observation

MMstatnomissing<-MMstat[!(is.na(MMstat$COPD) | is.na(MMstat$LOS)),]
View(MMstatnomissing)
is.na(MMstatnomissing$COPD)

#

mean(MMstat$Age)
attach(MMstat)
mean(Age)

############
###################################################################
mmsubset<-as_tibble(MMstat)#storing as local dataset, mor neat presentation of the dataset little cumberson
mmsubset

#FILTER, filtering the row
broccolidpl<-filter(mmsubset, Broccoli=='y')
View(broccolidpl)

broccolidpl<-filter(mmsubset, Broccoli=='y', COPD=='0')

broccolidpl<-filter(mmsubset, Age>='80')

#use pipe operator

broccolidpl<-filter(mmsubset, Diabetes=='1'| Diabetes=='0')
View(broccolidpl)

#or the %in% operator (index operator)
broccolidpl<-filter(mmsubset, Diabetes %in% c('1', '2'))

#SELECT, selcting the columns

broccolidpl<-dplyr::select(mmsubset, Diabetes, Broccoli)
broccolidpl<-dplyr::select(mmsubset, ID:Broccoli)#from ID to Broccoli
broccolidpl<-dplyr::select(mmsubset,starts_with('Bro'))

View(broccolidpl)

#chaining and pipelining (filter and select)

broccolidpl<-filter(dplyr::select(mmsubset, Broccoli, Age), Age > 80)
View(broccolidpl)
head(broccolidpl)

#chaining it is better and more intuitive % rather than %

broccolidpl<-mmsubset %>%
  dplyr::select(Broccoli, Age)%>%
  filter(Age>80)

View(broccolidpl)

#ARRANGE reorder rows

broccolidpl<-mmsubset %>%
  dplyr::select(Broccoli, Age)%>%
  arrange(Broccoli)

View(broccolidpl)

broccolidpl<-mmsubset %>%
  dplyr::select(Broccoli, Age)%>%
  arrange(asc=Age)

#MUTATE, it adds new variables


broccolidpl<-mmsubset %>%
  dplyr::select(Broccoli, Age, CC, CPB)%>%
  mutate(ONBH=CPB-CC)

View(broccolidpl)

#SUMMARISE reduce variables to values


broccolidpl<-mmsubset %>%
  group_by(Broccoli)%>%
  summarise(Avaragebleeding=mean(Bleeding, na.rm=T))
View(broccolidpl)

broccolidpl<-mmsubset %>%
  group_by(Age)%>%
  summarise(Avaragebleeding=mean(Bleeding, na.rm=T))
View(broccolidpl)

#function summarise_each

broccolidpl<-mmsubset %>%
  group_by(Age)%>%
  summarise_each(mean, Bleeding, CPB)
View(broccolidpl)

broccolidpl<-mmsubset %>%
  group_by(Age)%>%
  summarise_each(funs(min(.,na.rm=TRUE), max(., na.rm=TRUE)),
matches('Bleeding'))
View(broccolidpl)

#tally counting the observation according strata (n function)

broccolidpl<-mmsubset %>%
  group_by(Age, Broccoli)%>%
  tally(sort=TRUE)

View(broccolidpl)

#table function

mmsubset %>%
  group_by(Broccoli, Male)%>%
  dplyr::select(Mortality)%>%
table()%>%
head()

