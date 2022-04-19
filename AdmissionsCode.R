
# ADMISSIONS ASSIGNMENT - MORGAN DULAK #

#### LIBRARIES ####
library(tidyverse)
library(stringr)
library(dplyr)
library(DataExplorer)
library(mice)
library(plyr)
library(rapport)
library(ggplot2)
library(DMwR2)
library(Metrics)
library(dplyr)
library(caret)
library(e1071)
library(tree)
library(randomForest)
library(gbm)
library(class)
library(mice)

#### READING IN DATA ####

data <- read.csv("TU.csv")
data <- as.data.frame(data)

str(data)
sum(sapply(data, is.logical))

#### RENAMING FEATURES ####

data <- dplyr::rename(data,Entry.Term = Entry.Term..Application.)
data <- dplyr::rename(data,Origin.Date = First_Source.Origin.First.Source.Date)
data <- dplyr::rename(data, InquiryDate = Inquiry.Date)
data <- dplyr::rename(data, Sport2Rating = Sport.2.Rating)
data <- dplyr::rename(data, StaffAssignedName = Staff.Assigned.Name)
data <- dplyr::rename(data, Admit.Type = Admit.Type)
data <- dplyr::rename(data, Sport1 = Sport.1.Sport)
data <- dplyr::rename(data, Sport1Rating = Sport.1.Rating)
data <- dplyr::rename(data, Sport2 = Sport.2.Sport)
data <- dplyr::rename(data, Sport3 = Sport.3.Sport)
data <- dplyr::rename(data, Sport3Rating = Sport.3.Rating)
data <- dplyr::rename(data, Interest1=Academic.Interest.1)
data <- dplyr::rename(data, Interest2=Academic.Interest.2)
data <- dplyr::rename(data,Origin.Summary = First_Source.Origin.First.Source.Summary)
data <- dplyr::rename(data,Event.Participation = Total.Event.Participation)
data <- dplyr::rename(data,Campus.Visits = Count.of.Campus.Visits)
data <- dplyr::rename(data,Organization.Category = School..1.Organization.Category)
data <- dplyr::rename(data,School1Code = School.1.Code)
data <- dplyr::rename(data,School1ClassRank = School.1.Class.Rank..Numeric.)
data <- dplyr::rename(data,School1ClassSize = School.1.Class.Size..Numeric.)
data <- dplyr::rename(data,School1GPA = School.1.GPA)
data <- dplyr::rename(data,School1GPAScale = School.1.GPA.Scale)
data <- dplyr::rename(data,School1GPARecalculated = School.1.GPA.Recalculated)
data <- dplyr::rename(data,School2ClassRank = School.2.Class.Rank..Numeric.)
data <- dplyr::rename(data,School2ClassSize = School.2.Class.Size..Numeric.)
data <- dplyr::rename(data,School2GPA = School.2.GPA)
data <- dplyr::rename(data,School2GPAScale = School.2.GPA.Scale)
data <- dplyr::rename(data,School2GPARecalculated = School.2.GPA.Recalculated)
data <- dplyr::rename(data,School3ClassRank = School.3.Class.Rank..Numeric.)
data <- dplyr::rename(data,School3ClassSize = School.3.Class.Size..Numeric.)
data <- dplyr::rename(data,School3GPA = School.3.GPA)
data <- dplyr::rename(data,School3GPAScale = School.3.GPA.Scale)
data <- dplyr::rename(data,School3GPARecalculated = School.3.GPA.Recalculated)
data <- dplyr::rename(data, SAT.I = SAT.I.CR...M)
data <- dplyr::rename(data, SAT.R = SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)
data <- dplyr::rename(data, Financial.Aid = Intend.to.Apply.for.Financial.Aid.)
data <- dplyr::rename(data, SAT.Concordance.Score = SAT.Concordance.Score..of.SAT.R.)
data <- dplyr::rename(data, ACT.Concordance.Score.Reading = ACT.Concordance.Score..of.SAT.R.)
data <- dplyr::rename(data, ACT.Concordance.Score = ACT.Concordance.Score..of.SAT.)
data <- dplyr::rename(data, SAT.W = SAT.R.Evidence.Based.Reading.and.Writing.Section)

#### FIXING BLANKS ####

data$Ethnicity[data$Ethnicity == ""] <- "No Answer"
data$Race[data$Race == ""] <- "No Answer"
data$Religion[data$Religion == ""] <- "No Answer"
data$Legacy[data$Legacy == ""] <- "Not Legacy"
data$Athlete[data$Athlete == ""] <- "Not Athlete"
data$Sport1[data$Sport1 == ""]<- "None"
data$Sport2[data$Sport2 == ""]<- "None"
data$Sport3[data$Sport3 == ""]<- "None"
data$Sport1Rating[data$Sport1Rating == ""]<- "Not Applicable"
data$Sport2Rating[data$Sport2Rating == ""]<- "Not Applicable"
data$Sport3Rating[data$Sport3Rating == ""]<- "Not Applicable"
data$Interest1[data$Interest1 == ""]<- "Undecided"
data$Interest2[data$Interest2 == ""]<- "Undecided"

#There was few blanks, filled with "Gail Robertson", the most common admissions counselor
#Creating an "other category" not possible, due to low volume of blanks
data$StaffAssignedName[data$StaffAssignedName == ""]<- "Gail Roberson"

data$Permanent.Geomarket[data$Permanent.Geomarket == ""]<- "OT"
data$Permanent.Country[data$Permanent.Country == ""]<- "NA"


lapply(data,class)
lapply(data,levels)

#### CREATING FACTORS ####

data$Entry.Term <- as.factor(data$Entry.Term)
data$Admit.Type <- as.factor(data$Admit.Type)
data$Permanent.Country<- as.factor(data$Permanent.Country)
data$Sex <- as.factor(data$Sex)
data$Ethnicity <- as.factor(data$Ethnicity)
data$Race <- as.factor(data$Race)
data$Entry.Term <- as.factor(data$Entry.Term)
data$Religion <- as.factor(data$Religion)
data$Application.Source <- as.factor(data$Application.Source)
data$Decision.Plan <- as.factor(data$Decision.Plan)
data$StaffAssignedName <- as.factor(data$StaffAssignedName)
data$Legacy <- as.factor(data$Legacy)
data$Athlete<- as.factor(data$Athlete)
data$Sport1 <- as.factor(data$Sport1)
data$Sport1Rating <- as.factor(data$Sport1Rating)
data$Sport2 <- as.factor(data$Sport2)
data$Sport2Rating <- as.factor(data$Sport2Rating)
data$Sport3 <- as.factor(data$Sport3)
data$Sport3Rating <- as.factor(data$Sport3Rating)
data$Interest1 <- as.factor(data$Interest1)
data$Interest2 <- as.factor(data$Interest2)
data$Origin.Summary<- as.factor(data$Origin.Summary)
data$Organization.Category <- as.factor(data$Organization.Category)
data$School1Code <- as.factor(data$School1Code)
data$School1GPAScale <- as.factor(data$School1GPAScale)
data$School2GPAScale <- as.factor(data$School2GPAScale)
data$School3GPAScale <- as.factor(data$School3GPAScale)
data$Citizenship.Status <- as.factor(data$Citizenship.Status)
data$Academic.Index<- factor(data$Academic.Index, levels = c(1,2,3,4,5))
data$Financial.Aid <- as.factor(data$Financial.Aid)
data$Test.Optional <- as.factor(data$Test.Optional)
data$Decision <- as.factor(data$Decision)


#### FIXING NA's ####

#REMOVING FEATURES
#removing date columns due to high variation
#removing postal code due to high variation
#removing "admit type" because all values are FY

data <- data %>% select(-Origin.Date,-InquiryDate,-Submitted,-Permanent.Postal,-Admit.Type)

#removing school1code due to high variation

data <- data %>% select(-School1Code)

plot_missing(data,missing_only = T)

#removing all columns under "bad" or "remove" that cannot be replaced with 'None'
#these all have too much missing data to reasonably fill in

data <- data %>% select(-School2ClassRank,-School2ClassSize,-School2GPA, -School2GPAScale, -School2GPARecalculated,
                        -School3ClassRank, -School3GPA, -School3GPAScale, -School3GPARecalculated,-ACT.Concordance.Score,
                        -ACT.Writing,-SAT.I.Writing, -SAT.I, -SAT.I.Critical.Reading, SAT.I.Math, -School3ClassSize,
                        -SAT.I.Math,Test.Optional,-School1GPA,-School1GPAScale,-SAT.W,-SAT.R.Math.Section,-ACT.Concordance.Score.Reading,
                        -School1ClassRank,-School1ClassSize, -ACT.English, -ACT.Reading, -ACT.Math, -ACT.Science.Reasoning, -ACT.Composite,
                        -SAT.R, -SAT.Concordance.Score, -Test.Optional,-Sport3Rating,-Sport2Rating,-Origin.Summary)

# now there's only very few columns with missing data to fill in


# looking into NA's in financial aid

sum(is.na(data$Financial.Aid))
data[is.na(data$Financial.Aid),]
summary(data$Financial.Aid)

# 2/3 of students have financial aid, so filling in with 1
count(data$Financial.Aid)
data$Financial.Aid[is.na(data$Financial.Aid)] <- 1

#first academic index

# I found that all students without an academic index score are foreign nationals.
# Filling in with a 3 (as it is on a scale of 1-5) and the method behind it's calculation is not revealed


data$Academic.Index[is.na(data$Academic.Index)] <- 3

#### RECODING ####

#SPORTS

#removing genders from sports
#combining diving and swimming 

table(data$Sport1)

data$Sport1 <- revalue(data$Sport1, c("Tennis Men"="Tennis", "Tennis Women"="Tennis","Cross Country Men"="Cross Country","Cross Country Women" = "Cross Country",
                                      "Basketball Men"="Basketball","Soccer Men"="Soccer","Soccer Women"="Soccer","Softball"="Baseball","Track Men"="Track & Field","Track Women"="Track & Field"))



table(data$Sport2)

data$Sport2 <- revalue(data$Sport2, c("Tennis Men"="Tennis", "Tennis Women"="Tennis","Cross Country Men"="Cross Country","Cross Country Women" = "Cross Country",
                                      "Basketball Men"="Basketball","Soccer Men"="Soccer","Soccer Women"="Soccer","Softball"="Baseball","Track Men"="Track & Field","Track Women"="Track & Field"))

table(data$Sport3)

data$Sport3 <- revalue(data$Sport3, c("Tennis Men"="Tennis", "Tennis Women"="Tennis","Cross Country Men"="Cross Country","Cross Country Women" = "Cross Country",
                                      "Basketball Men"="Basketball","Soccer Men"="Soccer","Soccer Women"="Soccer","Softball"="Baseball","Track Men"="Track & Field","Track Women"="Track & Field","Diving"="Swimming"))


#PERMANENT COUNTRY

#revaluing all countries into continents other than the US

table(data$Permanent.Country)
data$Permanent.Country<-revalue(data$Permanent.Country, c("Belgium" = "Europe","Bosnia and Herzegovina" = "Europe", "Germany"="Europe","Iceland"="Europe","Poland"="Europe","Turkey"= "Europe","France" = "Europe","Albania"="Europe","Switzerland"="Europe","Ukraine"="Europe","Portugal"="Europe",
                                                                  "Cameroon"="Africa","Chile"="SA","Columbia"="SA","Costa Rica"="NA","Czech Republic"="Europe", "Ecuador"="SA","Ethiopia"="Africa","Guatemala"="NA","Iran"="Asia","Jamaica"="NA","Kazakhstan"="Asia","Lebanon"="Asia","Malaysia"="Asia",
                                                                  "Montenegro"="Europe","Nepal"="Asia","Nicaragua"="NA","Oman"="Asia","Panama"="NA","Philippines"="Asia","Romania"="Europe","Singapore"="Asia","Spain"="Europe","Tanzania"="Africa","Trinidad and Tobago"="NA","Venezuela"="SA",
                                                                  "Brazil"="SA","Canada"="NA","China"="Asia","Dominica"="Europe","Dominican Republic"="NA","Egypt"="Africa","Ghana"="Africa","Honduras"="NA","India"="Asia","Japan"="Asia","Lithuania"="Europe","Mexico"="NA","Morocco"="Africa","Netherlands"="Europe",
                                                                  "Nigeria"="Africa","Pakistan"="Asia","Paraguay"="SA","Russia"="Europe","South Africa"="Africa","Switzerland"="Europe","Thailand"="Asia","United Arab Emirates"="Asia","Uruguay"="SA","Vietnam"="Asia","Argentina"="SA","Barbados"="NA","Bolivia"="SA",
                                                                  "Cambodia"="Asia","Cayman Islands"="NA","Colombia"="SA","Cyprus"="Europe","El Salvador"="SA","Indonesia"="Asia","Belize"="NA","Greece"="Europe","Kenya"="Africa","Norway"="Europe","The Bahamas"="NA","Hong Kong S.A.R."="Asia","Kuwait"="Asia","Palestine"="Europe",
                                                                  "Uganda"="Africa","Indonesia"="Asia","Luxembourg"="Europe","Peru"="SA","United Kingdom"="Europe","New Zealand"="NA","Cote D'Ivoire"="Africa","Ireland"="Europe","Mongolia"="Asia","Saudi Arabia"="Asia","Bangladesh"="Asia","Italy"="Europe","Mozambique"="Africa","South Korea"="Asia",
                                                          "Uzbekistan"="Asia","Georgia"="Asia","Jordan"="Asia","Taiwan"="Asia","Zimbabwe"="Africa","Uganda"="Africa","Australia"="NA")
                                                                    )

# MERIT AWARD
#extracting Merit Award into just $$ amount awarded
#    > source of function being used https://stla.github.io/stlapblog/posts/Numextract.html

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

data$Merit.Award <- as.numeric(numextract(data$Merit.Award))

# For Merit Award, I realized that all NA's were TT Full Ride Scholarships,
# because they did not have a number at the end, therefore becoming missing
# The highest awards were set to 125, beginning with TT, so setting the NA's to 125

data$Merit.Award[is.na(data$Merit.Award)] <- 125

# PERMANENT GEOMARKET
#fixing permanent geomarket to extract state

data$Permanent.Geomarket<-substr(data$Permanent.Geomarket,start=1,stop=2)
data$Permanent.Geomarket<- as.factor(data$Permanent.Geomarket)

table(data$Permanent.Geomarket)

#combining small levels into "OT"/  represents "other"

data$Permanent.Geomarket <- revalue(data$Permanent.Geomarket, c("AK"="OT","ND"="OT","MT"="OT","DE"="OT","MT"="OT","ND"="OT",
                                                                "PR"="OT","WV"="OT","WY"="OT","VT"="OT","RI"="OT","SD"="OT"))


#RACE
table(data$Race)

#I am recoding anyone who is mixed with 3+ races, or has a race with less than 25 values into a category called "Mixed"

data$Race <- recode(data$Race,"Black or African American, Native Hawaiian or Other Pacific, White"="Mixed",
                   "Asian, Black or African American, White"="Mixed",
                   "Asian, Black or African American, Native Hawaiian or Other Pacific, White"="Mixed",
                   "American Indian or Alaska Native, Asian, Black or African American, White "="Mixed",
                   "American Indian or Alaska Native, Black or African American, White"="Mixed",
                   "American Indian or Alaska Native, Asian, Black or African American, Native Hawaiian or Other Pacific, White"="Mixed",
                  "Black or African American, Native Hawaiian or Other Pacific"="Mixed",
                  "American Indian or Alaska Native, Native Hawaiian or Other Pacific" ="Mixed", 
                  "American Indian or Alaska Native, Asian, Black or African American, White"="Mixed",
                  "American Indian or Alaska Native, Asian"="Mixed",
                  "American Indian or Alaska Native, Black or African American"="Mixed",
                  "American Indian or Alaska Native, Asian, White"="Mixed",
                  "Asian, Native Hawaiian or Other Pacific"="Mixed",
                  "Native Hawaiian or Other Pacific, White"="Mixed",
                  "Asian, Native Hawaiian or Other Pacific, White"="Mixed",
                  "Asian, Black or African American"="Mixed")

#RELIGON

table(data$Religion)

#Combined branches of religion into bigger categories (I researched this)

data$Religion<-revalue(data$Religion, c("Anglican" = "Protestant", "Evangelical"="Protestant",
                                        "Lutheran"="Protestant","Baptist"="Protestant","Methodist"="Protestant",
                                        "Church of Christ"="Protestant","United Methodist"="Protestant","Church of God"="Protestant",
                                        "Assembly of God"="Protestant","Non-Denominational"="Christian","Southern Baptist"="Protestant",
                                        "Lutheran-Missouri Synod"="Protestant","Coptic Church (Egypt)"="Protestant","Independent"="Other",
                                        "Jewish Messianic"="Jewish","United Church of Christ"="Protestant",
                                        "Baha'I"="Other","Christian Reformed"="Religion","Episcopal"="Protestant","Jain"="Buddhism",
                                        "Mennonite"="Religion","Pentecostal"="Protestant","Zoroastrian"="Hindu","Bible Churches"="Protestant",
                                       "Christian Scientist"="Protestant","Jehovah's Witnesses"="Other","Society of Friends (Quaker)"="Other",
                                       "Church of the Nazarene" = "Protestant","Presbyterian Church of America"="Presbyterian","Religion"="Other"))

#LEGACY
#getting rid of all athlete mentions

table(data$Legacy)

data$Legacy <- revalue(data$Legacy, c("Athlete, Fine Arts, Legacy, VIP"="Fine Arts, Legacy, VIP","Athlete, Fine Arts, Legacy"="Fine Arts, Legacy", 
                                      "Athlete, Legacy"="Legacy","Athlete, Legacy, Opt Out, VIP"="Legacy,Opt Out, VIP","Athlete, Legacy, Opt Out"="Legacy, Opt Out",
                                      "Athlete, Legacy, VIP"="Legacy, VIP","Athlete, Fine Arts, Legacy, Opt Out, VIP"= "Fine Arts, Legacy, Opt Out, VIP","Fine Arts, Legacy, Opt Out, VIP"="Fine Arts, Legacy, VIP"))

#ATHLETE
#getting rid of all legacy mentions

table(data$Athlete)

data$Athlete <- revalue(data$Athlete, c("Athlete, Fine Arts, Legacy, VIP"="Athlete","Athlete, Fine Arts, Legacy"="Athlete", 
                                      "Athlete, Legacy"="Athlete","Athlete, Legacy, Opt Out, VIP"="Athlete","Athlete, Legacy, Opt Out"="Athlete",
                                      "Athlete, Legacy, VIP"="Athlete","Athlete, Fine Arts, Legacy, Opt Out, VIP"= "Athlete",
                                      "Athlete, Opt Out, VIP"="Athlete","Athlete, VIP"="Athlete","Athlete, Opt Out"="Athlete",
                                      "Athlete, Fine Arts, Opt Out"="Athlete","Athlete, Fine Arts"="Athlete"))

#INTEREST ONE

#combining languages
#combining management categories
#combining music types
#combining similar interests/majors

  table(data$Interest1)

data$Interest1 <- revalue(data$Interest1, c("Latin"="Language","French"="Language","Chinese"="Language",
                                            "Foreign Languages"="Language","German"="Language","Italian"="Language" ,"Spanish"="Language","Russian"="Language",
                                            "Business - Sport Management"="Management","Choral Music"="Music","Music Education"="Music","Music Composition"="Music","Instrumental Music"="Music",
                                            "Pre-Dental"="Pre-Medical","Pre-Veterinary"="Pre-Medical","Mathematical Finance"="Finance","Linguistics"="Language","Classical Languages"="Language",
                                            "Applied Chemistry"="Chemistry","Biomathematics"="Biochemistry","Biochemistry & Molecular Biology"="Biochemistry","Cognitive Science"="Language","Business Legal Studies"="Pre-Law",
                                            "Art and Art History"="Art","Ancient Mediterranean Studies"="History","Astronomy"="Physics","Human Communication"="Communication","Management"="Business - Management","Comparative Literature"="English",
                                            "Arts, Letters & Enterprise"="English","Agriculture"="Other","African American Studies"="Other","Architectural Studies"="Architecture","Art History"="Art",
                                            "East Asian Studies"="Other","Business - Management Information Systems"="Business Analytics & Technology","Women's & Gender Studies"="Other","Film Studies"="Other",
                                            "Global Latinx Studies"="Other","Pharmacy"="Pre-Medical","Nursing"="Pre-Medical","New Media"="Other","Creative Writing"="English","Architecture"="Other","Religion"="Other"))
#INTEREST TWO

table(data$Interest2)
data$Interest2 <- revalue(data$Interest2, c("Latin"="Language","French"="Language","Chinese"="Language",
                                            "Foreign Languages"="Language","German"="Language","Italian"="Language" ,"Spanish"="Language","Russian"="Language",
                                            "Business - Sport Management"="Management","Choral Music"="Music","Music Education"="Music","Music Composition"="Music","Instrumental Music"="Music",
                                            "Pre-Dental"="Pre-Medical","Pre-Veterinary"="Pre-Medical","Mathematical Finance"="Finance","Linguistics"="Language","Classical Languages"="Language",
                                            "Applied Chemistry"="Chemistry","Biomathematics"="Biochemistry","Biochemistry & Molecular Biology"="Biochemistry","Cognitive Science"="Language","Business Legal Studies"="Pre-Law",
                                            "Art and Art History"="Art","Ancient Mediterranean Studies"="History","Astronomy"="Physics","Human Communication"="Communication","Management"="Business - Management","Comparative Literature"="English",
                                            "Arts, Letters & Enterprise"="English","Agriculture"="Other","African American Studies"="Other","Architectural Studies"="Architecture","Art History"="Art",
                                            "East Asian Studies"="Other","Business - Management Information Systems"="Business Analytics & Technology","Women's & Gender Studies"="Other","Film Studies"="Other", "Geosciences"="Physics",
                                            "Global Latinx Studies"="Other","Pharmacy"="Pre-Medical","Nursing"="Pre-Medical","New Media"="Other","Creative Writing"="English","Architecture"="Other","Religion"="Other",
                                            "Japanese"="Language","German"="Language","Logic and Cognition"="Other","Greek"="Language","Ethics"="Philosophy","Philosophy of Art"="Philosophy","American Intercultural Studies"="Other",
                                            "Medieval & Renaissance Studies"="History","Archaeology"="Geosciences","Scientific Computing"="Computer Science","Architecture"="Urban Studies","Theatre"="Other","Geosciences"="Physics"))

data$Interest2<- revalue(data$Interest2, c("Geosciences"="Physics"))
table(data$Interest2)


#STAFF ASSIGNED NAME 

table(data$StaffAssignedName)

#Christine Ragan has only 1 student, need to combine this with other factor level

data$StaffAssignedName <- revalue(data$StaffAssignedName, c("Christine Ragan"="Michaela Knipp"))


#### TRANSFORMATIONS ####
# references: https://www.projectpro.io/recipes/normalize-and-standardize-data-r

#Standardizing numeric data

#data$School1GPARecalculated<-scale(data$School1GPARecalculated)

#data$Event.Participation<-scale(data$Event.Participation)

#data$Campus.Visits<-scale(data$Campus.Visits)

#data$Merit.Award<-scale(data$Merit.Award)

####SPLITTING TEST TRAIN ####
data$Decision <- as.numeric(data$Decision)
train<- data %>% filter(train.test =="train") %>% select(-ID,-train.test)
test<- data %>% filter(train.test =="test")  %>% select(-ID,-train.test)
trainID<- data %>% filter(train.test =="train") 
testID<- data %>% filter(train.test =="test")


####LINEAR REGRESSION####

simple.ols<- lm(Decision ~ Entry.Term + Permanent.Country + Sex + Ethnicity + Race + Religion + Application.Source + Decision.Plan + StaffAssignedName
                +Legacy +Athlete +Sport1+ Sport2+ Sport3+ Interest1 +Interest2+ Event.Participation + Campus.Visits + Organization.Category + School1GPARecalculated
                +Permanent.Geomarket +Citizenship.Status+ Academic.Index +Merit.Award + Financial.Aid, data = data)

olspred <-predict(simple.ols, newdata=test)

ols.submission<- data %>% 
  dplyr::select(ID,Decision) %>%
  filter(ID>10000) 

olspred<-olspred-1

olspred<- ifelse(olspred>0.5,1,0)


ols.submission$Decision <-olspred

write.csv(ols.submission, "Dulak_1.csv", row.names=FALSE)


#### KNN ####

#need to fill in NA's in test
test$Decision <- olspred

#dependent variable needs to be factor
#independent needs to be numeric

train$Decision <- as.factor(train$Decision)
test$Decision <- as.factor(test$Decision)

train$Event.Participation <- as.numeric(train$Event.Participation)
test$Event.Participation <- as.numeric(test$Event.Participation)
train$Campus.Visits <- as.numeric(train$Campus.Visits)
test$Campus.Visits <- as.numeric(test$Campus.Visits)
train$Merit.Award <- as.numeric(train$Merit.Award)
test$Merit.Award <- as.numeric(test$Merit.Award)

#creating temp data frames with only numeric independent variables

target<- train %>% dplyr::select(Decision)
testcategory <- test %>% dplyr::select(Decision)
temptrain <- train %>% dplyr::select(-Decision)
temptest <- test %>% dplyr::select(-Decision)

temptest$Academic.Index <- as.numeric(temptest$Academic.Index)
temptest$School1GPARecalculated <- as.numeric(temptest$School1GPARecalculated)
temptest<- temptest %>% dplyr::select(School1GPARecalculated, Event.Participation, Campus.Visits, Academic.Index, Merit.Award)

temptrain$Academic.Index <- as.numeric(temptrain$Academic.Index)
temptrain$School1GPARecalculated <- as.numeric(temptrain$School1GPARecalculated)
temptrain<- temptrain %>% dplyr::select(School1GPARecalculated, Event.Participation, Campus.Visits, Academic.Index, Merit.Award)

#checking na's on temp dataframes
sum(is.na(target))
sum(is.na(temptrain))
sum(is.na(temptest))

#making temp a binary
target<- as.numeric(target$Decision)-1


knn <- knn(train = temptrain,
                      test = temptest,
                      cl = target,
                      k = 3)

knn<- as.numeric(knn)-1

#putting results into dataframe for submission


 knn.submission <- data %>% 
  dplyr::select(ID,Decision) %>%
  filter(ID>10000) 

knn.submission$Decision<-knn

write.csv(knn.submission, "Dulak_2.csv")


#### SVM for Classification, linear kernel, cost = 10 ####
svmfit<-svm(Decision ~., 
            data=train, 
            type="C-classification",
            kernel="linear", 
            cost=10)

#predict with linear kernel
predlinear<-predict(svmfit,newdata=test)
predlinear<- as.numeric(predlinear)-1

linear.submission<- data %>% 
  dplyr::select(ID,Decision) %>%
  filter(ID>10000) 

linear.submission$Decision<-predlinear

write.csv(linear.submission, "Dulak_3.csv")

#### SVM for Classification, radial kernel, cost = 10 ####

svmfit2<-svm(Decision ~., 
             data=train, 
             type="C-classification",
             kernel="radial",
             gamma=0.01,
             cost=10)

#predict with radial kernel
predradial<- predict(svmfit2,newdata=test)
predradial<-as.numeric(predradial)
predradial<- predradial-1

radial.submission<- data %>% 
  dplyr::select(ID,Decision) %>%
  filter(ID>10000) 

radial.submission$Decision<-predradial

write.csv(radial.submission, "Dulak_4.csv")


#### SVM for Classification, polynomial kernel, cost = 10 ####

svmfit3<-svm(Decision ~., 
             data=train, 
             type="C-classification",
             kernel="polynomial",
             gamma=0.01,
             cost=10)

#predict with polynomial kernel
predpolynomial<- predict(svmfit3,newdata=test)
predpolynomial<-as.numeric(predpolynomial)
predpolynomial<- predpolynomial-1

poly.submission<- data %>% 
  dplyr::select(ID,Decision) %>%
  filter(ID>10000) 

poly.submission$Decision<-predpolynomial

write.csv(testID, "Dulak_5.csv")


#### RANDOM FORESTS ####

#random forest model 
rf1<-randomForest(Decision ~ .,train, na.action = na.exclude)

# Predict on test
predrf1<-predict(rf1,newdata=test)
predrf1<-as.numeric(predrf1)
predrf1<- predrf1-1

rf1.submission<- data %>% 
  dplyr::select(ID,Decision) %>%
  filter(ID>10000) 

rf1.submission$Decision<-predrf1

write.csv(rf1.submission, "Dulak_6.csv")

#### BAGGING #####

bag1<-randomForest(Decision ~ .,train, mtry=12, na.action = na.exclude)

#prediction on test
predbag1<-predict(bag1,newdata=test, type = "class")
predbag1<-as.numeric(predbag1)
predbag1<- predbag1-1
bag.submission<- data %>% 
  dplyr::select(ID,Decision) %>%
  filter(ID>10000) 

bag.submission$Decision<-predbag1

write.csv(bag.submission, "Dulak_7.csv")

#### BOOSTING ####
train$Decision<- as.numeric(train$Decision)
train$Decision <- train$Decision-1
test$Decision<- as.numeric(test$Decision)
test$Decision <- test$Decision-1

boost1<-gbm(Decision ~.,data=train,
             distribution="bernoulli")


pred_boost<-predict.gbm(boost1,newdata=test,
                        n.trees=100,
                        type="response")

pred_boost<- ifelse(pred_boost>=0.5,1,0)

boost.submission<- data %>% 
  dplyr::select(ID,Decision) %>%
  filter(ID>10000) 

boost.submission$Decision<-pred_boost
write.csv(testID, "Dulak_8.csv")
