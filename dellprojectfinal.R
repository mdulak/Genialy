
install.packages("tidyverse")
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(car)
delldata<-read_excel("C:/Users/Morgan's Laptop/Desktop/Trinity/Fall 20/Analytics/Student Data for Distribution.xlsx")
costdata<-read_excel("C:/Users/Morgan's Laptop/Desktop/Trinity/Fall 20/Analytics/Student Data for Distribution.xlsx", sheet = "Cost Details")

#EXPLORATION
ggplot(delldata,aes(x=`Origin`))+geom_bar(aes(fill=`LOB`))
'We found that product A and C are only shipped from site A'

ggplot(delldata,aes(x=`Ship Date`))+geom_bar(aes(fill=`Ship Qtr`))
'Exploring the dates of each ship quarter'
ggplot(delldata,aes(x=`Ship Date`))+geom_density(aes(fill=`Ship Qtr`))
ggplot(delldata,aes(x=`Receipt Date`))+geom_density(aes(fill=`Ship Qtr`))
'We chose to remove NAs (which were primarily in the Ship Date column) to keep the data as accurate as possible. 
We want to keep the integrity of our data and creating replacement dates did not make sense, as the lead time is the explanatory variable'
delldata_nona<-na.omit(delldata)
'When exploring data to clean, we identified bad dates, which are entries that have receipt date before ship date, or download date after receipt date.'
goodDates <- delldata_nona %>%
  filter(delldata_nona$`Ship Date` > delldata_nona$`PO Download Date` & delldata_nona$`Receipt Date` > delldata_nona$`PO Download Date` & delldata_nona$`Ship Date`< delldata_nona$`Receipt Date`) 

badDates <- delldata_nona %>%
  filter(!(delldata_nona$`Ship Date` > delldata_nona$`PO Download Date` & delldata_nona$`Receipt Date` > delldata_nona$`PO Download Date` & delldata_nona$`Ship Date`< delldata_nona$`Receipt Date`)) 
'We identified the amount of illogical dates, and decided to clean them with dplyr'

#CLEANING DATA

fixedDates <- badDates %>%
  mutate(`Ship Date` = as.Date(`Ship Date`)) %>% #converted variables into same date formats 
  mutate(`Ship Date` = ymd(`Ship Date`)) %>%
  mutate(`Receipt Date` = as.Date(`Receipt Date`)) %>%
  mutate(`Receipt Date` = ymd(`Receipt Date`)) %>%
  mutate(`PO Download Date` = as.Date(`PO Download Date`)) %>%
  mutate(`PO Download Date` = ymd(`PO Download Date`)) %>%
  mutate(small1 = if_else(`Ship Date` > `Receipt Date`, `Receipt Date`, `Ship Date`)) %>% #testing dates to be logical
  mutate(small2 = if_else(`Ship Date` > `PO Download Date`, `PO Download Date`, `Ship Date`)) %>% #bad dates are switched so that they are in chronological order 
  mutate(smallestDate = if_else(small1 > small2, small2, small1)) %>%
  mutate(big1 = if_else(`Ship Date` < `Receipt Date`, `Receipt Date`, `Ship Date`)) %>%
  mutate(big2 = if_else(`Ship Date` < `PO Download Date`, `PO Download Date`, `Ship Date`)) %>%
  mutate(biggestDate = if_else(big1 < big2, big2, big1)) %>%
  mutate(middle1 = if_else(small1 <= small2, small2, small1)) %>%
  mutate(middle2 = if_else(big1 <= big2, big1, big2)) %>%
  select(-small1, -small2) %>%
  select(-big1, -big2) %>%
  mutate(middleDate = if_else(smallestDate == middle1, middle2, middle1)) %>%
  select(-middle1, -middle2) %>%
  mutate(`PO Download Date` = smallestDate) %>%
  mutate(`Ship Date` = middleDate) %>%
  mutate(`Receipt Date` = biggestDate) %>%
  select(-smallestDate, -middleDate, -biggestDate)

clean <- rbind(goodDates, fixedDates) #appended fixed dates to the already chronological dates 


'Once we cleaned the data to be in chronological order and changed date variables into date data types,
we turned our categorical variables into factors and calculated a new numerical variable for lead time'

clean<-clean %>%
  mutate(LT=(as.Date(`Receipt Date`)-as.Date(`Ship Date`)))%>%
  mutate(`LT`=as.numeric(`LT`))%>%
  mutate(LOB=as.factor(`LOB`))%>%
  mutate(`Ship Mode`=as.factor(`Ship Mode`))%>%
  mutate(`Ship Qtr`=as.factor(`Ship Qtr`))%>%
  mutate(`Origin`=as.factor(`Origin`))

#DESCRIPTIVE STATISTICS 
avgfastboat<-clean %>% 
  group_by(`LOB`)%>%
  filter(`Ship Mode`=="FASTBOAT")%>%
  summarise(avg=mean(`LT`))

avgocean<-clean %>% 
  group_by(`LOB`)%>%
  filter(`Ship Mode`=="OCEAN")%>%
  summarise(avg=mean(`LT`))

avgground<-clean %>% 
  group_by(`Origin`)%>%
  filter(`Ship Mode`=="GROUND")%>%
  summarise(avg=mean(`LT`))

avgair<-clean %>% 
  group_by(`Origin`)%>%
  filter(`Ship Mode`=="AIR")%>%
  summarise(avg=mean(`LT`))

fastboatProbDF <- clean %>%
  filter(`Ship Mode` == "FASTBOAT" & `LOB` == "Product A") %>%
  select(`LT`)


statsLOB<- clean %>%
  group_by(`LOB`) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

statsOrigin<- clean %>%
  group_by(`Origin`) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

statsMode<- clean %>%
  group_by(`Ship Mode`) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

statsQtr<- clean %>%
  group_by(`Ship Qtr`) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

#Create Model
'Checking for correlation between variables'
correlation_test_model<-lm(`LT`~`Ship Mode`+`LOB`*`Ship Qtr`,data=clean)
correlation_test<-summary(correlation_test_model,correlation = TRUE)
print(correlation_test)
vif(correlation_test_model)
'We needed to create a combined variable to minimize collinearity'
clean$combined<-paste(clean$LOB,clean$Origin,clean$`Ship Mode`,sep = " ")
full_model <-lm(`LT`~`combined`+`Ship Qtr`,data=clean)
summary(full_model)


#Meeting the 5 Assumptions
'1. Check for normal distribution'
ggplot(clean, aes(`LT`)) + geom_density()
'Not normally distributed'

'2. Check for linear relationship between independent variable and dependent variable'
ggplot(clean, aes(x=`LT`, y=combined))+ geom_point()
'All of our independent variables are categorical data so it is impossible to verify a linear relationship'

'3.Check for abnormalities in the residuals'
plot(lm(`LT`~combined+`Ship Qtr`, data=clean))
'Residuals on Q-Q plot are extremely abnormal and are not normally distributed around 0'

'4. Check possibility of multicollinearity'
vif(full_model)
'When we created the combined variable in our model, multicollinearity was addressed'

'5.Check for heteroskedasticity'
plot(lm(`LT`~combined+`Ship Qtr`, data=clean))
'The residuals vs fitted line is not bowed and the variance in residuals seems acceptable
'
####Splitting Model

set.seed(500)
size = floor(0.5 * nrow(clean))
rownums = seq(1:nrow(clean))
trainIndex = sample(rownums, size) 
train = clean[trainIndex,]
test = clean[-trainIndex,]

###Training Model

simple_model <-lm(`LT`~`combined`+`Ship Qtr`,data=train)
summary(simple_model)
vif(simple_model)

train$pred <- simple_model$fitted.values  
train$residual <- simple_model$residuals
summary(simple_model)
train_R2 <- 1 - (sum((train$residual)^2)/
                   sum((train$`LT` - mean(train$`LT`))^2))
train_RMSE<- sqrt(mean((train$residual)^2))
###Testing Model
test$predicted <- predict(simple_model, newdata = test) 
test$residual <- test$`LT` - test$predicted

test_R2 <- 1-(sum((test$residual)^2)/sum((test$`LT`-mean(test$`LT`))^2))
test_RMSE<- sqrt(mean((test$`LT`)^2))
output <- data.frame(train_R2, 
                     train_RMSE,
                     test_R2, 
                     test_RMSE)

View(output)
'R^2 Test and R^2 Train are 0.02 difference which validates our model
RMSE Test and RMSE Train is 8.3 apart which is acceptable to validate our model
Intercept is Product A , QTR1 , Site A, Ship Mode Air'

#Analytics Q1
# Use the dataset to come up with a multi-variable linear regression equation for lead
# time. What is the adjusted R-square for the model? Are all variables equally important?

summary(full_model)

#ANSWER:
'Multiple R-squared:  0.3262	Adjusted R-squared:  0.3253
#Ship Quarter is not as significant as a variable as Product, Site, and Ship Model'

#Analytics Q2
#2. Check for multicollinearity in the independent variables of the model and list down the
#degree of correlation between these predictor variables. Create regression models with
#Multicollinearity and by eliminating the multicollinearity to determine and present the

#ANSWER:
'Using a bad model without combined variable gives us multicollinearity, and vifs over 10.
Our good model does not have multicollinearity, all vifs are under 1.5'

vif(full_model)
bad_model<-lm(`LT`~`Ship Mode`+`LOB`*`Ship Qtr`,data=clean)
vif(bad_model)
#What level of impact does it have on the coefficients, p-values, predictions and accuracy of
#predictions? 
summary(bad_model)
summary(full_model)

#ANSWER:
'The good model has a higher R^2 , of 0.325 as compared to the bad model with an R^2 of 0.2928
Quarter as a variable becomes less significant in the good model where multicollinearity is not present 
In our good model, Changing the site or product does not significantly impact lead time as long as the ship mode is air'

#Analytics Q3. Has the logistics lead time reduced since Q2, when compared with Q4 and Q1 for AIR
#and GROUND ship mode?
  q3<-clean %>%
  filter(`Ship Mode`=="GROUND" |`Ship Mode`=="AIR" )
#Checking Quarters
ggplot(q3,aes(x=`Receipt Date`,y=`LT`,color=`Ship Qtr`))+geom_point()+geom_smooth()
ggplot(q3,aes(x=`Ship Date`,y=`LT`,color=`Ship Qtr`))+geom_point()

feb1 <- ymd("2020-02-01")
april31 <- ymd("2020-04-31")
may1 <- ymd("2020-05-01")
july31 <- ymd("2020-07-31") 


 q3$`Ship Qtr`<-as.character(q3$`Ship Qtr`)


ggplot(q3,aes(x=`Ship Mode`, y = `LT`)) + 
  facet_wrap(~`Ship Qtr`) +
  geom_bar(stat='identity',aes(fill = factor(`Ship Mode`)))

'The logistic lead time increased for both Air and Group shipping methods during Q2 as compared to Q1 and Q4'

# 4.Analytics Q4 How many in-transit days can be saved if we bring inventory by AIR from Site A
#compared to Site C?

#Air Site A vs Air Site C 
avgair<-clean %>% 
  group_by(`Origin`)%>%
  filter(`Ship Mode`=="AIR")%>%
  summarise(avg=mean(`LT`))

q4 <- avgair[avgair$Origin == "Site C","avg"] - avgair[avgair$Origin == "Site A","avg"]
'By changing site A to ship mode Air as opposed to site C, we would save an average of 0.6292 days in-transit.'

#Optimization
'Assuming we have a customer deal 100K units of product A,
for which Purchase Orders(PO) are set to be provided to site A between 21st Sept and 25th Sept at a daily rate of 20K units (20K*5days = 100K) 
and we must meet the deadline of 27th Oct to receive all units at the destination facility/warehouse in the US in order to meet customer due date.
We have only $1.5M allocated on freight budget to realize a positive margin on customer sales (The higher the logistics cost, it lowers the profit margin).
The product is sold at $333 to the customer with 7% margin.
Considering all the above factors, the supply chain analytics team needs to optimize and evaluate the following:'
 
  '1.Can we fulfil all 100K units before the deadline with the available budget based on the current manufacturing and logistics lead times? '
#DESCRIPTIVE STATISTICS 
avgfastboat<-clean %>% 
  group_by(`LOB`)%>%
  filter(`Ship Mode`=="FASTBOAT")%>%
  summarise(avg=mean(`LT`))

avgocean<-clean %>% 
  group_by(`LOB`)%>%
  filter(`Ship Mode`=="OCEAN")%>%
  summarise(avg=mean(`LT`))

avgground<-clean %>% 
  group_by(`Origin`)%>%
  filter(`Ship Mode`=="GROUND")%>%
  summarise(avg=mean(`LT`))
 
fastboatProbDF <- clean %>%
  filter(`Ship Mode` == "FASTBOAT" & `LOB` == "Product A") %>%
  select(`LT`)

fastboatProbVect <- as.vector(fastboatProbDF['LT'])
success_vector_fastboat<-fastboatProbVect[fastboatProbVect<32]  # we make a success vector to find x (successes)
prop.test(x=247,n=305,p=0.95)
# 95 percent confidence interval- 0.7602859 - 0.8513945

sep25<-ymd("2020-09-25")
oct27<-ymd("2020-10-27")
sep21<-ymd("2020-09-21")

difftime(oct27,sep25) #Time difference of 32 days

13*100000 < 1500000 # Using fastboat is under budget 

'On average, fastboat arrives in 28.46885 days.  
Additionally, shipping al products via fastboast will only cost 1.3 M which is within budget.
After performing a probability analysis we found that the likelihood of the products arriving on time via fastboat is only 76.0%-85.1%
Shipping all by air is too expensive, but shipping all by ocean is too slow. 
Shipping by ground is not possible because site A and C are on different continents.'


 ' 2.If not, please come up with an optimized volume of product A that needs to be lifted by air/Ocean/Fast Boat 
  to meet maximum customer demand depending on the ship mode lead times and budget available? '


difftime(oct27,sep21) #Time difference of 36 days
oceanProbDF <- clean %>%
  filter(`Ship Mode` == "OCEAN" & `LOB` == "Product A") %>%
  select(`LT`)
oceanProbVect <- as.vector(oceanProbDF['LT'])
                           
success_vector_ocean <- oceanProbVect[oceanProbVect < 36] #vector to find x (number of successes)
prop.test(x=326,n=511,p=.95)  #Prop test to see if Ocean products can arrive on time from Sep 21
#95 percent confidence interval: 0.5944143-0.6794082
'We can optimize costs by shipping the first 20k of product A by ocean to save $60k on shipping costs. 
The average Ship time for product A via Ocean is 35.2 days, however we cannot be confident that all products shipped by 
Ocean will arrive on time. 
After performing a probability analysis we found that the likelihood of those products arriving on time is only 59.4%-67.9%'


' 3.Create a proposal to request an additional budget to fulfil all 100K units by the deadline?  '

airProbDF <- clean %>%
  filter(`Ship Mode` == "AIR" & `LOB` == "Product A") %>%
  select(`LT`)
airProbVect <- as.vector(airProbDF['LT'])
success_vector_air<-airProbVect[airProbVect<32]
prop.test(x=2067,n=2094,p=0.95)

'After performing a probability analysis we found that the likelihood of the products arriving on time via air is 98.1%-99.1%
Therefore, to ensure that all products from each shipment arrive on time with high confidence, we will need to increase our budget 
by 700,000'

' 4. Provide an analysis to the leadership team on total logistics cost vs total revenue vs profit margin
with the initial budget provided and proposed budget for them to decide what trade-offs to make?'
  
fastboatProbDF <- clean %>%
  filter(`Ship Mode` == "FASTBOAT" & `LOB` == "Product A") %>%
  select(`LT`)
fastboatProbVect <- as.vector(fastboatProbDF['LT'])
success_vector_fastboat<-fastboatProbVect[fastboatProbVect<36] #created a vector to find number of successes
prop.test(x=260,n=305,p=0.95)

#95 percent confidence interval:
 #0.8064839 0.8893095

(13*40000)+(22*60000) #proposed shipping cost

'Shipping everything by air would almost guarantee that everything would arrive on time,
however this would cut deeply into our profits given that this would push us well over budget.
Therefore, if we want to maintain our budget and profit margin, we need to accept a lower probability of everything arriving on time.
There is an 80.4-88.9% chance of the first two days of shipments by fastboat arrive on time. 
We believe 80% is a tolerable probability.
In conclusion we believe that the first 40k units shipped on September 21 and 22 should be sent by fastboast and the remaining units should be shipped by air.
This will cost us in total $1,840,000 which is over budget by $340,000. 


