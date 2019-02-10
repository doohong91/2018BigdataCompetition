
data = read.csv('../Naver/data/fatal-police-shootings-data11.csv', header = T, encoding = 'utf-8')

str(data)

data$armed[data$armed == ''] = 'undetermined'
data$flee[data$flee == ''] = 'Other'
data$gender[data$gender == ''] = NA
data$race[data$race == ''] = NA
colSums(is.na(data))

data$armed = factor(data$armed, labels = c('gun','not weapon','toy','unarmed','undetermined',
                                           'vehicle','weapon')) 
data$flee = factor(data$flee, labels = c('Car','Foot','Not fleeing','Other'))
data$gender = factor(data$gender, labels = c('F','M'))
data$race = factor(data$race, labels = c('A','B','H','N','O','W'))

data = na.omit(data)

date = as.POSIXlt(as.Date(data$date))
data$year = as.factor(date$year + 1900)
data$month = as.factor(date$mon + 1)
data$dayofweek = as.factor(weekdays(date))
data$age = cut(data$age, c(0,20,30,40,50,60,70,100), labels = c('under 20','20`s','30`s','40`s','50`s','60`s','over 70'))

data2 = data[c(2,3,6,9,11,12,13)]
data[-c(4,5,8,10)]

result = kmodes(data2,15)
summary(result)
result

library(C50)
model = C5.0(data = data[,-5], armed ~.)
model
summary(model)
