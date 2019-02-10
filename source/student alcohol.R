

library(dplyr)

por = read.csv('../Naver/data/student-por.csv')
mat = read.csv('../Naver/data/student-mat.csv')

# por = read.csv('../rbigdata//student-por.csv')
# mat = read.csv('../rbigdata//student-mat.csv')

summary(por)
summary(mat)

### 과목별 성적의 평균이 차이가 나므로 min-max를 이용해 표준화

normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

por[31:33] = as.data.frame(lapply(por[31:33], normalize))
mat[31:33] = as.data.frame(lapply(mat[31:33], normalize))

### por, mat 데이터를 inner join (full join의 경우 intersect가 아닌 difference 부분에서는 NA값이 채워지기 때문)

colum = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob",
          "reason","nursery","internet")
data = inner_join(por, mat, by = colum)


str(data)

# names = colnames(data)
# diff = setdiff(names, colum)
# difi = c('schoolsup.x','famsup.x','paid.x')
# 
# for (i in 1:nrow(data)){
#   for (j in 1:length(diff)/2){
#     data[i,diff[j]] = ifelse(is.na(data[i,diff[j]]), data[i,diff[j+20]],
#                            data[i,diff[j]])
#     }
# }
# data[diff[1:20]] = as.factor(data[diff[1:20]])

colSums(is.na(data))

### 표준화한 과목별 성적으로 평균을 계산하여 grade 변수 생성

grade = round(transmute(data, grade = (G3.x + G3.y)/2),digit=2)


data = data[,1:33]
colnames(data) = colnames(por)
data = data[,1:30]
data$grade = grade$grade

colnames(data)

### Dalc, Walc의 합으로 alco 계산, alco를 이용해 drink 변수 생성(drink를 종속변수로 사용)

data$alco = data$Dalc + data$Walc

summary(data$alco)

# for (i in 1:nrow(data)){
#   if (data$alco[i] == 2){
#     data$drink[i] = '1.Fine'
#   }
#   else if (data$alco[i] == 3){
#     data$drink[i] = '2.Acceptable'
#   }
#   else if (data$alco[i] >= 4 && data$alco[i] < 6){
#     data$drink[i] = '3.Worried'
#   }
#   else data$drink[i] = '4.Serious'
# }

for (i in 1:nrow(data)){
  if (data$alco[i] <= 3){
    data$drink[i] = '1.Fine'
  }
  else data$drink[i] = '2.Worried'
}

data$drink = as.factor(data$drink)

# write.csv(data,'drink.csv')
##### drink에 영향을 주는 변수 삭제(alco, Dalc, Walc)

# t.test(data$alco[data$school == 'GP'],data$alco[data$school == 'MS'])
t.test(data$alco[data$sex == 'F'],data$alco[data$sex == 'M'])
t.test(data$alco[data$address == 'R'],data$alco[data$address == 'U'])
# t.test(data$alco[data$famsize == 'GT3'],data$alco[data$famsize == 'LT3'])
# t.test(data$alco[data$Pstatus == 'A'],data$alco[data$Pstatus == 'T'])
# t.test(data$alco[data$schoolsup == 'yes'],data$alco[data$schoolsup == 'no'])
# t.test(data$alco[data$famsup == 'yes'],data$alco[data$famsup == 'no'])
# t.test(data$alco[data$paid == 'yes'],data$alco[data$paid == 'no'])
# t.test(data$alco[data$activities == 'yes'],data$alco[data$activities == 'no'])
t.test(data$alco[data$nursery == 'yes'],data$alco[data$nursery == 'no'])
# t.test(data$alco[data$higher == 'yes'],data$alco[data$higher == 'no'])
# t.test(data$alco[data$internet == 'yes'],data$alco[data$internet == 'no'])
# t.test(data$alco[data$romantic == 'yes'],data$alco[data$romantic == 'no'])

summary(aov(absences ~ drink, data = data))
summary(aov(grade ~ drink, data = data))


cor(data$alco, data$grade)
cor(data$alco, data$absences)

data = data[-c(27,28,32)]

str(data)
summary(data)


### 시각화

library(ggplot2)
library(gridExtra)




grid.arrange(
  ggplot(data, aes(age,fill = drink)) + geom_bar(position = 'fill'),
  ggplot(data, aes(sex,fill = drink)) + geom_bar(position = 'fill'),
  ggplot(data, aes(school,fill = drink)) + geom_bar(position = 'fill'),
  ggplot(data, aes(address,fill = drink)) + geom_bar(position = 'fill'),
  nrow=2, top = '기본정보 변수(Age, Sex, School,Address)')

grid.arrange(
ggplot(data, aes(internet,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(romantic,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(health,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(goout,fill = drink)) + geom_bar(position = 'fill'),
nrow=2, top = '기본정보 변수(Internet, Romantic, Health, Goout)')


grid.arrange(
ggplot(data, aes(famsize,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(Pstatus,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(Mjob,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(Fjob,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(guardian,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(famrel,fill = drink)) + geom_bar(position = 'fill'),
nrow=3, top = '가족 변수(Famsize, Pstatus, Mjob, Fjob, Guaudian, Famrel)')

grid.arrange(
ggplot(data, aes(Medu,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(Fedu,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(nursery,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(higher,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(traveltime,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(studytime,fill = drink)) + geom_bar(position = 'fill'),
nrow=3,  top = '학습환경 변수(Medu, Fedu, Nursery, Higher, Traveltime, Studytime)')

grid.arrange(
ggplot(data, aes(reason,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(famsup,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(schoolsup,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(paid,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(activities,fill = drink)) + geom_bar(position = 'fill'),
ggplot(data, aes(freetime,fill = drink)) + geom_bar(position = 'fill'),
nrow=3,  top = '학습환경 변수(Reason, Famsup, Schoolsup, Paid, Activities, Freetime)')

grid.arrange(
  ggplot(data, aes(absences)) + geom_density(aes(fill = drink),alpha=.6),
  ggplot(data, aes(failures,fill = drink)) + geom_bar(position = 'fill'),
  ggplot(data, aes(grade)) + geom_density(aes(fill = drink),alpha=.6),
  nrow=2 ,top = '학업성취 변수(Absences, Failures, Grade)')

grid.arrange(
  ggplot(data, aes(address,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(internet,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(romantic,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(health,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(goout,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(reason,fill = drink)) + geom_bar(position = 'dodge'),nrow=3)

grid.arrange(
  ggplot(data, aes(famsize,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(Pstatus,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(Mjob,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(Fjob,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(guardian,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(famrel,fill = drink)) + geom_bar(position = 'dodge'),nrow=3)

grid.arrange(
  ggplot(data, aes(Medu,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(Fedu,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(nursery,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(higher,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(traveltime,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(studytime,fill = drink)) + geom_bar(position = 'dodge'),nrow=3)

grid.arrange(
  ggplot(data, aes(failures,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(famsup,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(schoolsup,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(paid,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(activities,fill = drink)) + geom_bar(position = 'dodge'),
  ggplot(data, aes(freetime,fill = drink)) + geom_bar(position = 'dodge'),nrow=3)


library(gmodels)
CrossTable(data$Mjob, data$Medu, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$Fjob, data$Fedu, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$address, data$traveltime, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$pstatus, data$famrel, prop.r = F, prop.chisq = F, prop.t = F)

CrossTable(data$reason, data$studytime, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$schoolsup, data$studytime, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$famsup, data$studytime, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$paid, data$studytime, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$activities, data$studytime, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$higher, data$studytime, prop.r = F, prop.chisq = F, prop.t = F)

CrossTable(data$reason, data$absences, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$schoolsup, data$absences, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$famsup, data$absences, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$paid, data$absences, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$activities, data$absences, prop.r = F, prop.chisq = F, prop.t = F)
CrossTable(data$higher, data$absences, prop.r = F, prop.chisq = F, prop.t = F)



### sampling :upsample (종속변수의 비율 차이때문에 편향을 줄이기 위해 upsampling)

library(caret)
xdata = upSample(subset(data.train, select=-drink), data$drink)
xdata = xdata[-30]
table(xdata$Class)
str(xdata)

library(party)
parts = createDataPartition(data$drink, p=.7)
data.train = data[parts$Resample1,]
data.test = data[-parts$Resample1, ]

data.up.train <- upSample(subset(data.train, select=-drink),data.train$drink)

# write.csv(xdata,'../Naver/drink.csv')

### decision tree

library(C50)
model = C5.0(drink ~., data=data.train)

summary(model)
pred = predict(model, data.test)
confusionMatrix(pred, data.test$drink)

library(C50)
model = C5.0(drink ~ sex+famsize+age+traveltime+health+goout+Fjob+Mjob+famrel+reason+studytime+
               freetime+Fedu+Medu+absences+grade, data=data.train)
summary(model)
pred = predict(model, data.test)
confusionMatrix(pred, data.test$drink)

library(caret)

model = train(drink ~ sex+famsize+age+traveltime+health+goout+Fjob+Mjob+famrel+reason+studytime+
            freetime+Fedu+Medu+absences+grade, data = data.train, method = "C5.0")
model

ctrl = trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
grid = expand.grid(.model = "rules", .winnow = "FALSE", .trials = 20)
model = train(drink ~ sex+famsize+age+traveltime+health+goout+Fjob+Mjob+famrel+reason+studytime+
              freetime+Fedu+Medu+absences+grade, data = data.train,
              method = "C5.0", trControl = ctrl, tuneGrid = grid)
summary(model)

plot(model$finalModel)
varimportance = varImp(model, scale = F)
plot(varimportance)
pred = predict(model, data.test)
confusionMatrix(pred, data.test$drink)

Sys.setenv(JAVA_HOME = "A:/Java/jdk1.8.0_181/jre/")
library(RWeka)
m = train(drink ~ sex+famsize+schoolsup+higher+health+goout+Fjob+famrel+reason+studytime+
            freetime+failures+absences+grade, data = data, method = "JRip")
m

summary(m$finalModel)

### Logistic regression

library(LogicReg)

model = train(drink ~ sex+famsize+age+traveltime+health+goout+Fjob+Mjob+famrel+reason+studytime+
            freetime+Fedu+Medu+absences+grade, data = data.train ,method="glm",family=binomial())
model
pred = predict(model, data.test)
confusionMatrix(pred, data.test$drink)
varimportance = varImp(model)
plot(varimportance)
### random forest

library(randomForest)
model = randomForest(drink ~., data=data.train)
model = randomForest(drink ~ sex+famsize+schoolsup+higher+health+goout+Fjob+famrel+reason+studytime+
                       freetime+failures+absences+grade, data=data.train)

varImpPlot(model, type = 2)
pred = predict(model, data.test)
confusionMatrix(pred, data.test$drink)
model = train(drink ~ sex+famsize+age+traveltime+health+goout+Fjob+Mjob+famrel+reason+studytime+
                freetime+Fedu+Medu+absences+grade,data = data.train, method = "rf")

model
pred = predict(model, data.test)
confusionMatrix(pred, data.test$drink)
varImp(model)


### Logistic regression

Null.model <- glm(drink ~ 1, data = data, family = binomial)
glm.model <- glm(drink ~ sex+famsize+age+traveltime+health+goout+Fjob+Mjob+famrel+reason+studytime+
                   freetime+Fedu+Medu+absences+grade,
                 data = data.train, family = binomial)
both.glm <- step(Null.model,
                 scope = list(lower = Null.model, upper = glm.model),
                 direction = 'both')

glm.pred <- predict(glm.model, data.test, type = 'response')

glm.pred1 <- ifelse(glm.pred>0.5,2,1)
glm.pred1 <- factor(glm.pred1, levels=c(1,2), labels = c('1.Fine','2.Worried'))

confusionMatrix(glm.pred1, data.test$drink)


both.glm <- glm(Class ~ studytime + goout + age + activities + sex + famrel + 
                  Pstatus + health + internet + address + higher + absences + 
                  Fjob, data = xdata, family = binomial)
summary(both.glm)
both.glm


glm.pred <- predict(both.glm, xdata, type = 'response')

glm.pred1 <- ifelse(glm.pred>0.5,1,0)
glm.pred1 <- factor(glm.pred1, levels=c(0,1), labels = c('no','yes'))

confusionMatrix(glm.pred1, xdata$Class, positive = 'yes')


### SVM

library(kernlab)
model = ksvm(drink ~ sex+address+nursery+failures+freetime+traveltime+studytime+age+goout+absences+grade,
             data = data.train, kernel = 'rbfdot')
pred = predict(model, data.test)
confusionMatrix(pred, data.test$drink)


model = ksvm(drink ~ sex+address+nursery+failures+freetime+traveltime+studytime+age+goout+absences+grade,
             data = data.train, kernel = 'vanilladot')
pred = predict(model, data.test)
confusionMatrix(pred, data.test$drink)

m = train(drink ~ sex+address+nursery+failures+freetime+traveltime+studytime+age+goout+absences+grade,
          data = data, method = "svmLinear")
m
m$finalModel

m = train(Class ~ ., data = xdata, method = "svmRadial")
m

### decision rule (에러때문에 확인 못해봄)

library(RWeka)
m = train(Class ~ ., data = xdata, method = "JRip")
m

m = train(drink ~ ., data = data[-c(27,28,32)], method = "nnet")
m
