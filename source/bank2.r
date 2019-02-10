raw_data <- read.csv("bank-additional.csv",sep = ';' ,header = T)
data <- raw_data
table(raw_data$age, raw_data$y)
raw_data$previous
str(raw_data)
summary(data)
colSums(is.na(raw_data))
table(raw_data$y)

# job 변수 전처리 unknown 을 최빈값으로
job.level <- levels(data$job)[-12]
data$job <- as.numeric(data$job) 
table(data$job)
data$job <- ifelse(data$job == 12, 1, data$job)
data$job <- factor(data$job, levels = 1:11, labels = job.level)

# martial 변수 전처리 : unknown을 최빈값으로
data$marital <- as.numeric(data$marital)
table(data$marital)
data$marital <- ifelse(data$marital == 4, 2, data$marital)
data$marital <- factor(data$marital, levels = c(1,2,3), labels = c('divorced', 'married', 'single'))

summary(data)
table(data$month)
table(data[data$month=='oct',][21])
data[data$month=='mar',][21]

# education 변수 전처리 : unknown을 최빈값으로
edu.level <- levels(data$education)[-8]
data$education <- as.numeric(data$education)
table(data$education)
data$education <- ifelse(data$education == 8, 7, data$education)
data$education <- factor(data$education, levels = 1:7, labels = edu.level)

edu.level <- levels(data$education)[-5]
data$education <- as.numeric(data$education)
table(data$education)
data$education <- ifelse(data$education == 5, 7, data$education)
data$education <- factor(data$education, levels = c(1,2,3,4,6,7), labels = edu.level)

# 필요없는 변수 제거
colnames(data)
data <- data[,-c(5,11)]
summary(data)

# housing 변수 전처리 unknown을 최빈값으로
data$housing <- as.numeric(data$housing)
table(data$housing)
data$housing <- ifelse(data$housing == 2, 3 ,data$housing)
data$housing <- factor(data$housing, levels = c(1,3), labels = c('no', 'yes'))

# loan 변수 전처리 unknown을 최빈값으로
data$loan <- as.numeric(data$loan)
table(data$loan)
data$loan <- ifelse(data$loan == 2, 1, data$loan)
data$loan <- factor(data$loan, levels = c(1,3), labels = c('no', 'yes'))

# pdays 변수 전처리. 21보다 큰경우와 아닌경우로 나눈다
data$pdays <- ifelse(data$pdays>21, 0, 1)
data$pdays <- as.factor(data$pdays)

# previous 변수 factor 화
table(data$previous, data$y)
data$previous <- factor(data$previous)
data$previous <- as.numeric(data$previous)


# numeric 변수 최대최소 정규화
#nomalize <- function(x){
#  x <- (x-min(x))/(max(x)-min(x))
#}
#data$age <- nomalize(data$age)
#data$emp.var.rate <- nomalize(data$emp.var.rate)
#data$cons.price.idx <- nomalize(data$cons.price.idx)
#data$cons.conf.idx <- nomalize(data$cons.conf.idx)
#data$euribor3m <- nomalize(data$euribor3m)
#data$nr.employed <- nomalize(data$nr.employed)

# default 변수 전처리
#data$default <- as.numeric(data$default)
#data$default <- ifelse(data$default == 3, 2, data$default)
#data$default <- factor(data$default, levels = c(1,2), labels = c('no', 'unknown'))
#table(data$default)

# campaign 변수 범주화 
table(data$campaign, data$y)
data$campaign <- ifelse(data$campaign > 11, 3, ifelse(data$campaign >4, 2, 1))
data$campaign <- factor(data$campaign, levels = c(1,2,3), labels = c('low', 'medium', 'high'))


# 데이터분할
#smp.yes <- sample(which(data$y == 'yes'), 315, replace = F)
#smp.no <- sample(which(data$y == 'no'), 2568, replace = F)

#train <- data[smp,]
#test <- data[-smp,]

# 동일개수

set.seed(122)
smp.yes <- which(bank$y == 'yes')
smp.yes1 <- sample(4640, 3248, replace = F)
smp.yes2 <- smp.yes[smp.yes1]
smp.yes3 <- smp.yes[-smp.yes1]
smp.no <- sample(which(bank$y == 'no'), 4640, replace = F)
smp.no1 <- sample(4640, 3248, replace = F)
smp.no2 <- smp.no[smp.no1]
smp.no3 <- smp.no[-smp.no1]

smp.train <- c(smp.yes2, smp.no2)
smp.test <- c(smp.yes3, smp.no3)
train <- bank[smp.train,]
test <- bank[smp.test,]

library(C50)

c50.model = C5.0(y ~ pcontact + nr.employed + poutcome + month + emp.var.rate +
                   contact, data = train, trials = 10)
summary(c50.model)
plot(c50.model)
c50.pred = predict(c50.model, test)

confusionMatrix(c50.pred, test$y, positive = "yes")

c50.model.selection = C5.0(y ~ nr.employed + pdays + month + contact + cons.conf.idx + 
                   housing + campaign, data = train)
summary(c50.model.selection)
plot(c50.model.selection)
c50.pred.selection = predict(c50.model.selection, test)

confusionMatrix(c50.pred.selection, test$y, positive = "yes")


# glm full model
Null.model <- glm(y ~ 1, data = train, family = binomial)
glm.model <- glm(y ~ ., data = train, family = binomial)
glm.pred <- predict(glm.model, test, type = 'response')

glm.pred1 <- ifelse(glm.pred>0.5,1,0)
glm.pred1 <- factor(glm.pred1, levels=c(0,1), labels = c('no','yes'))
confusionMatrix(glm.pred1, test$y, positive = 'yes')

# glm variable select

both.glm <- step(Null.model, scope = list(lower = Null.model, upper = glm.model), direction = 'both')
both.glm <- glm(y ~ pdays + month + campaign + emp.var.rate + cons.price.idx + 
                  contact + euribor3m + loan
                , data = train, family = binomial)
glm.pred <- predict(both.glm, test, type = 'response')

glm.pred1 <- ifelse(glm.pred>0.5,1,0)
glm.pred1 <- factor(glm.pred1, levels=c(0,1), labels = c('no','yes'))
confusionMatrix(glm.pred1, test$y, positive = 'yes')
