getwd()
library(caret)
library(dplyr)
library(randomForest)

raw_data <- read.csv("bank/bank-additional.csv",sep = ';' ,header = T)
str(raw_data)
summary(raw_data)

colSums(is.na(bank))
table(bank$y)


bank.client = raw_data[1:7]
bank.contact = raw_data[8:11]
bank.other = raw_data[12:15]
bank.context = raw_data[16:20]
bank.output = raw_data[21]

# delete 'default'

bank.client = bank.client[-5]


# create 'pcontact' by 'pdays'
table(bank.other$pdays)
bank.other$pcontact <- as.factor(ifelse(bank.other$pdays == 999, 0, 1))
levels(bank.other$pcontact) = c("no", "yes")
table(bank.other$pcontact)

# create 'newcustomer' 

bank.other$newcustomer <- as.factor(ifelse(bank.other$poutcome == "nonexistent" & 
                                             bank.other$pdays == 999, 1, 0))
levels(bank.other$newcustomer) = c("no", "yes")
table(bank.other$newcustomer)

bank.other$pdays = as.factor(bank.other$pdays)


bank.other$pdays <- ifelse(bank.other$pdays == 999, 0, bank.other$pdays)
table(bank.other$pdays)


# filter(bank.other, pcontact == "no") %>% select(poutcome, previous) %>% table
# filter(bank.other, pcontact == "yes") %>% select(poutcome, previous) %>% table
# 
# filter(bank.other, pcontact == "no") %>% select(pdays, previous) %>% table
# filter(bank.other, pcontact == "yes") %>% select(pdays, previous) %>% table
# 
# filter(bank.other, pcontact == "no") %>% select(poutcome, campaign) %>% table
# filter(bank.other, pcontact == "yes") %>% select(poutcome, campaign) %>% table


# delete 'duration'

bank.contact = bank.contact[-4]

# # PCA bank.context
# 
# bank.context.pca = princomp(bank.context, cor = T)
# summary(bank.context.pca, loadings = T)
# a1 = bank.context.pca$scores
# 
# # normalize numeric attributes 
# 
# bank.context = as.data.frame(lapply(bank.context, scale))
# bank.other = cbind(as.data.frame(lapply(bank.other[1:3], scale)),bank.other[4:5])

# making dataset

bank = cbind(bank.client, bank.contact, bank.other, bank.context, bank.output)


set.seed(122)
bank.yes = bank[bank$y == "yes", ]
bank.no = bank[bank$y == "no", ]
sample.yes = sample(nrow(bank.yes), nrow(bank.yes)*0.7)
sample.no = sample(nrow(bank.no), nrow(bank.no)*0.7)
train = rbind(bank.yes[sample.yes,],bank.no[sample.no,])
test  = rbind(bank.yes[-sample.yes,],bank.no[-sample.no,])


### decision tree (C5.0)

library(C50)
matrix.dimentions = list(c("no","yes"),c("no","yes"))
names(matrix.dimentions) = c("predicted", "actual")
error.cost = matrix(c(0,1,8.133038,0), nrow = 2, dimnames = matrix.dimentions)

c50.model = C5.0(y ~ ., data = train, trials = 10)
summary(c50.model)
plot(c50.model)
c50.pred = predict(c50.model,test)

confusionMatrix(c50.pred, test$y, positive = "yes")


m = train(y ~ ., data = bank, method = "C5.0")

bank.c50.model = C5.0(y ~ month + campaign + poutcome + euribor3m + nr.employed + job +
                        age + contact + emp.var.rate + pdays, data = train)

summary(bank.c50.model)
plot(bank.c50.model)
bank.c50.pred = predict(bank.c50.model, test)

confusionMatrix(bank.c50.pred, test$y, positive = "yes")

m = train(y ~  month + campaign + poutcome + euribor3m + nr.employed + job +
            age + contact + emp.var.rate + pdays, data = train, method = "C5.0")

ctrl = trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
grid = expand.grid(.model = "tree", .winnow = "TRUE", .trials = 20)
set.seed(122)
m2 = train(y ~ ., data = train, method = "C5.0", trControl = ctrl, tuneGrid = grid)


c50.model.selection = C5.0(y ~ nr.employed + month + poutcome + contact + cons.conf.idx + 
                             campaign + age + previous, data = train)
summary(c50.model.selection)
plot(c50.model.selection)
c50.pred.selection = predict(c50.model.selection, test)

confusionMatrix(c50.pred.selection, test$y, positive = "yes")

m = train(y ~ nr.employed + month + poutcome + contact + cons.conf.idx + 
            campaign + age + previous, data = train, method = "C5.0")

ctrl = trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
grid = expand.grid(.model = "rules", .winnow = "TRUE", .trials = 20)
set.seed(122)
m2 = train(y ~ nr.employed + month + poutcome + contact + cons.conf.idx + 
             campaign + age + previous, data = train, method = "C5.0", 
           trControl = ctrl, tuneGrid = grid)

### decision tree (CHAID)

library(rpart)
bank.rpart.model = rpart(y ~ month + campaign + poutcome + euribor3m + nr.employed + job +
                           age + contact + emp.var.rate + pdays,
                         data = bank.train)
# install.packages("rattle")
# installed.packages("rpart.plot")
# install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(bank.rpart.model)
bank.c50.pred = predict(bank.c50.model, bank.test)


#### tunig

m = train(y ~ ., data = bank, method = "C5.0")
summary(m$finalModel)
plot(m.final)

p = predict(m, bank)
table(p, bank$y)
head(predict(m, bank, type = "prob"))

ctrl = trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
grid = expand.grid(.model = "tree", .winnow = "TRUE", .trials = 10)
set.seed(201806)
m2 = train(y ~ ., data = bank, method = "C5.0", trControl = ctrl, tuneGrid = grid)
summary(m2$finalModel)
m2$finalModel$rules


### glm full model



Null.model <- glm(y ~ 1, data = bank, family = binomial)
glm.model <- glm(y ~ ., data = bank, family = binomial)
both.glm <- step(Null.model, 
                 scope = list(lower = Null.model, upper = glm.model), 
                 direction = 'both')



glm.pred <- predict(glm.model, test, type = 'response')

glm.pred1 <- ifelse(glm.pred>0.5,1,0)
glm.pred1 <- factor(glm.pred1, levels=c(0,1), labels = c('no','yes'))
confusionMatrix(glm.pred1, test$y, positive = 'yes')

### glm variable select

both.glm <- step(Null.model, scope = list(lower = Null.model, upper = glm.model), direction = 'both')
both.glm <- glm(y ~ pdays + month + campaign + emp.var.rate + cons.price.idx + 
                  contact + euribor3m + loan
                , data = train, family = binomial)
glm.pred <- predict(both.glm, test, type = 'response')

glm.pred1 <- ifelse(glm.pred>0.5,1,0)
glm.pred1 <- factor(glm.pred1, levels=c(0,1), labels = c('no','yes'))
confusionMatrix(glm.pred1, test$y, positive = 'yes')
