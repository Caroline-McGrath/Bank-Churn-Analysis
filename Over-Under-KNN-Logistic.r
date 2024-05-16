

churn <- read.csv("C:\\Users\\Pat\\Downloads\\train.churn.csv")
str(churn)
churn <- subset(churn, select = -c(id, CustomerId,Surname))
churn$CreditScore <- as.integer(churn$CreditScore)
churn$Age <- as.integer(churn$Age)
churn$Balance <- as.integer(churn$Balance)
churn$HasCrCard <- as.factor(churn$HasCrCard)
churn$IsActiveMember <-as.factor(churn$IsActiveMember)
churn$EstimatedSalary <- as.integer(churn$EstimatedSalary)
churn$Exited <- as.factor(churn$Exited)
churn$Geography <- as.factor(churn$Geography)
churn$Gender <- as.factor(churn$Gender)
str(churn)


### Cross-Validation 

library(caret)
library(class)
library(dplyr)

## Knn

sample <- sample(c(TRUE, FALSE), nrow(churn), replace=TRUE, prob=c(0.7,0.3))
train  <- churn[sample, ]
test   <- churn[!sample, ]

churn2 <- train %>%
  mutate_if(is.numeric,scale) %>%
  mutate_if(is.matrix, as.numeric)

  

test2 <- test %>%
  mutate_if(is.numeric,scale)

trControl <- trainControl(method  = "cv", number  = 10)

knn <- train(Exited ~ .,
             method     = "knn",
             tuneGrid  = expand.grid(k = 9),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = churn2)

knnpred <- predict(knn, test)

levels(knnpred) <- c('Stayed', 'Churned')
levels(test$Exited) <- c('Stayed', 'Churned')

confusionMatrix(knnpred, test$Exited)


library(pROC)
knnPredict <- predict(knn,newdata = test , type="prob")
knnROC <- roc(test2$Exited, knnPredict[,"1"])
plot(knnROC, type="S", print.thres = T, print.auc=T)


## Logistic 

trControl <- trainControl(method  = "cv", number  = 10)

glm <- train(Exited ~ .,
             method     = "glm",
             trControl  = trControl,
             metric     = "Accuracy",
             data       = train)


summary(glm)
glmroc <- roc(glm)

glmpreds <- predict(glm, newdata = test)
glmpreds

levels(glmpreds) <- c('Stayed', 'Churned')
levels(test$Exited) <- c('Stayed', 'Churned')

confusionMatrix(glmpreds , test$Exited)

glmpreds <- predict(glm, newdata = test, type = "prob")
glmpreds

library(pROC)
glmROC <- roc(test$Exited, glmpreds[,"1"])
plot(glmROC, type="S", print.thres = T, print.auc=T)



## oversampling


library(ROSE)

over <- ovun.sample(Exited~., data = train,
                    p = 0.5, 
                    method="over")$data
table(over$Exited)


      ## knn 

knnover <- over %>%
  mutate_if(is.numeric,scale)

test2 <- test %>%
  mutate_if(is.numeric,scale)

trControl <- trainControl(method  = "cv", number  = 10)

knnover <- train(Exited ~ .,
             method     = "knn",
             trControl  = trControl,
             tuneGrid  = expand.grid(k = 9),
             metric     = "Accuracy",
             data       = knnover)

knnpredover <- predict(knnover, test2)

levels(knnpredover) <- c('Stayed', 'Churned')

confusionMatrix(knnpredover, test2$Exited)

knnPredict <- predict(knnover,newdata = test2 , type="prob")
knnROC <- roc(test2$Exited, knnPredict[,"1"])
plot(knnROC, type="S",print.thres = T,print.auc=T)


## logistic 


trControl <- trainControl(method  = "cv", number  = 10)

glmover <- train(Exited ~ .,
             method     = "glm",
             trControl  = trControl,
             metric     = "Accuracy",
             data       = over)

glmpredsover <- predict(glmover, newdata = test)
glmpredsover

levels(glmpredsover) <- c('Stayed', 'Churned')

confusionMatrix(glmpredsover , test$Exited)

glmpredsover <- predict(glmover, newdata = test, type = "prob")
glmpredsover

glmROCover <- roc(test$Exited, glmpredsover[,"1"])
plot(glmROCover, type="S", print.thres = T, print.auc=T)







## Under sampling


under <- ovun.sample(Exited~., data = train,
                     p = 0.5, 
                     method="under")$data

table(under$Exited)


    ## knn 

knnunder <- under %>%
  mutate_if(is.numeric,scale)

test2 <- test %>%
  mutate_if(is.numeric,scale)

trControl <- trainControl(method  = "cv", number  = 10)

knnunder <- train(Exited ~ .,
                 method     = "knn",
                 trControl  = trControl,
                 tuneGrid  = expand.grid(k = 9),
                 metric     = "Accuracy",
                 data       = knnunder)

knnpredunder <- predict(knnunder, test2)

levels(knnpredunder) <- c('Stayed', 'Churned')

confusionMatrix(knnpredunder, test2$Exited)

knnPredict <- predict(knnunder,newdata = test2 , type="prob")
knnROC <- roc(test2$Exited, knnPredict[,"1"])
plot(knnROC, type="S",print.thres = T,print.auc=T)


      ## Logistic 

glmunder <- train(Exited ~ .,
                 method     = "glm",
                 trControl  = trControl,
                 metric     = "Accuracy",
                 data       = under)

glmpredsunder <- predict(glmunder, newdata = test)
glmpredsunder

levels(glmpredsunder) <- c('Stayed', 'Churned')

confusionMatrix(glmpredsunder , test$Exited)

glmpredsunder <- predict(glmunder, newdata = test, type = "prob")
glmpredsunder

glmROCunder <- roc(test$Exited, glmpredsunder[,"1"])
plot(glmROCunder, type="S", print.thres = T, print.auc=T)





## BOTH 


both <- ovun.sample(Exited~., data = train,
                    p = 0.5, 
                    method="both")$data

table(both$Exited)

    ## KNN 

knnboth <- both %>%
  mutate_if(is.numeric,scale)

test2 <- test %>%
  mutate_if(is.numeric,scale)

trControl <- trainControl(method  = "cv", number  = 10)

knnboth <- train(Exited ~ .,
                  method     = "knn",
                  trControl  = trControl,
                  tuneGrid  = expand.grid(k = 9),
                  metric     = "Accuracy",
                  data       = knnboth)

knnpredboth <- predict(knnboth, test2)

levels(knnpredboth) <- c('Stayed', 'Churned')

confusionMatrix(knnpredboth, test2$Exited)

knnPredict <- predict(knnboth,newdata = test2 , type="prob")
knnROC <- roc(test2$Exited, knnPredict[,"1"])
plot(knnROC, type="S",print.thres = T,print.auc=T)


## Logistic 

glmboth <- train(Exited ~ .,
                  method     = "glm",
                  trControl  = trControl,
                  metric     = "Accuracy",
                  data       = both)

glmpredsboth <- predict(glmboth, newdata = test)
glmpredsboth

levels(glmpredsboth) <- c('Stayed', 'Churned')

confusionMatrix(glmpredsboth , test$Exited)

glmpredsboth <- predict(glmboth, newdata = test, type = "prob")
glmpredsboth

glmROCboth <- roc(test$Exited, glmpredsboth[,"1"])
plot(glmROCboth, type="S", print.thres = T, print.auc=T)












