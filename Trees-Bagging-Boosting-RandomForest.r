
# Libraries 

library(ISLR2)
library(janitor)
library(tidyverse)
library(tidymodels)
library(broom)
library(gt)
library(patchwork)
library(tree)
library(caret)
library(rpart.plot)

# Load and process data 

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

    ## Set aside testing set

sample <- sample(c(TRUE, FALSE), nrow(churn), replace=TRUE, prob=c(0.7,0.3))
train  <- churn[sample, ]
test   <- churn[!sample, ]


    ## Undersampling training set

library(ROSE)

under <- ovun.sample(Exited~., data = train,
                     p = 0.5, 
                     method="under")$data

table(under$Exited)


    ## Single decision tree

tree <- tree(
  formula = Exited ~ .,
  data    = under,
  method  = "class"
)
plot(tree)
text(tree)


treepreds <-predict(tree, test, type = 'class')
confusionMatrix(treepreds, test$Exited)



    ## tune using Cross validation


Sample_tree<-rpart(Exited~., method ="class", data = under, control=rpart.control(minsplit=2, cp=0.0001))
plotcp(Sample_tree)
text(sample_tree)


Sample_tree<-rpart(Exited~., method ="class", data = under, control=rpart.control(minsplit=2, cp=0.00083))
prp(Sample_tree)
text(Sample_tree)

treepreds <-predict(Sample_tree, test, type = 'class')
confusionMatrix(treepreds, test$Exited)

library(data.tree)
Sample_tree <- as.Node(Sample_tree)



    ## Bagging

library(ipred)

bag <- seq(10,500,20)
bag <- as.data.frame(bag)
bag$Probability <- rep(0,25)
bag$Consensus <- rep(0,25)
x=1 

for (i in seq(10,500,20)){
  
  model <- bagging(Exited ~., data = under, coob= T, nbagg = i)
  pruned <- prune(model, cp = 0.00083)
  
  predictions <- predict(pruned, test)
  bag$Consensus[x] <- 1 - unname(confusionMatrix(predictions, test$Exited)$overall[1])
  print(bag$Consensus[x])
  
  probs <- predict(pruned, test, prob = TRUE)
  conf2 <- confusionMatrix(probs, test$Exited)
  paccuracy <- conf2$overall[1]
  unname(paccuracy)
  Probability <- 1-paccuracy
  bag$Probability[x] <- Probability
  
  x = x+1
}

library(ggplot2)

ggplot(data = bag) + 
  geom_point(aes(x = bag , y = Probability), color = "hotpink") +
  geom_line(aes(x = bag, y = Probability ), color = "hotpink") +
  geom_point(aes(x = bag, y = Consensus ), color = "orange")  +
  geom_line(aes(x = bag, y =  Consensus ), color = "orange")  +
  theme_bw() +
  labs(x = "Number of Bootstrap Samples", y = "Test Error")


    ## Optimal Bagged Model

model <- bagging(Exited ~., data = under, coob= T, nbagg = 370)
pruned <- prune(model, cp = 0.00083)
predictions <- predict(model, test)
confusionMatrix(predictions, test$Exited)



    ## Random Forest Classification 



library(randomForest)

rf <- randomForest(Exited~.,data = under, nodesize = 1)
rf

rfPreds <- predict(rf, test)
confusionMatrix(rfPreds, test$Exited)

    ## Tune it 

mtry <- seq(1,10,1)
random <- as.data.frame(mtry)
random$Error <- rep(0,10)
y <- 1

for (i in 1:10){
  testRF <- randomForest(Exited~.,data = under, mtry = i)
  Preds <- predict(testRF, test)
  conf <- confusionMatrix(Preds, test$Exited)
  accuracy <- conf$overall[1]
  unname(accuracy)
  error <- 1-accuracy
  random$Error[y] <- error
  
  y <- y + 1
}

ggplot(random, aes( x = mtry, y = Error)) + 
  geom_point(color = "#009E73") + labs( x = "# of Random Variables Selected (mtry)", y = "Test Set Error") +
  geom_line(color = "#009E73")


      ## Tune NTREE

ntree <-  seq(10,500, 10)
mtry3 <- as.data.frame(ntree)
mtry3$Error <- rep(0,50)
z = 1

for (i in ntree){
  testRF <- randomForest(Exited~.,data = under, mtry = 3, ntree = i)
  Preds <- predict(testRF, test)
  conf <- confusionMatrix(Preds, test$Exited)
  accuracy <- conf$overall[1]
  unname(accuracy)
  error <- 1-accuracy
  mtry3$Error[z] <- error

  z <- z + 1
}


mtry10 <- as.data.frame(ntree)
mtry10$Error <- rep(0,50)
z = 1

for (i in ntree){
  testRF <- randomForest(Exited~.,data = under, mtry = 10, ntree = i)
  Preds <- predict(testRF, test)
  conf <- confusionMatrix(Preds, test$Exited)
  accuracy <- conf$overall[1]
  unname(accuracy)
  error <- 1-accuracy
  mtry10$Error[z] <- error

  z <- z + 1
}


mtry5 <- as.data.frame(ntree)
mtry5$Error <- rep(0,50)
z = 1

for (i in ntree){
  testRF <- randomForest(Exited~.,data = under, mtry = 5, ntree = i)
  Preds <- predict(testRF, test)
  conf <- confusionMatrix(Preds, test$Exited)
  accuracy <- conf$overall[1]
  unname(accuracy)
  error <- 1-accuracy
  mtry5$Error[z] <- error

  z <- z + 1
}


ggplot(mtry3, aes( x = ntree, y = Error)) + geom_point(color = "GREEN") + geom_line()

mtry3 <- mtry3[ntree < 510,]

ntreetune <- as.data.frame(ntree)
ntreetune$"m = p = 10" <- mtry10$Error
ntreetune$"m = sgrt(p) = 3" <- mtry3$Error
ntreetune$"m = p/2 = 5" <- mtry5$Error

ggplot(data = ntreetune) + 
  geom_point(aes(x = ntree, y = `m = p = 10`), color = "hotpink") +
  geom_line(aes(x = ntree, y = `m = p = 10`), color = "hotpink") +
  geom_point(aes(x = ntree, y = `m = sgrt(p) = 3`), color = "orange")  +
  geom_line(aes(x = ntree, y = `m = sgrt(p) = 3`), color = "orange") +
  geom_point(aes(x = ntree, y = `m = p/2 = 5`), color = "sky blue") +
  geom_line(aes(x = ntree, y = `m = p/2 = 5`), color = "sky blue") +
  theme_bw() +
  labs(x = "Number of Trees", y = "Test Classification Error")
  
  

    ## optimal random forest model

RF <- randomForest(Exited~.,data = under, mtry = 3, ntree = 440, 
                   importance=TRUE, sampSize = 9100)
RF
Preds <- predict(RF, test)
confusionMatrix(Preds, test$Exited)

    ## Variable Importance:

varImpPlot(RF)


    ## Boosting

library(gbm)

under$Exited <- as.numeric(under$Exited)
under$Exited <- ifelse(under$Exited > 1.5, 1, 0)

boost <- gbm(
  formula = Exited ~ ., data = under, distribution = "bernoulli",
  n.trees = 100, shrinkage = 0.1,
  n.minobsinnode = 10, cv.folds = 5) 

preds <- predict(boost, test, type = "response")
preds<- ifelse(preds > 0.5,1,0) 

preds <- as.factor(preds)
confusionMatrix(preds, test$Exited)


    ## Tune the boost (number of trees and depth)

ntree <- rep(seq(100, 5000, 50), each = 3)
trees <- as.data.frame(ntree)
trees$Depth <- rep(1:3, 99)
trees$Error <- rep(0,297)

for (i in 1:297){
    boost <- gbm(
      formula = Exited ~ ., data = under, distribution = "bernoulli",
      n.trees = trees$ntree[i], 
      shrinkage = 0.1, 
      interaction.depth = trees$Depth[i],
      n.minobsinnode = 10, cv.folds = 3) 
    
    preds <- predict(boost, test, type = "response")
    preds<- ifelse(preds > 0.5,1,0) 
    
    preds <- as.factor(preds)
    conf <- confusionMatrix(preds, test$Exited)
    accuracy <- conf$overall[1]
    unname(accuracy)
    error <- 1-accuracy
    
    trees$Error[i] <- error
    
  }

ntree <- seq(100, 2500, 50)
treesplit <- as.data.frame(ntree)
treesplit$D1 <- subset(trees, Depth == 1)$Error
treesplit$D2 <- subset(trees, Depth == 2)$Error
treesplit$D3 <- subset(trees, Depth == 3)$Error

ggplot(data = treesplit) + 
  geom_point(aes(x = ntree , y = D1), color = "hotpink") +
  geom_line(aes(x = ntree, y = D1 ), color = "hotpink") +
  geom_point(aes(x = ntree, y = D2 ), color = "orange")  +
  geom_line(aes(x = ntree, y =  D2 ), color = "orange") +
  geom_point(aes(x = ntree, y = D3 ), color = "sky blue") +
  geom_line(aes(x = ntree, y = D3 ), color = "sky blue") +
  theme_bw() +
  labs(x = "Number of Trees", y = "Test Classification Error")


lambda <- seq(0.01, 0.2, 0.01)
shrink <- as.data.frame(lambda)
shrink$Error <- rep(0, 20)
r <- 1

for (i in lambda){
  print(i)
  boost <- gbm(
    formula = Exited ~ ., data = under, distribution = "bernoulli",
    n.trees = 2050, shrinkage = i, interaction.depth = 3,
    n.minobsinnode = 10, cv.folds = 2) 
  
  preds <- predict(boost, test, type = "response")
  preds<- ifelse(preds > 0.5,1,0) 
  
  preds <- as.factor(preds)
  conf <- confusionMatrix(preds, test$Exited)
  accuracy <- conf$overall[1]
  unname(accuracy)
  error <- 1-accuracy
  
  shrink$Error[r] <- error
  
  r <- r + 1
}

ggplot(shrink, aes(x = lambda, y = Error))+geom_point(color = "turquoise4")+
  geom_line(color = "turquoise4") + theme_bw()


    ## Optimal Boost

boost <- gbm(
  formula = Exited ~ ., data = under, distribution = "bernoulli",
  n.trees = 2040, shrinkage = 0.08, interaction.depth = 6,
  n.minobsinnode = 5, cv.folds = 5) 

preds <- predict(boost, test, type = "response")
preds<- ifelse(preds > 0.5,1,0) 

preds <- as.factor(preds)
confusionMatrix(preds, test$Exited)

need <- summary.gbm(boost)
need




