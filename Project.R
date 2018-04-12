#loading data
acath <- read.csv("~/Desktop/acath.csv")

#STEP 1: DATA PRE-PROCESSING
head(acath)
summary(acath)
acath <- na.omit(acath)
lapply(acath, class)

acath <- with(acath, data.frame(data.frame(sex=factor(sex), age=age, cad.dur=cad.dur, choleste=choleste,
                                           sigdz = factor(sigdz), tvdlm = factor(tvdlm))))
lapply(acath, class)


#STEP 2: EXPLORATORY DATA-ANALYSIS
summary(acath)             
boxplot(age~sigdz, data=acath, ylab="Age of patient",
        xlab="Incidence of significant coronary disease")

with(acath, tapply(age, sigdz, mean))

boxplot(cad.dur~sigdz, data=acath, ylab="Duration of symptoms",
        xlab="Incidence of significant coronary disease")

with(acath, tapply(cad.dur, sigdz, mean))

boxplot(choleste~sigdz, data=acath, ylab="Cholesterol",
        xlab="Incidence of significant coronary disease")

with(acath, tapply(choleste, sigdz, mean))
         
with(acath, table(sigdz,sex))
with(acath, table(sigdz,tvdlm))
#TVDLM NOT A GOOD PREDICTOR!!! Because everytime tvdlm is 1, so is sigdz!
#Get rid of tvdlm column
acath <- acath[,-6]
head(acath)
summary(acath)
#STEP 3: PARTITION THE DATA
set.seed(5)
index_training <- sample(1:nrow(acath), round(0.7*nrow(acath)))
training_data <- acath[index_training,]
test_data <- acath[-index_training,]

#STEP 4: LOAD PACKAGES FOR CLASSIFICATION TREES
library(rpart)
library(rpart.plot)

#STEP 5: FITTING THE MODEL
tree_1 <- rpart(sigdz ~ sex + age + cad.dur + choleste, 
                data = training_data, method = "class", control = rpart.control(minsplit=0))
rpart.plot(tree_1,type = 1,extra=2,under=TRUE, main = "Classification Tree for Coronary Disease")
fancyRpartPlot(tree_1)
printcp(tree_1)
plotcp(tree_1)

pruned_tree_1 <- prune(tree_1, cp = 0.01)                  
rpart.plot(pruned_tree_1,type = 1,extra=2,under=TRUE, main = "Pruned Classification Tree for Coronary Disease")

#Could also try minsplit, but for simplicity, will not do it.

#STEP 6: PREDICTION AND VALIDATION
test_data$pred <- predict(tree_1,test_data, type="class")
table(test_data$sigdz,test_data$pred,dnn = c("Actual","Predicted"))
#Accuracy=0.704
#Recall of coronary disease = 0.85
#Precision of prediction of coronary disease = 0.81
#Recall of non-disease = 0.61
#Precision of prediction of non-disease = 0.67
#I care about the false negatives.

#STEP 7: DIFFERENT SAMPLE SIZE
set.seed(27)
index_training_2 <- sample(1:nrow(acath), round(0.5*nrow(acath)))
training_data_2 <- acath[index_training_2,]
test_data_2 <- acath[-index_training_2,]

tree_2 <- rpart(sigdz ~ sex + age + cad.dur + choleste, data = training_data_2, method = "class", control = rpart.control(minsplit=0))
rpart.plot(tree_2,type = 1,extra=2,under=TRUE, main = "Classification Tree for Coronary Disease")
fancyRpartPlot(tree_2)
printcp(tree_2)
plotcp(tree_2)

pruned_tree_2 <- prune(tree_2, cp = 0.01)                  
rpart.plot(pruned_tree_2,type = 1,extra=2,under=TRUE, main = "Pruned Classification Tree 2 for Coronary Disease")

test_data_2$pred <- predict(tree_2,test_data_2, type="class")
table(test_data_2$sigdz,test_data_2$pred,dnn = c("Actual","Predicted"))


library(rattle)
fancyRpartPlot(tree_3)

#LIFT CHART:
library(BCA)
lift.chart(c("tree_1"), data=test_data,targLevel="1",trueResp=1490/2258, type="cumulative", sub="Validation")
lift.chart(c("tree_2"), data=test_data_2, targLevel="1",trueResp=1490/2258, type="cumulative", sub="Validation")
