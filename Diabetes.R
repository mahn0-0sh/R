library(data.table)
library(ggplot2)

# reading data
d <- fread("diabetes_binary_5050split_health_indicators_BRFSS2015.csv")

# here we guess that some predictors have significant effect
# BMI, Age, NoDocbcCost, HighChol, Sex
# we plot data of these predictors against each class to check our guess
d_num <- d
d$Diabetes_binary = as.factor(d$Diabetes_binary)
# BMI boxplot
ggplot(d, aes(BMI, group = Diabetes_binary, color = Diabetes_binary))+
  geom_boxplot(alpha = .75)

# Age boxplot
ggplot(d, aes(Age, group = Diabetes_binary, color = Diabetes_binary))+
  geom_boxplot(alpha = .75)

# NoDocbcCost plot by percentage
ds = d[, .(n = .N), .(Diabetes_binary, NoDocbcCost)]
ds[, n_total := sum(n), .(NoDocbcCost)]
ds[, n_percent := n / n_total]
ggplot(ds, aes(as.factor(NoDocbcCost), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )

# HighChol plot by percentage
ds = d[, .(n = .N), .(Diabetes_binary, HighChol)]
ds[, n_total := sum(n), .(HighChol)]
ds[, n_percent := n / n_total]
ggplot(ds, aes(as.factor(HighChol), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )

# Sex plot by percentage
ds = d[, .(n = .N), .(Diabetes_binary, Sex)]
ds[, n_total := sum(n), .(Sex)]
ds[, n_percent := n / n_total]
ggplot(ds, 
       aes(as.factor(Sex), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )

summary(d)
# this show us that we have no missing values

# balance data set
library(ROSE)
#odt <- ovun.sample(Diabetes_012~. ,data=d, method="over")

library(tidyverse)

set.seed(7)
#take out 60 percent of data randomly, for train, 20-20 for validation and test
ss <- sample(1:3,size=nrow(d),replace=TRUE,prob=c(0.6,0.2,0.2))
train <- d[ss==1, ]
val <- d[ss==2, ]
test <- d[ss==3, ]

glm.fits = glm(Diabetes_binary ~. ,
               data=train ,family="binomial")
summary (glm.fits)
#features with very little p-value (high significance)
#HighBP, HighChol, CholCheck, BMI, HvyAlcoholConsump, NoDocbcCost, GenHlth, Sex, Age, Education, Income                                                                                                                       

library(glmnet)
#x=model.matrix(Diabetes_binary~., train)[,-1]
#y=train$Diabetes_binary
#grid=10^seq(10,-2, length =100)
#mod=glmnet (x,y,alpha=1, lambda=grid,family = "binomial")

set.seed(1)
x_train=model.matrix(Diabetes_binary~., train)[,-1]
y_train=train$Diabetes_binary
cv.out = cv.glmnet(x_train, y_train, alpha = 1, family = "binomial") # Fit lasso model on training data
#plot(cv.out) # Draw plot of training MSE as a function of lambda
bestlam = cv.out$lambda.min # Select lamda that minimizes training MSE
bestlam

set.seed(1)
x_val=model.matrix(Diabetes_binary~., val)[,-1]
y_val=val$Diabetes_binary
cv.out = cv.glmnet(x_val, y_val, alpha = 1, family = "binomial") # Fit lasso model on training data
vallam = cv.out$lambda.min # Select lamda that minimizes training MSE
vallam


x_test=model.matrix(Diabetes_binary~., test)[,-1]
y_test=test$Diabetes_binary

grid=10^seq(10,-2, length =100)
tr_model=glmnet(x_train, y_train, alpha=1, lambda=grid,family = "binomial")
#lasso.pred=predict(tr_model, s=bestlam, newx=x_test)

#lasso_pred = predict(tr_model, s = bestlam, newx = x_test) # Use best lambda to predict test data
#mean((lasso_pred - y_test)^2)

#lasso.coef=predict(tr_model ,type="coefficients", s= bestlam, newx=x_test) [1:22,]

lasso.coef=predict(tr_model ,type="coefficients", s= bestlam) [1:22,]
lasso.coef

lam <- (bestlam+vallam)/2
predictions_train <- predict(tr_model, s = bestlam, newx = x_train)

predictions_test <- predict(tr_model, s = bestlam, newx = x_test)

predicted_classes <- ifelse(predictions_test > 0.3, 1, 0)
er <- (predicted_classes - (as.numeric(test$Diabetes_binary)-1))^2
lasso_error_rate <- sum(er)/nrow(test)




#feature selection using random forest
library(randomForest)

set.seed(7)
rfs <- randomForest(Diabetes_binary ~. ,data=train)
var_imp <- importance(rfs)
rownames(var_imp)
which(rownames(var_imp)==colnames(dt))
names <- rownames(var_imp)
names
dt <- d
for(i in 1:21) {
  if(var_imp[i]<300) {
    j <- which(names[i]==colnames(dt))
    dt <- subset(dt, select = -c(j))
  }
}

set.seed(8)
rfs2 <- randomForest(Diabetes_binary ~. ,data=dt[ss==1,])

set.seed(9)
rfs_pred <- predict(rfs, test)
er <- (as.numeric(rfs_pred) - as.numeric(test$Diabetes_binary))^2
error_rate <- sum(er)/nrow(test)
error_rate

set.seed(10)
rfs_pred <- predict(rfs2, test)
er <- (as.numeric(rfs_pred) - as.numeric(test$Diabetes_binary))^2
sum(er)
error_rate <- sum(er)/nrow(test)
error_rate



library(rpart)
library(rpart.plot)
set.seed(1234)
tree <- rpart(Diabetes_binary ~., data = train)
rpart.plot(tree)
p <- predict(tree, test, type = 'class')
as.numeric(p)[10]
err <- mean( (as.numeric(test$Diabetes_binary)-as.numeric(p))^2 )



# undersampling method for imbalanced classes
#balanced_sample = NULL
#tmp_df = rbind(d%>%filter(Diabetes_012=='0'), d%>%filter(Diabetes_012=='1'))
#tmp<-ovun.sample(Diabetes_012 ~ ., data = tmp_df, method = "under", p = 0.5, seed = 5)$data
#balanced_sample<-rbind(balanced_sample, tmp)

#tmp_df <- NULL
#tmp_df = rbind(d%>%filter(Diabetes_012=='2'), d%>%filter(Diabetes_012=='1'))
#tmp<-ovun.sample(Diabetes_012 ~ ., data = tmp_df, method = "under", p = 0.5, seed = 5)$data
#balanced_sample<-rbind(balanced_sample, tmp%>%filter(Diabetes_012=='2'))

