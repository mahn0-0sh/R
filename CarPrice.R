library(data.table)
library(ggplot2)
library(ggcorrplot)

#load data

dtable = fread("CarPrice_Assignment.csv")


#boxplots

#boxplots on columns fueltype, wheelbase, carheight
box_plot <- ggplot(dtable, aes(x = fueltype, y = wheelbase))
box_plot + geom_boxplot()

box_plot <- ggplot(dtable, aes(x = wheelbase, y = carheight, group=1))
box_plot + geom_boxplot()

box_plot <- ggplot(dtable, aes(x = fueltype, y = carheight))
box_plot + geom_boxplot()

#missing values

#we put mean values for numeric and most repeated for categorical.
#curbweight
dtable$curbweight[is.na(dtable$curbweight)] <- mean(dtable$curbweight, na.rm = TRUE)
#boreratio
dtable$boreratio[is.na(dtable$boreratio)] <- mean(dtable$boreratio, na.rm = TRUE)
#chars
dtable[dtable == ''] <- NA
#cylindernumber
c <- dtable$cylindernumber
type <- "five"
uniq <- unique(c)
for (i in 1:length(uniq)) {
   if( length(which(c==uniq[i])) > length(which(c==type)) ) type = uniq[i]
}
dtable$cylindernumber[is.na(dtable$cylindernumber)] <- type
#carbody
c <- dtable$carbody
type <- "sedan"
uniq <- unique(c)
for (i in 1:length(uniq)) {
  if( length(which(c==uniq[i])) > length(which(c==type)) ) type = uniq[i]
}
dtable$carbody[is.na(dtable$carbody)] <- type



#correlation map

#choosing numeric columns
numt <- dtable[,c(11:15 ,18 ,20:27)]
#get cor matrix
cort <- cor(numt)
#plot
ggcorrplot(cort, hc.order = TRUE, type = "full",
           lab = FALSE)
#lab = TRUE for coefficients.

#correlation tests

#H_0: cor=0 -> declined
cor.test(dtable$carlength, dtable$curbweight)
#H_0: cor=<0 -> declined
cor.test(dtable$carwidth, dtable$carheight, alternative = "greater")
#H_0: cor>=0 -> not declined
cor.test(dtable$carwidth, dtable$carwidth, alternative = "less")
#H_0: cor>=0 -> declined
cor.test(dtable$carwidth, dtable$highwaympg, alternative = "less")
# H_0: cor = 0 -> declined
cor.test(dtable$citympg, dtable$highwaympg)



#dummies

#dummy for every categorical feature except car name (This is not a feature!)
#removed one category of each one to prevent collinearity
dtable$symboling<-as.character(dtable$symboling)
dt <- fastDummies::dummy_cols(dtable[,-c(4)],remove_first_dummy = TRUE)


#train-test

#keep one random
set.seed(7)
#take out 80 percent of data randomly, for train
ss <- sample(1:2,size=nrow(dtable),replace=TRUE,prob=c(0.7,0.3))
x <- ss==1
y <- !x

#leave out categorical columns
numeric_table <- dt[,c(10:14,17,19:60)]
#train and test sets on numeric table
train <- numeric_table[x,]
test <- numeric_table[y,]





#phase 2

#fit

#first we fit a linear model on training set
train_fit <- lm(price ~. ,data=train)
summary(train_fit)

train_prediction <- predict(train_fit, train)
train_real <- train$price

train_TSS <- 0
for(i in 1:length(train_real)) {
  train_TSS <- train_TSS + (train_real[i] - mean(train_real))^2
}
train_MSE <- deviance(train_fit)/length(train_real)

deviance(train_fit)
train_TSS
train_MSE

#prediction on test
test_prediction <- predict(train_fit, test)
test_real <- test$price
resid <- (test_prediction-test_real)^2
test_RSS <-sum(resid)
test_TSS <- 0
n <- length(test_real)
for(i in 1:n) {
  test_TSS <- test_TSS + (test_real[i] - mean(test_real))^2
}
test_MSE <- test_RSS/n
test_R2 <- 1-(test_RSS/test_TSS)
p <- ncol(test)-1
test_adj_R2 <- 1-( ((1-test_R2)*(n-1))/(n-p-1) )

test_RSS
test_TSS
test_MSE
test_R2
test_adj_R2


library(jtools)
library(broom)
plot_coefs(train_fit)


#phase 3

#backward selection
feature_train_table <- numeric_table[x,]
p_vals <- summary(train_fit)$coefficients[,4]
p_vals <- p_vals[-1]
for(i in 1:32) { # to 30
  ind <- which.max(p_vals)
  p_ind <- grep("price", colnames(feature_train_table))
  if(ind>=p_ind) ind<-ind+1
  feature_train_table <- subset(feature_train_table, select = -c(ind))
  new_train_fit=lm(price ~. ,data=feature_train_table)
  p_vals <- summary(new_train_fit)$coefficients[,4]
  p_vals <- p_vals[-1]
}
summary(new_train_fit)

deviance(new_train_fit)
deviance(new_train_fit)/length(train_real)
train_MSE <- deviance(train_fit)/length(train_real)

test_prediction <- predict(new_train_fit, test)
test_real <- test$price
resid <- (test_prediction-test_real)^2
test_RSS <-sum(resid)
test_TSS <- 0
n <- length(test_real)
for(i in 1:n) {
  test_TSS <- test_TSS + (test_real[i] - mean(test_real))^2
}
test_MSE <- test_RSS/n
test_R2 <- 1-(test_RSS/test_TSS)
p <- ncol(test)-1
test_adj_R2 <- 1-( ((1-test_R2)*(n-1))/(n-p-1) )

test_RSS
test_TSS
test_MSE
test_R2
test_adj_R2



#forward using RSS

library(leaps)
regfit_full = regsubsets(price ~ .+price, data = train, nvmax = 19, method = "forward")
reg_summary = summary(regfit_full)

#ANOVA

feature_train_table <- numeric_table[x,]
t_fit <- anova(train_fit)
f_stats <- t_fit[-c(44),c(4)]
vect <- c()
for(i in 1:10) {
  vect<-append(vect, which.max(f_stats))
  f_stats[which.max(f_stats)] <- 0
}
vect
for(i in 1:10) {
  print(row.names(t_fit)[vect[i]])
}

length(unique(dtable$CarName))



#synergy

model <- lm(price ~.^2 ,data=train)
summary(model)
# pairs that might have synergy (high correlation):
# highwaympg,citympg / highwaympg,curbweight / citympg,curbweight / citympg,horsepower
# wheelbase,carwidth / carwidth,carlength
# enginesize,horsepower / enginesize,curbweight / wheelbase,carlength / curbweight,carwidth
model <- lm(price ~. +(highwaympg+citympg+curbweight)^2 +(wheelbase+carwidth+carlength)^2
            +(carwidth+curbweight)^2+(citympg+horsepower)^2,data=train)
summary(model)

summary(aov(price~highwaympg*citympg, data=train))
summary(aov(price~highwaympg*curbweight, data=train))
summary(aov(price~curbweight*citympg, data=train))
summary(aov(price~horsepower*citympg, data=train))
summary(aov(price~wheelbase*carwidth, data=train))
summary(aov(price~carwidth*carlength, data=train))
summary(aov(price~enginesize*horsepower, data=train))
summary(aov(price~enginesize*curbweight, data=train))
summary(aov(price~wheelbase*carlength, data=train))
summary(aov(price~carwidth*curbweight, data=train))


elm_model <- step(model, direction = "backward")
summary(elm_model)

#optional

library(randomForest)

df <- train
tst <- test
names(df)[names(df) == 'symboling_-2'] <- 'sym_minus'
names(tst)[names(tst) == 'symboling_-2'] <- 'sym_minus'

rfs <- randomForest(price ~. , data = df)
#importance(rfs)
varImpPlot(rfs)

rfs_pred <- predict(rfs, tst)
tst$rfs_pred = rfs_pred
rss <- sum( (tst$rfs_pred-tst$price)^2 )
rfs
test_MSE

test_R2 <- 1-(rss/test_TSS)
test_adj_R2 <- 1-( ((1-test_R2)*(n-1))/(n-p-1) )
mse <- rss/n

rss
mse
test_R2
test_adj_R2
