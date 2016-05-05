




###set up the working directory

setwd("/Users/jinganqu/Dropbox/optimus_project")
#setwd("C:/Users/Jingan/Dropbox/optimus_project")

###load the needed packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("MASS","randomForest","class","e1071","ggplot2")
ipak(packages)



###load the data file

################################
#########data preparation#######
################################

dataset <- read.csv("cleanDataWithoutLowAccurancy.csv")


### make up the missing value by prediction



#use random forest to predict the missing value in Gender
rf.model <- randomForest(factor(Gender)~.,data=na.omit(dataset))
id_na <- which(is.na(dataset$Gender))
dataset$cd[id_na] <- as.numeric(predict(rf.model,newdata=dataset[id_na,]))



#use random forest to predict the missing value in SecondaryBrainRegion
rf.model <- randomForest(factor(SecondaryBrainRegion)~.,data=na.omit(dataset))
id_na <- which(is.na(dataset$SecondaryBrainRegion))
dataset$cd[id_na] <- as.numeric(predict(rf.model,newdata=dataset[id_na,]))

#use random forest to predict the missing value in PrimaryCellClass
rf.model <- randomForest(factor(PrimaryCellClass)~.,data=na.omit(dataset))
id_na <- which(is.na(dataset$PrimaryCellClass))
dataset$cd[id_na] <- as.numeric(predict(rf.model,newdata=dataset[id_na,]))

#use random forest to predict the missing value in SecondaryCellClass
rf.model <- randomForest(factor(SecondaryCellClass)~.,data=na.omit(dataset))
id_na <- which(is.na(dataset$SecondaryCellClass))
dataset$cd[id_na] <- as.numeric(predict(rf.model,newdata=dataset[id_na,]))

#use random forest to predict the missing value in TertiaryCellClass
rf.model <- randomForest(factor(TertiaryCellClass)~.,data=na.omit(dataset))
id_na <- which(is.na(dataset$TertiaryCellClass))
dataset$cd[id_na] <- as.numeric(predict(rf.model,newdata=dataset[id_na,]))





#save the data for training
set.seed(1)
id <- sample(1:nrow(dataset))
training_id <- id[1:round(nrow(dataset)*2/3)]

X <- dataset[training_id,-3]




# the label variable 
Y <- dataset[training_id,3]

#save the data X, Y as one .csv file
write.csv(data.frame(X,Y),"gender.csv",row.names=F)

################################
#########Model building#########
################################

### use different classification algorithm to build different 

#algorithm: logistic regression, knn, naive bayesian, random forest, LDA
lr.model <- glm(Y~.,family=binomial,data = X)
#knn.model no training

# change  to norminal
tmp <- X
tmp <- cut(tmp,4)
tmp[,18:23] <- apply(tmp[,18:23],2,function(x){as.factor(x>=median(x))})
tmp <- apply(tmp,2,as.factor)
naive.model <- naiveBayes(x=tmp,y=Y)


rf.model <- randomForest(x=X,y=factor(Y))
lda.model <- lda(as.factor(Y) ~ .,data = X)


################################
####Classification gender#######
################################

### predict gender

#test data 
X_test <- dataset[-training_id,-4] 
Y_test <- dataset[-training_id,4]



# use accuracy as weight to ensemble (result from Weka)
w.lr <- 85.5176
w.knn <- 84.364
w.naive <- 80.666
w.rf <- 85.9882
w.lda <- 85.0003
weight <- c(w.lr, w.knn, w.naive, w.rf, w.lda)



#predict by logistic regression
predLR <- predict(lr.model,newdata=X_test,type = "response")
pred.lr <- factor(as.numeric(predLR>0.5))

#predict by knn
# numeric & scale
tmpX <- X
tmpX_test <- X_test
for (i in 1:ncol(X)){
  tmpX[,i] <- as.numeric(tmpX[,i])
  tmpX_test[,i] <- as.numeric(tmpX_test[,i])
  tmpX[,i] <- (tmpX[,i] - min(tmpX[,i]))/(max(tmpX[,i]) - min(tmpX[,i]))
  tmpX_test[,i] <- (tmpX_test[,i] - min(tmpX_test[,i]))/
    (max(tmpX_test[,i]) - min(tmpX_test[,i]))
  
}
pred.knn <- knn(train = tmpX, test = tmpX_test, cl = as.factor(Y), k = 100)

#predict by naive bayesian
tmp2 <- X_test
tmp <- as.data.frame(tmp)
tmp2$age <- tmp$age
tmp2[,18:23] <- tmp[,18:23]
tmp2 <- apply(tmp2,2,as.factor)
predNB <- predict(naive.model,newdata=tmp2,type = "raw")
pred.naive <- as.factor(c(0,1)[apply(predNB,1,which.max)])


#predict by random forest
pred.rf <- predict(rf.model,newdata=X_test)

#predict by LDA
predLDA <- predict(lda.model,newdata = X_test)
pred.lda <- predLDA$class


#combine all the results of different models by weighted ensemble method
#weights is the accuracy from Weka

# get the variable vote
results <- data.frame(pred.lr,pred.knn,pred.naive,pred.rf,pred.lda)
vote <- apply(results,1,function(x) {
  as.numeric(((x==1) %*% weight) >= ((x==0) %*% weight))
})

#calculate the probability (model LR, naive and LDA can calculate the p)
#got the variable vote_prob
p.lr <- predLR
p.naive <- predNB[,2]
p.lda <- predLDA$posterior[,2]
pDF <- data.frame(p.lr, p.naive, p.lda)
vote_prob <- apply(pDF,1, function(x){
  x %*% weight[c(1,3,5)]/sum(weight[c(1,3,5)])
})

#save the data file as .csv

final <- data.frame(dat, vote, vote_prob)
write.csv(final, "genderPrediction.csv",row.names=F)






################################
#########Result Visulization####
################################

t <- table(vote)
tmp <- as.numeric(t)
at <- cumsum(tmp)-0.5*tmp
label=paste0(round(tmp/sum(tmp),3) * 100,"%")
pie <- ggplot(final,aes(x = factor(1),fill = factor(vote))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  #labs(title = "Pie Chart for Prediction") +
  annotate(geom = "text", y = at, x = 1, label = label) +
  xlab("") +
  ylab("") 
plot(pie)

ggplot(final, aes(vote_prob, fill = factor(vote))) +
  geom_histogram(binwidth = 0.01) +
  xlab("Probability") +
  ylab("Frequency") 






################################
#########Experiment#############
################################


######## predit
X <- data.frame(infor_data,vh[,7:ncol(vh)]) 
names(X)[24:32] <- 1:9
Y <- vh[,6]

lr.model <- glm(Y~.,family=binomial,data = X)
#knn.model no training

# change to norminal
tmp <- X
tmp <- cut(tmp,4)
tmp[,18:23] <- apply(tmp[,18:23],2,function(x){as.factor(x>=median(x))})
tmp <- apply(tmp,2,as.factor)
naive.model <- naiveBayes(x=tmp,y=Y)


rf.model <- randomForest(x=X,y=factor(Y))
lda.model <- lda(as.factor(Y) ~ .,data = X)

X_test <- dataset



# use accuracy as weight to ensemble (result from Weka)
w.lr <- 85.5176
w.knn <- 84.364
w.naive <- 80.666
w.rf <- 85.9882
w.lda <- 85.0003
weight <- c(w.lr, w.knn, w.naive, w.rf, w.lda)


### use weighted ensemble method to predict 
#predict logistic regression
predLR <- predict(lr.model,newdata=X_test,type = "response")
pred.lr <- factor(as.numeric(predLR>0.5))

#predict knn
# numeric & scale
tmpX <- X
tmpX_test <- X_test
for (i in 1:ncol(X)){
  tmpX[,i] <- as.numeric(tmpX[,i])
  tmpX_test[,i] <- as.numeric(tmpX_test[,i])
  tmpX[,i] <- (tmpX[,i] - min(tmpX[,i]))/(max(tmpX[,i]) - min(tmpX[,i]))
  tmpX_test[,i] <- (tmpX_test[,i] - min(tmpX_test[,i]))/
    (max(tmpX_test[,i]) - min(tmpX_test[,i]))
  
}
pred.knn <- knn(train = tmpX, test = tmpX_test, cl = as.factor(Y), k = 100)

#predict naive bayesian
tmp2 <- X_test
tmp <- as.data.frame(tmp)

tmp2[,18:23] <- tmp[,18:23]
tmp2 <- apply(tmp2,2,as.factor)
predNB <- predict(naive.model,newdata=tmp2,type = "raw")
pred.naive <- as.factor(c(0,1)[apply(predNB,1,which.max)])


#predict random forest
pred.rf <- predict(rf.model,newdata=X_test)

#predict LDA
predLDA <- predict(lda.model,newdata = X_test)
pred.lda <- predLDA$class

#combine all the results of different models with accuracy weights

results <- data.frame(pred.lr,pred.knn,pred.naive,pred.rf,pred.lda)
vote <- apply(results,1,function(x) {
  as.numeric(((x==1) %*% weight) >= ((x==0) %*% weight))
})

#calculate the probability (model LR, naive and LDA can calculate the p)
p.lr <- predLR
p.naive <- predNB[,2]
p.lda <- predLDA$posterior[,2]
pDF <- data.frame(p.lr, p.naive, p.lda)
vote_prob <- apply(pDF,1, function(x){
  x %*% weight[c(1,3,5)]/sum(weight[c(1,3,5)])
})

# visualize the result
t1 <- table(vote,vh[,2])
accuracy1 <- sum(diag(t1))/sum(t1)
df1 <- as.data.frame(t1)
names(df1) <- c("Prediction","True","Freq")
label1 <- paste0(df1$Freq,"/",c("right","wrong","wrong","right"))

g1 <-ggplot(df1, aes(x = factor(Prediction), 
                     y = Freq, fill=factor(True))) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(y=Freq, ymax=Freq, label=Freq), 
            position= position_dodge(width=0.9), vjust=-.5, color="black") +
  #scale_y_continuous("Frequency",limits=c(0,100),breaks=seq(0, 100, 10)) + 
  xlab("Prediction ") +
  ylab("Frequency") 

plot(g1)







######## predit 

X <- data.frame(infor_data,vh[,9:ncol(vh)]) 
names(X)[24:30] <- 1:7
Y <- vh[,8]

lr.model <- glm(Y~.,family=binomial,data = X)
#knn.model no training

# change to norminal
tmp <- X
tmp[,18:23] <- apply(tmp[,18:23],2,function(x){as.factor(x>=median(x))})
tmp <- apply(tmp,2,as.factor)
naive.model <- naiveBayes(x=tmp,y=Y)


rf.model <- randomForest(x=X,y=factor(Y))
lda.model <- lda(as.factor(Y) ~ .,data = X)

X_test <- data.frame(infor_data,vh[,5:11]) 



# use accuracy as weight to ensemble (result from Weka)
w.lr <- 85.5176
w.knn <- 84.364
w.naive <- 80.666
w.rf <- 85.9882
w.lda <- 85.0003
weight <- c(w.lr, w.knn, w.naive, w.rf, w.lda)


### use weighted ensemble method to predict
#predict logistic regression
predLR <- predict(lr.model,newdata=X_test,type = "response")
pred.lr <- factor(as.numeric(predLR>0.5))

#predict knn
# numeric & scale
tmpX <- X
tmpX_test <- X_test
for (i in 1:ncol(X)){
  tmpX[,i] <- as.numeric(tmpX[,i])
  tmpX_test[,i] <- as.numeric(tmpX_test[,i])
  tmpX[,i] <- (tmpX[,i] - min(tmpX[,i]))/(max(tmpX[,i]) - min(tmpX[,i]))
  tmpX_test[,i] <- (tmpX_test[,i] - min(tmpX_test[,i]))/
    (max(tmpX_test[,i]) - min(tmpX_test[,i]))
  
}
pred.knn <- knn(train = tmpX, test = tmpX_test, cl = as.factor(Y), k = 100)

#predict naive bayesian
tmp2 <- X_test
tmp <- as.data.frame(tmp)
tmp2$age <- tmp$age
tmp2[,18:23] <- tmp[,18:23]
tmp2 <- apply(tmp2,2,as.factor)
predNB <- predict(naive.model,newdata=tmp2,type = "raw")
pred.naive <- as.factor(c(0,1)[apply(predNB,1,which.max)])


#predict random forest
pred.rf <- predict(rf.model,newdata=X_test)

#predict LDA
predLDA <- predict(lda.model,newdata = X_test)
pred.lda <- predLDA$class

#combine all the results of different models with accuracy weights

results <- data.frame(pred.lr,pred.knn,pred.naive,pred.rf,pred.lda)
vote <- apply(results,1,function(x) {
  as.numeric(((x==1) %*% weight) >= ((x==0) %*% weight))
})

#calculate the probability (model LR, naive and LDA can calculate the p)
p.lr <- predLR
p.naive <- predNB[,2]
p.lda <- predLDA$posterior[,2]
pDF <- data.frame(p.lr, p.naive, p.lda)
vote_prob <- apply(pDF,1, function(x){
  x %*% weight[c(1,3,5)]/sum(weight[c(1,3,5)])
})


# visualize the result
t2 <- table(vote,vh[,4])
accuracy2 <- sum(diag(t2))/sum(t2)
df2 <- as.data.frame(t2)
names(df2) <- c("Prediction","True","Freq")
#label2 <- paste0(df2$Freq,"/",c("right","wrong","wrong","right"))

ggplot(df2, aes(x = factor(Prediction), 
                y = Freq, fill=factor(True))) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(y=Freq, ymax=Freq, label=Freq), 
            position= position_dodge(width=0.9), vjust=-.5, color="black") +
  #scale_y_continuous("Frequency",limits=c(0,100),breaks=seq(0, 100, 10)) + 
  xlab("Prediction ") +
  ylab("Frequency") 




################################
#########Result Visulization####
################################

t <- table(vote)
tmp <- as.numeric(t)
at <- cumsum(tmp)-0.5*tmp
label=paste0(round(tmp/sum(tmp),3) * 100,"%")
pie <- ggplot(final,aes(x = factor(1),fill = factor(vote))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") 
annotate(geom = "text", y = at, x = 1, label = label) +
  xlab("") +
  ylab("") 

ggplot(final, aes(vote_prob, fill = factor(vote))) +
  geom_histogram(binwidth = 0.01) +
  xlab("Probability") +
  ylab("Frequency")





################################
#########Experiment#############
################################


######## predit
X <- data.frame(infor_data,vh[,7:ncol(vh)]) 
names(X)[24:32] <- 1:9
Y <- vh[,6]

lr.model <- glm(Y~.,family=binomial,data = X)
#knn.model no training

# change age to norminal
tmp <- X
tmp$age <- cut(tmp$age,4)
tmp[,18:23] <- apply(tmp[,18:23],2,function(x){as.factor(x>=median(x))})
tmp <- apply(tmp,2,as.factor)
naive.model <- naiveBayes(x=tmp,y=Y)


rf.model <- randomForest(x=X,y=factor(Y))
lda.model <- lda(as.factor(Y) ~ .,data = X)

X_test <- data.frame(infor_data,vh[,3:11]) 
# 34 features, ignore the variable 00p - 02g, 
# because in the model, we just consider the voter turnout in the past 10 years
# we assume the other voter turnout 10 years ago cannot affect the 14g directly.

# rename the vh in X_test as 1,2,3,4...
names(X_test)[24:32] <- 1:9

# use accuracy as weight to ensemble (result from Weka)
w.lr <- 85.5176
w.knn <- 84.364
w.naive <- 80.666
w.rf <- 85.9882
w.lda <- 85.0003
weight <- c(w.lr, w.knn, w.naive, w.rf, w.lda)


### use weighted ensemble method to predict 14g
#predict logistic regression
predLR <- predict(lr.model,newdata=X_test,type = "response")
pred.lr <- factor(as.numeric(predLR>0.5))

#predict knn
# numeric & scale
tmpX <- X
tmpX_test <- X_test
for (i in 1:ncol(X)){
  tmpX[,i] <- as.numeric(tmpX[,i])
  tmpX_test[,i] <- as.numeric(tmpX_test[,i])
  tmpX[,i] <- (tmpX[,i] - min(tmpX[,i]))/(max(tmpX[,i]) - min(tmpX[,i]))
  tmpX_test[,i] <- (tmpX_test[,i] - min(tmpX_test[,i]))/
    (max(tmpX_test[,i]) - min(tmpX_test[,i]))
  
}
pred.knn <- knn(train = tmpX, test = tmpX_test, cl = as.factor(Y), k = 100)

#predict naive bayesian
tmp2 <- X_test
tmp <- as.data.frame(tmp)
tmp2$age <- tmp$age
tmp2[,18:23] <- tmp[,18:23]
tmp2 <- apply(tmp2,2,as.factor)
predNB <- predict(naive.model,newdata=tmp2,type = "raw")
pred.naive <- as.factor(c(0,1)[apply(predNB,1,which.max)])


#predict random forest
pred.rf <- predict(rf.model,newdata=X_test)

#predict LDA
predLDA <- predict(lda.model,newdata = X_test)
pred.lda <- predLDA$class

#combine all the results of different models with accuracy weights

results <- data.frame(pred.lr,pred.knn,pred.naive,pred.rf,pred.lda)
vote <- apply(results,1,function(x) {
  as.numeric(((x==1) %*% weight) >= ((x==0) %*% weight))
})

#calculate the probability (model LR, naive and LDA can calculate the p)
p.lr <- predLR
p.naive <- predNB[,2]
p.lda <- predLDA$posterior[,2]
pDF <- data.frame(p.lr, p.naive, p.lda)
vote_prob <- apply(pDF,1, function(x){
  x %*% weight[c(1,3,5)]/sum(weight[c(1,3,5)])
})

# visualize the result
t1 <- table(vote,vh[,2])
accuracy1 <- sum(diag(t1))/sum(t1)
df1 <- as.data.frame(t1)
names(df1) <- c("Prediction","True","Freq")


g1 <-ggplot(df1, aes(x = factor(Prediction), 
                     y = Freq, fill=factor(True))) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(y=Freq, ymax=Freq, label=Freq), 
            position= position_dodge(width=0.9), vjust=-.5, color="black")
plot(g1)







######## predit 

X <- data.frame(infor_data,vh[,9:ncol(vh)]) 
names(X)[24:30] <- 1:7
Y <- vh[,8]

lr.model <- glm(Y~.,family=binomial,data = X)
#knn.model no training

# change age to norminal
tmp <- X
tmp$age <- cut(tmp$age,4)
tmp[,18:23] <- apply(tmp[,18:23],2,function(x){as.factor(x>=median(x))})
tmp <- apply(tmp,2,as.factor)
naive.model <- naiveBayes(x=tmp,y=Y)


rf.model <- randomForest(x=X,y=factor(Y))
lda.model <- lda(as.factor(Y) ~ .,data = X)

X_test <- data.frame(infor_data,vh[,5:11]) 

# rename the vh in X_test as 1,2,3,4...
names(X_test)[24:30] <- 1:7

# use accuracy as weight to ensemble (result from Weka)
w.lr <- 85.5176
w.knn <- 84.364
w.naive <- 80.666
w.rf <- 85.9882
w.lda <- 85.0003
weight <- c(w.lr, w.knn, w.naive, w.rf, w.lda)


### use weighted ensemble method to predict 14g
#predict logistic regression
predLR <- predict(lr.model,newdata=X_test,type = "response")
pred.lr <- factor(as.numeric(predLR>0.5))

#predict knn
# numeric & scale
tmpX <- X
tmpX_test <- X_test
for (i in 1:ncol(X)){
  tmpX[,i] <- as.numeric(tmpX[,i])
  tmpX_test[,i] <- as.numeric(tmpX_test[,i])
  tmpX[,i] <- (tmpX[,i] - min(tmpX[,i]))/(max(tmpX[,i]) - min(tmpX[,i]))
  tmpX_test[,i] <- (tmpX_test[,i] - min(tmpX_test[,i]))/
    (max(tmpX_test[,i]) - min(tmpX_test[,i]))
  
}
pred.knn <- knn(train = tmpX, test = tmpX_test, cl = as.factor(Y), k = 100)

#predict naive bayesian
tmp2 <- X_test
tmp <- as.data.frame(tmp)
tmp2$age <- tmp$age
tmp2[,18:23] <- tmp[,18:23]
tmp2 <- apply(tmp2,2,as.factor)
predNB <- predict(naive.model,newdata=tmp2,type = "raw")
pred.naive <- as.factor(c(0,1)[apply(predNB,1,which.max)])


#predict random forest
pred.rf <- predict(rf.model,newdata=X_test)

#predict LDA
predLDA <- predict(lda.model,newdata = X_test)
pred.lda <- predLDA$class

#combine all the results of different models with accuracy weights

results <- data.frame(pred.lr,pred.knn,pred.naive,pred.rf,pred.lda)
vote <- apply(results,1,function(x) {
  as.numeric(((x==1) %*% weight) >= ((x==0) %*% weight))
})

#calculate the probability (model LR, naive and LDA can calculate the p)
p.lr <- predLR
p.naive <- predNB[,2]
p.lda <- predLDA$posterior[,2]
pDF <- data.frame(p.lr, p.naive, p.lda)
vote_prob <- apply(pDF,1, function(x){
  x %*% weight[c(1,3,5)]/sum(weight[c(1,3,5)])
})


# visualize the result
t2 <- table(vote,vh[,4])
accuracy2 <- sum(diag(t2))/sum(t2)
df2 <- as.data.frame(t2)
names(df2) <- c("Prediction","True","Freq")
#label2 <- paste0(df2$Freq,"/",c("right","wrong","wrong","right"))

ggplot(df2, aes(x = factor(Prediction), 
                y = Freq, fill=factor(True))) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(y=Freq, ymax=Freq, label=Freq), 
            position= position_dodge(width=0.9), vjust=-.5, color="black") +
  #scale_y_continuous("Frequency",limits=c(0,100),breaks=seq(0, 100, 10)) + 
  xlab("Prediction voter turnout") +
  ylab("Frequency") +
  scale_fill_discrete(name ="True Voter Turnout")





save.image("gender.RData")



#####visulization data
original <- read.csv("dataWith190feature6label.csv")



t <- table(original$Gender)
tmp <- as.numeric(t)
at <- cumsum(tmp)-0.5*tmp
label=paste0(round(tmp/sum(tmp),3) * 100,"%")
pie <- ggplot(original,aes(x = factor(1),fill = factor(Gender))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart for Drosophila Gender") +
  annotate(geom = "text", y = at[1:3], x = 1, label = label[1:3]) +
  xlab("") +
  ylab("") +
  scale_fill_discrete(name ="Drosophila Gender")
plot(pie)

ggsave("gender.png",pie)



t <- table(original$PrimaryBrainRegion)
tmp <- as.numeric(t)
at <- cumsum(tmp)-0.5*tmp
per <- round(tmp/sum(tmp),3)
label=paste0(round(tmp/sum(tmp),3) * 100,"%")
at <- at[per!=0]
label <- label[per!=0]

pie <- ggplot(original,aes(x = factor(1),fill = factor(PrimaryBrainRegion))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart for Primary Brain Region") +
  annotate(geom = "text", y = at, x = 1.4, label = label) +
  xlab("") +
  ylab("") +
  scale_fill_discrete(name ="Primary Brain Region")
plot(pie)
ggsave("PrimaryBrainRegion.png",pie)

temp <- read.csv("SecondaryBrainRegion2.csv")
t <- table(temp$SecondaryBrainRegion)
tmp <- as.numeric(t)
at <- cumsum(tmp)-0.5*tmp
per <- round(tmp/sum(tmp),3) *100
label=paste0(round(tmp/sum(tmp),3) * 100,"%")
at <- at[per>.5]
label <- label[per>.5]

pie <- ggplot(temp,aes(x = factor(1),fill = factor(SecondaryBrainRegion))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart for Secondary Brain Region") +
  annotate(geom = "text", y = at, x = 1.4, label = label) +
  xlab("") +
  ylab("") +
  scale_fill_discrete(name ="Secondary Brain Region")
plot(pie)
ggsave("SecondaryBrainRegion.png",pie)


t <- table(original$PrimaryCellClass)
tmp <- as.numeric(t)
at <- cumsum(tmp)-0.5*tmp
per <- round(tmp/sum(tmp),3)
label=paste0(round(tmp/sum(tmp),3) * 100,"%")
at <- at[per!=0]
label <- label[per!=0]

pie <- ggplot(original,aes(x = factor(1),fill = factor(PrimaryCellClass))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart for Primary Cell Class") +
  annotate(geom = "text", y = at, x = 1.4, label = label) +
  xlab("") +
  ylab("") +
  scale_fill_discrete(name ="Primary Cell Classn")
plot(pie)
ggsave("PrimaryCellClass.png",pie)

temp <- read.csv("SecondaryCellClass2.csv")
t <- table(temp$SecondaryCellClass)
tmp <- as.numeric(t)
at <- cumsum(tmp)-0.5*tmp
per <- round(tmp/sum(tmp),3)
label=paste0(round(tmp/sum(tmp),3) * 100,"%")
at <- at[per!=0]
label <- label[per!=0]

pie <- ggplot(temp,aes(x = factor(1),fill = factor(SecondaryCellClass))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart for Secondary Cell Class") +
  annotate(geom = "text", y = at, x = 1.4, label = label) +
  xlab("") +
  ylab("") +
  scale_fill_discrete(name ="Secondary Cell Class")
plot(pie)
ggsave("SecondaryCellClass.png",pie)






temp <- read.csv("TertiaryCellClass2.csv")
t <- table(temp$TertiaryCellClass)
tmp <- as.numeric(t)
at <- cumsum(tmp)-0.5*tmp
per <- round(tmp/sum(tmp),3)
label=paste0(round(tmp/sum(tmp),3) * 100,"%")
at <- at[per!=0]
label <- label[per!=0]

pie <- ggplot(temp,aes(x = factor(1),fill = factor(TertiaryCellClass))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart for Tertiary Cell Class") +
  annotate(geom = "text", y = at, x = 1.4, label = label) +
  xlab("") +
  ylab("") +
  scale_fill_discrete(name ="Tertiary Cell Class")
plot(pie)
ggsave("TertiaryCellClass.png",pie)




