#Load the datasets
train <- read.csv("train.csv",head=TRUE)
test <- read.csv("test.csv",head=TRUE)

#Add a variable survived to test to make equal number of variables in test and train for append
test.survived <- data.frame(Survived=rep("None",nrow(test)),test[,])

#Rearranges the first and second cols
col_idx <- grep("PassengerId", names(test.survived))
test.survived <- test.survived[, c(col_idx, (1:ncol(test.survived))[-col_idx])]

#Combine both datasets
data.combined <- rbind(train,test.survived)

str(data.combined)

#Change char and int variables(categorical) to a factor
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Load ggplot2 for visualisation
library(ggplot2)

#Hypothesis: High class people have higher rates of survival
train$Pclass <- as.factor(train$Pclass)
ggplot(train,aes(x=Pclass, fill=factor(Survived)))+
  geom_bar(width = 0.5)+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill = "Survived")

#Check the first few names
head(as.character(train$Survived))

#Check the number of unique names in combined data
length(unique(as.character(data.combined$Name)))

#Get duplicate names and store them in a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

#Displays all the duplicate records (2 sets of duplicate)#So they are 4 different people!
data.combined[which(data.combined$Name %in% dup.names),]

#What is up with "Miss" and "Mr" thing?
library(stringr)

#Find any correlation with miss and other factor(eg: sibsp)
misses <- data.combined[which(str_detect(data.combined$Name,"Miss")),]
misses[1:5,]

#Hypothesis: Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name,"Mrs")),]
mrses[1:5,]

#Check for male the same pattern exists
males <- data.combined[which(data.combined$Sex=="male"),]
males[1:5,]

#Expand Pclass and Survived by adding Title
extractTitle <- function(Name)
{
  Name <- as.character(Name)
  
  if(length(grep("Miss. ",Name))>0){
    return("Miss. ")
  }
  else if(length(grep("Master. ",Name))>0){
    return("Master. ")
  }
  else if(length(grep("Mrs. ",Name))>0){
    return("Mrs. ")
  }
  else if(length(grep("Mr. ",Name))>0){
    return("Mr. ")
  }
  else{
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)){
  titles <- c(titles , extractTitle(data.combined[i,"Name"]))
}
data.combined$Title <- as.factor(titles)

#Since Survived label exists only for Train dataset , we plot only for 891 rows
#Cause Test dataset has Survived as None

ggplot(data.combined[1:891,], aes(x=Title, fill=Survived))+
         geom_bar(width=0.5)+
         facet_wrap(~Pclass)+
         ggtitle("Pclass")+
         xlab("Title")+
         ylab("Total Count")+
         labs(fill = "Survived")
#-----------------------------------------------------------------------

table(data.combined$Sex)
#Plot the graph for Sex,Pclass,Survived
ggplot(data.combined[1:891,] , aes(x=Sex, fill=Survived))+
  geom_bar(width=0.5)+
  facet_wrap(~Pclass)
  ggtitle(Pclass)+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived")
  
#Distribution of age over entire dataset
summary(data.combined$Age)
#We see that a large amount of data on age(263) are missing.

#A glace of plot of survival rate- sex,pclass,age
ggplot(data.combined[1:891,] , aes(x=Age, fill=Survived))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~Sex + ~Pclass)+
  ggtitle("Pclass")+
  xlab("Age")+
  ylab("Total Count")+
  labs(fill="Survived")

#Male children can be said as master which is evident from age
#Min age=0.330 and Max=14.5(young only)
boys <- data.combined[which(data.combined$Title == "Master. "),]
summary(boys$Age)

#But for Miss it can be eithe ryoung girls or older woman who are not married
#Min age=0.17 and Max=63(old)
misses <- data.combined[which(data.combined$Title == "Miss. "), ]
summary(misses$Age)

#Plot Miss, Survival with Pclass
ggplot(misses[misses$Survived != "None",], aes(x=Age,fill=Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass)+
  ggtitle("Age for Miss by Pclass")+
  xlab("Age")+
  ylab("Total Count")+
  labs(fill="Survived")

#find the number of miss who are alone 
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))
#So we infer that only 4 miss have their age less than max of master(14.5) out of 150

#SibSp varible summary
summary(data.combined$SibSp)

#Length of unique SibSp
length(unique(data.combined$SibSp))

#Factor it 
data.combined$SibSp <- as.factor(data.combined$SibSp)

#Plot title, SibSp, Pclass
ggplot(data.combined[1:891,] , aes(x=SibSp , fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + ~Title)+
  ggtitle("Pclass, Title")+
  xlab("SibSp")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

#Visualize the Parch variable
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,] , aes(x=Parch, fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + ~Title)+
  ggtitle("Pclass, Title")+
  xlab("Parch")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

#Visualize for family size
temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$Family.size <- as.factor(temp.SibSp + temp.Parch +1)

ggplot(data.combined[1:891,] , aes(x=Family.size, fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + ~Title)+
  ggtitle("Pclass, Title")+
  xlab("Family Size")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

#-------------------------------------------------------------------
#Lets checkout Ticket Variable
str(data.combined$Ticket)

#Ticket variable has 929factors which is large so convert it to char
#And display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#Try to find structure but nothing seems to be existing
#So we'll look at first char (substr(x,start,stop))
ticket.first.char <- ifelse(data.combined$Ticket == "", " ",substr(data.combined$Ticket,1,1))
unique(ticket.first.char)

#Now to analyse, we factor this
data.combined$ticket.first.char <- as.factor(ticket.first.char)
str(data.combined$ticket.first.char)

#Visualise plot of first char of ticket
ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Survivability by ticket_first_char")+
  xlab("ticket_first_char")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill="Survived")

#To see if Ticket is predictive
ggplot(data.combined[1:891,] , aes(x=ticket.first.char, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Ticket_First_Char, Pclass")+
  xlab("ticket_first_char")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill="Survived")

#To see if Ticket, Title
ggplot(data.combined[1:891,] , aes(x=ticket.first.char, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass + ~Title)+
  ggtitle("Title, Pclass")+
  xlab("ticket_first_char")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill="Survived")

#Next fares paid by Titanic passengers
summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined, aes(x=Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("Fare Distribution")+
  xlab("Fares")+
  ylab("Total Count")+
  ylim(0,200)

#Visualise to see if fare has a predictive power
ggplot(data.combined[1:891,] , aes(x=Fare, fill=Survived))+
  geom_histogram(binwidth=5)+
  facet_wrap(~Pclass + ~Title)+
  ggtitle("Title, Pclass")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,30)+
  labs(fill="Survived")

#Now coming to Cabin variable
str(data.combined$Cabin)

#Large factor value(187), so we convert it into character
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#Replace the empty gaps with U
data.combined[which(data.combined$Cabin == ""),"Cabin"]<-"U"
data.combined$Cabin[1:100]

#First char of Cabin as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

#High level plot of Cabin
ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Survivability by cabin_first_char")+
  xlab("cabin_first_char")+
  ylab("Total Count")+
  ylim(0,750)+
  labs(fill="Survived")

#Check whther Cabin can be used to predict via Pclass
ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass, cabin_first_char")+
  xlab("cabin_first_char")+
  ylab("Total Count")+
  ylim(0,750)+
  labs(fill="Survived")

#Check whther Cabin can be used to predict via Pclass and Title
ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass + ~Title)+
  ggtitle("Pclass, cabin_first_char")+
  xlab("cabin_first_char")+
  ylab("Total Count")+
  ylim(0,750)+
  labs(fill="Survived")

#We have to check for multiple cabin values
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin," "),"Y","N"))

#Check whther multiple Cabin can be used to predict via Pclass and Title
ggplot(data.combined[1:891,], aes(x=cabin.multiple, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass + ~Title)+
  ggtitle("Pclass, cabin_first_char")+
  xlab("cabin_first_char")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

#Now we look at last variable Embark
str(data.combined$Embarked)
levels(data.combined$Embarked)

#Plot the Embarkment for visualization
ggplot(data.combined[1:891,], aes(x=Embarked, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass + ~Title)+
  ggtitle("Pclass, Title")+
  xlab("Embarked")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

#Embarkment, Ticket, Fare, Cabin is not that useful
#Pclass, Title useful(age+sex) is super imp, name not imp, sex is imp, age has to many missing values
#Family Size, Sibsp and Parch has some potential

#----------------------------------------------------------------------

library(randomForest)

# Train a Random Forest with the default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)
#the more right the dot is the more imp it is ( in this case title )


# Train a Random Forest with the default parameters using pclass & title, SibSp
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
# 1% decrease in error rate (21.55% to 19.98%) that means 1% increase in accuracy 


# Train a Random Forest using pclass, title, & parch
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)
#Increased the error rate ,so nearly not predictive as Sibsp
#So we'll see howll it be if we used SibSp and Parch together


# Train a Random Forest using pclass, title, sibsp, parch
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)
#Improves model as error rate decreased 
#That means no. of parents,children,spouse and siblings do matter for survival


# Train a Random Forest using pclass, title, & family.size
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "Family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)
#Small improvement as error rate decreases
#So family size matters as it does reduce the error rate


# Train a Random Forest using pclass, title, sibsp, & family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)
#Increase in error rate


# Train a Random Forest using pclass, title, parch, & family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "Family.size")]
 
set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)


#Pclass ,Title, Family_size least error- 18.07%
#Estimate of Accuracy rate- 81.95%
#---------------------------------------------------------------------

# Subset test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "Title", "Family.size")]

# Make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20160215_1.csv", row.names = FALSE)

# Our submission scores 0.79426, but the OOB predicts that we should score 0.8159.
# Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates
library(caret)
library(doSNOW)






