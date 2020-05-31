# feature analysis
library(readr)
library(ggplot2)
library(ggthemes)
library(scales)
library(InformationValue)
library(dplyr)
library(stringr)

# Firstly, Load the training data and test data into the data.frame named train and test through the following code.
train <- read.csv("../Data/train.csv")
test <- read.csv("../Data/test.csv")
data <- bind_rows(train, test)
train.row <- 1:nrow(train)
test.row <- (1 + nrow(train)):(nrow(train) + nrow(test))
# Data Preview
str(data)           


# Pclass
data$Survived <- factor(data$Survived)
ggplot(data = data[1:nrow(train),], mapping = aes(x = Pclass, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Pclass') + 
  ylab('Count') + 
  ggtitle('How Pclass impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=factor(data$Pclass[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=factor(data$Pclass[1:nrow(train)]), Y=data$Survived[1:nrow(train)])


# SEX
data$Sex <- as.factor(data$Sex)
ggplot(data = data[1:nrow(train),], mapping = aes(x = Sex, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Sex') + 
  ylab('Count') + 
  ggtitle('How Sex impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=data$Sex[1:nrow(train)], Y=data$Survived[1:nrow(train)])
IV(X=data$Sex[1:nrow(train)], Y=data$Survived[1:nrow(train)])


# Age (age<18 , age>=18)
ggplot(data = data[(!is.na(data$Age)) & row(data['Age']) <= 891, ], aes(x = Age, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=5)  + 
  labs(title = "How Age impact survivor", x = "Age", y = "Count", fill = "Survived")



# Familysize
data$FamilySize <- data$SibSp + data$Parch + 1
ggplot(data = data[1:nrow(train),], mapping = aes(x = FamilySize, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('FamilySize') + 
  ylab('Count') + 
  ggtitle('How FamilySize impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=as.factor(data$FamilySize[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$FamilySize[1:nrow(train)]), Y=data$Survived[1:nrow(train)])



# Fair
ggplot(data = data[(!is.na(data$Fare)) & row(data['Fare']) <= 891, ], aes(x = Fare, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=10)  + 
  labs(title = "How Fare impact survivor", x = "Fare", y = "Count", fill = "Survived")



# Embarked
ggplot(data[1:nrow(train), ], mapping = aes(x = Embarked, y = ..count.., fill = Survived)) +
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Embarked') +
  ylab('Count') +
  ggtitle('How Embarked impact survivor') +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=as.factor(data$Embarked[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$Embarked[1:nrow(train)]), Y=data$Survived[1:nrow(train)])