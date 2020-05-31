# feature analysis
library(readr)
library(ggplot2)
library(ggthemes)
library(scales)
library(InformationValue)
library(dplyr)
library(stringr)

# Firstly, load and combine.
train <- read.csv("train.csv")
test <- read.csv("test.csv")
data <- bind_rows(train, test)
train.row <- 1:nrow(train)
test.row <- (1 + nrow(train)):(nrow(train) + nrow(test))
# show
str(data)           

# Pclass
data$Survived <- factor(data$Survived)
ggplot(data = data[1:nrow(train),], mapping = aes(x = Pclass, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Pclass') + 
  ylab('Count') + 
  ggtitle('How Pclass impact survivor') + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
WOETable(X=factor(data$Pclass[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=factor(data$Pclass[1:nrow(train)]), Y=data$Survived[1:nrow(train)])

# Title
data$Title <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
data$Title <- sub(' ', '', data$Title)
data$Title[data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
data$Title[data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
data$Title[data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
data$Title <- factor(data$Title)
ggplot(data = data[1:nrow(train),], mapping = aes(x = Title, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='stack') + 
  xlab('Title') + 
  ylab('Count') + 
  ggtitle('How Title impact survivor') + 
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("Perish", "Survived")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
WOETable(X=data$Title[1:nrow(train)], Y=data$Survived[1:nrow(train)])
IV(X=data$Title[1:nrow(train)], Y=data$Survived[1:nrow(train)])

# SEX
data$Sex <- as.factor(data$Sex)
ggplot(data = data[1:nrow(train),], mapping = aes(x = Sex, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Sex') + 
  ylab('Count') + 
  ggtitle('How Sex impact survivo') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=data$Sex[1:nrow(train)], Y=data$Survived[1:nrow(train)])
IV(X=data$Sex[1:nrow(train)], Y=data$Survived[1:nrow(train)])

# Age (age<18 , age>=18)
ggplot(data = data[(!is.na(data$Age)) & row(data[, 'Age']) <= 891, ], aes(x = Age, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=5)  + 
  labs(title = "How Age impact survivor", x = "Age", y = "Count", fill = "Survived")

# SibSp
ggplot(data = data[1:nrow(train),], mapping = aes(x = SibSp, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "How SibSp impact survivor", x = "Sibsp", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=as.factor(data$SibSp[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$SibSp[1:nrow(train)]), Y=data$Survived[1:nrow(train)])

# Parch
ggplot(data = data[1:nrow(train),], mapping = aes(x = Parch, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "How Parch impact survivor", x = "Parch", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=as.factor(data$Parch[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$Parch[1:nrow(train)]), Y=data$Survived[1:nrow(train)])

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

# Ticket
ticket.count <- aggregate(data$Ticket, by = list(data$Ticket), function(x) sum(!is.na(x)))
data$TicketCount <- apply(data, 1, function(x) ticket.count[which(ticket.count[, 1] == x['Ticket']), 2])
data$TicketCount <- factor(sapply(data$TicketCount, function(x) ifelse(x > 1, 'Share', 'Unique')))
ggplot(data = data[1:nrow(train),], mapping = aes(x = TicketCount, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('TicketCount') + 
  ylab('Count') + 
  ggtitle('How TicketCount impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=data$TicketCount[1:nrow(train)], Y=data$Survived[1:nrow(train)])
IV(X=data$TicketCount[1:nrow(train)], Y=data$Survived[1:nrow(train)])

# Fair
ggplot(data = data[(!is.na(data$Fare)) & row(data[, 'Fare']) <= 891, ], aes(x = Fare, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=10)  + 
  labs(title = "How Fare impact survivor", x = "Fare", y = "Count", fill = "Survived")

# Cabin
ggplot(data[1:nrow(train), ], mapping = aes(x = as.factor(sapply(data$Cabin[1:nrow(train)], function(x) str_sub(x, start = 1, end = 1))), y = ..count.., fill = Survived)) +
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Cabin') +
  ylab('Count') +
  ggtitle('How Cabin impact survivor') +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
data$Cabin <- sapply(data$Cabin, function(x) str_sub(x, start = 1, end = 1))
WOETable(X=as.factor(data$Cabin[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$Cabin[1:nrow(train)]), Y=data$Survived[1:nrow(train)])

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