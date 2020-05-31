#######Loading Data#######
prediction = read.csv("../Data/prediction.csv")
testData = read.csv("../Data/test.csv")

library(ggplot2)

Survived = rep("",nrow(prediction))
for (i in 1:nrow(prediction)) {
  if(prediction$Survived[i] == 1){
    Survived[i] = "Survived"
  }
  else
  {
    Survived[i] = "Died"
  }
 
}

testData$Status = Survived

#######Generate Sex Graph#######
sexGraph = ggplot(testData, aes(x = Sex, fill = Status))+
  geom_bar(stat = "count",width = 0.5, position = "stack")+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  geom_text(stat='count',aes(label=scales::percent(..count../sum(..count..))), color="white", position = position_stack(0.5))+
  theme_minimal()


#########Generate Age Grapgh########
Age = rep("", nrow(testData))
for (i in 1:nrow(testData)) {#This code fragment is copied from NB.R
  for (j in 0:7) {
    if (!is.na(testData$Age[i])) {
      if ((testData[i,5] > j*10) && (testData[i,5] <= (j+1)*10)) {
      #j*10 < age < (j+1)*10
        Age[i] = paste(j*10,"-",(j+1)*10)
      }
    }
    else
    {
      Age[i] = "NA"
    }
  }
}
testData$Age = Age
  
ageGraph = ggplot(testData, aes(x = Age, fill = Status))+
  geom_bar(stat = "count", position = "stack")+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  geom_text(stat='count',aes(label=scales::percent(..count../sum(..count..)))
            , color="white", position = position_stack(0.5))+
  theme_minimal()

#########Generate Fare Grapgh########
Fare = rep("", nrow(testData))
for (i in 1:nrow(testData)) {#This code fragment is copied from NB.R
  for (j in 0:5) {
    if (!is.na(testData$Fare[i])) {
      if (((testData$Fare[i]) > j*100) && (testData$Fare[i] <= (j+1)*100)) {
        Fare[i] = paste(j*100,"-",(j+1)*100)
      }
    }
    else
    {
      Fare[i] = "NA"
    }
  }
}
testData$Fare = Fare

FareGraph = ggplot(testData, aes(x = Fare, fill = Status))+
  geom_bar(stat = "count", position = "stack")+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  geom_text(stat='count',aes(label=scales::percent(..count../sum(..count..)))
            , color="white", position = position_stack(0.5))+
  theme_minimal()


#####Show Result#######

#sexGraph
#ageGraph
#FareGraph
