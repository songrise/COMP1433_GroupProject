##Ruixiang JIANG 19079662d##

######loading data#######

trainData = read.csv("../Data/train.csv")
trainData.row = nrow(trainData) #Number of entries in trainData.
trainData.column = ncol(trainData)
#######end of loading data########



################################
#########Attribute Prob#########
################################


#####alive probability######
aliveProb = length(which(trainData[2] == 1))/trainData.row
#######end of alive prob ########

######Laplace Smoothing######
Laplace = 1


######pclass probability#########
pclassProb = rep(0,3)

for (i in 1:3)
{
  pclassProb[i] = (length(which(trainData[3] == i)) + Laplace) / trainData.row
}
######end of pclass prob calculation######

######sex probability#######
sexProb = rep(0,2) #1 for male, 2 for female

sexProb[1] = (length(which(trainData[5] == "male")) + Laplace) / trainData.row
sexProb[2] = (length(which(trainData[5] == "female")) + Laplace) / trainData.row 
######end of sex prob calculation######

######age probability#######
##TODO I shall use Gaussian distribution
ageProb=rep(0, 9)
for (i in 0:7) {#convert into discreate data
    for (j in which(trainData[6] > i*10)) {
        if (trainData[j,6] <= (i+1)*10) { 
           #i*10< age < (i+1)*10
           ageProb[i+1] = ageProb[i+1] + 1
        }
    }
    ageProb[i+1] = (ageProb[i+1] + Laplace) / length(which(trainData[6] > 0))
}
ageProb[9] = 1 #reserved for NA
######end of age prob calculation######

######sibSp probability#######
sibProb=rep(0,9) #0-8

for (i in 1: 9){
  sibProb[i] = length((which(trainData[7] == i-1))+Laplace) / trainData.row
}
######end of sibSp prob calculation######


#########parch Probability##########
parchProb=rep(0,8)#0-6, last one is reserverd for exceptions

for (i in 1: 7){

  parchProb[i] = (length(which(trainData[8] == i-1)) + Laplace) / trainData.row
}
parchProb[8] = 1
######end of parch prob calculation######

# #########fare Probability##########
# #todo this is floating data
# fareProb = 0
# ######end of fare prob calculation######

#########embarked Probability##########
embarkedProb = rep(0,3) #C,Q,S

embarkedProb[1] = (length(which(trainData[12] == "C"))) / trainData.row
embarkedProb[2] = (length(which(trainData[12] == "Q"))) / trainData.row
embarkedProb[3] = (length(which(trainData[12] == "S"))) / trainData.row
######end of embarked prob calculation######


################################
########Conditional Prob########
################################

######pclass probability#########
pclassProbAlive = rep(0,3)
for (i in which(trainData[2]==1)) { # for index in alive 
  pclassProbAlive[trainData[i,3]] = pclassProbAlive[trainData[i,3]] + 1
}
for (i in 1:3)
{
  
  pclassProbAlive[i] = ((pclassProbAlive[i]+Laplace) / trainData.row) / aliveProb #conditional prob.
}
######end of pclass prob calculation######

######sex probability#######
sexProbAlive = rep(0,2) #1 for male, 2 for female
for (i in which(trainData[2]==1)) {
  if (as.character(trainData[i,5]) == "male") {
    sexProbAlive[1] = sexProbAlive[1] + 1
  }
  else {
    sexProbAlive[2] =  sexProbAlive[2] + 1
  }
}

sexProbAlive[1] = ((sexProbAlive[1]+Laplace) / trainData.row) / aliveProb
sexProbAlive[2] = ((sexProbAlive[2]+Laplace) / trainData.row) / aliveProb
######end of sex prob calculation######

######age probability#######
ageProbAlive=rep(0,9)
for (i in 0:7) {
    for (j in which(trainData[6]>0)) {
        if ((trainData[j,2]==1) &&(trainData[j,6] > i*10) && (trainData[j,6] <= (i+1)*10)) {
            #i*10< age < (i+1)*10
            ageProbAlive[i+1] = ageProbAlive[i+1] + 1
            }
        }
        ageProbAlive[i+1] = ((ageProbAlive[i+1] + Laplace) / length(which(trainData[6]>0))) / aliveProb
}
ageProbAlive[9] = 1 # reserved for NA
######end of age prob calculation######

######sibSp probability#######
sibProbAlive=rep(0,9) #0-8
for (i in which(trainData[2]==1)) {
  sibProbAlive[trainData[i,7]+1] = sibProbAlive[trainData[i,7]+1] + 1
}

for (i in 1: 9){
  
  sibProbAlive[i] = ((sibProbAlive[i]+Laplace) /  trainData.row) / aliveProb
}
######end of sibSp prob calculation######


#########parch Probability##########
parchProbAlive=rep(0,8)#0-6, last one is reserverd for exceptions
for (i in which(trainData[2]==1)) {
  parchProbAlive[trainData[i,8]+1] = parchProbAlive[trainData[i,8]+1] + 1
}

for (i in 1: 7){
  parchProbAlive[i] = ((parchProbAlive[i]+Laplace) / trainData.row) / aliveProb
}
parchProbAlive[8] = 1
######end of parch prob calculation######

# #########fare Probability##########
# fareProb = 0
# ######end of fare prob calculation######

#########embarked Probability##########
embarkedProbAlive = rep(0,3) #C,Q,S
for (i in which(trainData[2]==1)) {
  if (as.character(trainData[i,12]) == "C") {
    embarkedProbAlive[1] = embarkedProbAlive[1] + 1
  }
  else if (as.character(trainData[i,12]) == "Q")
  {
    embarkedProbAlive[2] = embarkedProbAlive[2] + 1
  }
  else
  {
    embarkedProbAlive[3] = embarkedProbAlive[3] + 1
  }
}
# +1 for Laplace Smoothing
embarkedProbAlive[1] = ((embarkedProbAlive[1]+Laplace) /trainData.row) / aliveProb
embarkedProbAlive[2] = ((embarkedProbAlive[2]+Laplace) / trainData.row) / aliveProb
embarkedProbAlive[3] = ((embarkedProbAlive[3]+Laplace) / trainData.row) / aliveProb

######end of embarked prob calculation######



################################
#####Predicting Test Data#######
################################


############Load Test data############
testData= read.csv("../Data/test.csv")
testData.row = nrow(testData)
##########end of loading##############

######Applying Naive Bayes######
Survived = rep(0,testData.row)
for (i in 1:testData.row) {
  ######loading attributes#####
  pclass = testData[i,2]
  age = 9
  for (j in 0:7) {
    if (is.na(testData[i,5])) {
        age = 9 # NA, ignore this attribute (ageProb[9] == 1, ageProbAlive[9] == 1)
        }

    else if ((testData[i,5] > j*10) && (testData[i,5] <= (j+1)*10)) {
        #i*10< age < (i+1)*10
        age = j+1
        }
    }

  if (as.character(testData[i,4]) == "male") {
    sex = 1
  }
  else {
    sex = 2
  }

  sibSp = testData[i,6]+1
  parch = testData[i,7]+1
  if (parch > 6) { #not exists in learning data
    parch = 8 # NA, ignore this attribute(parchProb[8] == 1, parchProbAlive[8] == 1)
  }
  
  if (as.character(testData[i,11]) == "C") {
    embarked = 1
  }
  else if (as.character(testData[i,11]) == "Q") {
    embarked = 2
  }
 else {
    embarked = 3
  }
  ######end of attribute loading#####
  #predict live prob. (Naive Bayes formula)
  p = (pclassProbAlive[pclass]*sexProbAlive[sex]*sibProbAlive[sibSp]*ageProbAlive[age]*parchProbAlive[parch]*embarkedProbAlive[embarked]*aliveProb)/(pclassProb[pclass]*sexProb[sex]*ageProb[age]*parchProb[parch]*sibProb[sibSp]*embarkedProb[embarked])
  #Survived[i] = age #This is for testing
  if (p <= 0.5) {# if predicted probability of survive <= 0.5
    Survived[i] = 0
  }
  else {
    Survived[i] = 1
  }
}

###########Export result############
passengerID = testData[1]
result = data.frame(passengerID,Survived)
write.table(result, file = "../Data/prediction.csv", quote = FALSE, sep = ",",row.names=FALSE)