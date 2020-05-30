###testing for Naive Bayes####

######loading data#######

passengerData = read.csv("../Data/train.csv")
passengerData.row = nrow(passengerData) #Number of entries in passengerData.
passengerData.column = ncol(passengerData)
#######end of loading data########


#####alive probability######
aliveProb = length(which(passengerData[2] == 1))/passengerData.row
#######end of alive prob ########
Laplace = 1
######pclass probability#########
pclassProb = rep(0,3)

for (i in 1:3)
{
  pclassProb[i] = (length(which(passengerData[3] == i)) + Laplace) / passengerData.row
}

######end of pclass prob calculation######

######sex probability#######
sexProb = rep(0,2) #1 for male, 2 for female

sexProb[1] = (length(which(passengerData[5] == "male")) + Laplace) / passengerData.row
sexProb[2] = (length(which(passengerData[5] == "female")) + Laplace) / passengerData.row 
######end of sex prob calculation######

######age probability#######
ageProb=rep(0, 9)
for (i in 0:7) {#convert into discreate data
    for (j in which(passengerData[6] > i*10)) {
        if (passengerData[j,6] <= (i+1)*10) { 
           #i*10< age < (i+1)*10
           ageProb[i+1] = ageProb[i+1] + 1
        }
    }
    ageProb[i+1] = (ageProb[i+1] + Laplace) / length(which(passengerData[6] > 0))
}
ageProb[9] = 1 #reserved for NA
######end of age prob calculation######

######sibSp probability#######
sibProb=rep(0,9) #0-8


for (i in 1: 9){
  sibProb[i] = length((which(passengerData[7] == i-1))+Laplace) / passengerData.row
}
######end of sibSp prob calculation######


#########parch Probability##########
parchProb=rep(0,8)#0-6, last one is reserverd for exceptions

for (i in 1: 7){

  parchProb[i] = (length(which(passengerData[8] == i-1)) + Laplace) / passengerData.row
}
parchProb[8] = 1

######end of parch prob calculation######

#########fare Probability##########
#todo this is floating data
fareProb = 0
######end of fare prob calculation######

#########embarked Probability##########
embarkedProb = rep(0,3) #C,Q,S

embarkedProb[1] = (length(which(passengerData[12] == "C"))) / passengerData.row
embarkedProb[2] = (length(which(passengerData[12] == "Q"))) / passengerData.row
embarkedProb[3] = (length(which(passengerData[12] == "S"))) / passengerData.row

######end of embarked prob calculation######


################################
########Conditional Prob########
################################

######pclass probability#########
pclassProbAlive = rep(0,3)
for (i in which(passengerData[2]==1)) { # for index in alive 
  pclassProbAlive[passengerData[i,3]] = pclassProbAlive[passengerData[i,3]] + 1
}
for (i in 1:3)
{
  # +1 for Laplace Smoothing
  pclassProbAlive[i] = ((pclassProbAlive[i]+Laplace) / passengerData.row) / aliveProb #conditional prob.
}

######end of pclass prob calculation######

######sex probability#######
sexProbAlive = rep(0,2) #1 for male, 2 for female
for (i in which(passengerData[2]==1)) {
  if (as.character(passengerData[i,5]) == "male") {
    sexProbAlive[1] = sexProbAlive[1] + 1
  }
  else {
    sexProbAlive[2] =  sexProbAlive[2] + 1
  }
}
# +1 for Laplace Smoothing
sexProbAlive[1] = ((sexProbAlive[1]+Laplace) / passengerData.row) / aliveProb
sexProbAlive[2] = ((sexProbAlive[2]+Laplace) / passengerData.row) / aliveProb
######end of sex prob calculation######

######age probability#######
ageProbAlive=rep(0,9)
for (i in 0:7) {
    for (j in which(passengerData[6]>0)) {
        if ((passengerData[j,2]==1) &&(passengerData[j,6] > i*10) && (passengerData[j,6] <= (i+1)*10)) {
            #i*10< age < (i+1)*10
            ageProbAlive[i+1] = ageProbAlive[i+1] + 1
            }
        }
        ageProbAlive[i+1] = ((ageProbAlive[i+1] + Laplace) / length(which(passengerData[6]>0))) / aliveProb
}
ageProbAlive[9] = 1 # reserved for NA


######end of age prob calculation######

######sibSp probability#######
sibProbAlive=rep(0,9) #0-8
for (i in which(passengerData[2]==1)) {
  sibProbAlive[passengerData[i,7]+1] = sibProbAlive[passengerData[i,7]+1] + 1
}

for (i in 1: 9){
  # +1 for Laplace Smoothing
  sibProbAlive[i] = ((sibProbAlive[i]+Laplace) /  passengerData.row) / aliveProb
}
######end of sibSp prob calculation######


#########parch Probability##########
parchProbAlive=rep(0,8)#0-6, last one is reserverd for exceptions
for (i in which(passengerData[2]==1)) {
  parchProbAlive[passengerData[i,8]+1] = parchProbAlive[passengerData[i,8]+1] + 1
}

for (i in 1: 7){
  # +1 for Laplace Smoothing
  parchProbAlive[i] = ((parchProbAlive[i]+Laplace) / passengerData.row) / aliveProb
}
parchProbAlive[8] = 1

######end of parch prob calculation######

#########fare Probability##########
#todo this is floating data
fareProb = 0
######end of fare prob calculation######

#########embarked Probability##########
embarkedProbAlive = rep(0,3) #C,Q,S
for (i in which(passengerData[2]==1)) {
  if (as.character(passengerData[i,12]) == "C") {
    embarkedProbAlive[1] = embarkedProbAlive[1] + 1
  }
  else if (as.character(passengerData[i,12]) == "Q")
  {
    embarkedProbAlive[2] = embarkedProbAlive[2] + 1
  }
  else
  {
    embarkedProbAlive[3] = embarkedProbAlive[3] + 1
  }
}
# +1 for Laplace Smoothing
embarkedProbAlive[1] = ((embarkedProbAlive[1]+Laplace) /passengerData.row) / aliveProb
embarkedProbAlive[2] = ((embarkedProbAlive[2]+Laplace) / passengerData.row) / aliveProb
embarkedProbAlive[3] = ((embarkedProbAlive[3]+Laplace) / passengerData.row) / aliveProb

######end of embarked prob calculation######


############Load Test data############
testData= read.csv("../Data/test.csv")
testData.row = nrow(testData)
#############end of loading###########

######Applying Naive Bayes######
Survived = rep(0,testData.row)
for (i in 1:testData.row) {
  #loading attributes
  pclass = testData[i,2]

  age = 9
  for (j in 0:7) {
    if (is.na(testData[i,6])) {
        age = 9
        }

    if ((testData[i,2]==1) && (testData[i,6] > j*10) && (testData[i,6] <= (j+1)*10)) {
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
    parch = 8 # ignore this attribute
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
  #end of attribute loading
  #predict live prob.
  p = (pclassProbAlive[pclass]*sexProbAlive[sex]*sibProbAlive[sibSp]*ageProbAlive[age]*parchProbAlive[parch]*embarkedProbAlive[embarked]*aliveProb)/(pclassProb[pclass]*sexProb[sex]*ageProb[age]*parchProb[parch]*sibProb[sibSp]*embarkedProb[embarked])
#   Survived[i] = p
  if (p <= 0.5) {# if predicted probability of alive <= 0.5
    Survived[i] = 0
  }
  else {
    Survived[i] = 1
  }
}

###########Export result############
passengerID = testData[1]
ans = data.frame(passengerID,Survived)
write.table(ans, file = "../Data/prediction.csv", quote = FALSE, sep = ",",row.names=FALSE)