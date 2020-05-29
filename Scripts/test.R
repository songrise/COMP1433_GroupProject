###testing for Naive Bayes####

######loading data#######

passengerData = read.csv("../Data/train.csv")
passengerData.row = nrow(passengerData) #Number of entries in passengerData.
passengerData.column = ncol(passengerData)
#######end of loading data########


#####alive probability######
aliveProb = length(which(passengerData[2] == 1))/passengerData.row
#######end of alive prob ########

######pclass probability#########
pclassProb = rep(0,3)
for (i in 1:passengerData.row) {
    pclassProb[passengerData[i,3]] = pclassProb[passengerData[i,3]] +  passengerData[i,2]
}
for (i in 1:3)
{
  # +1 for Laplace Smoothing
  pclassProb[i] = ((pclassProb[i]+1) / length(which(passengerData[3] == i)))
}

######end of pclass prob calculation######

######sex probability#######
sexProb = rep(0,2) #1 for male, 2 for female
for (i in 1:passengerData.row) {
    if (as.character(passengerData[i,5]) == "male") {
       sexProb[1] = sexProb[1] + passengerData[i,2]
    }
    else {
       sexProb[2] =  sexProb[2] + passengerData[i,2]
    }
}
  # +1 for Laplace Smoothing
sexProb[1] = ((sexProb[1]+1) / length(which(passengerData[5] == "male")))
sexProb[2] = ((sexProb[2]+1) / length(which(passengerData[5] == "female")))
######end of sex prob calculation######

######age probability#######
#todo this is floating data
ageProb=0
######end of age prob calculation######

######sibSp probability#######
sibProb=rep(0,9) #0-8
for (i in 1:passengerData.row) {
  sibProb[passengerData[i,7]+1] = sibProb[passengerData[i,7]+1] + passengerData[i,2]
}

for (i in 1: 9){
  # +1 for Laplace Smoothing
  sibProb[i] = (sibProb[i]+1) /  length(which(passengerData[7] == i-1))
}
######end of sibSp prob calculation######


#########parch Probability##########
parchProb=rep(0,8)#0-6, last one is reserverd for exceptions
for (i in 1:passengerData.row) {
  parchProb[passengerData[i,8]+1] = parchProb[passengerData[i,8]+1] + passengerData[i,2]
}

for (i in 1: 7){
  # +1 for Laplace Smoothing
  parchProb[i] = (parchProb[i]+1) /  length(which(passengerData[8] == i-1))
}
parchProb[8] = 1

######end of parch prob calculation######

#########fare Probability##########
#todo this is floating data
fareProb = 0
######end of fare prob calculation######

#########embarked Probability##########
embarkedProb = rep(0,3) #C,Q,S
for (i in 1:passengerData.row) {
  if (as.character(passengerData[i,12]) == "C") {
    embarkedProb[1] = embarkedProb[1] + passengerData[i,2]
  }
  else if (as.character(passengerData[i,12]) == "Q")
  {
    embarkedProb[2] = embarkedProb[2] + passengerData[i,2]
  }
  else
  {
    embarkedProb[3] = embarkedProb[3] + passengerData[i,2]
  }
}
# +1 for Laplace Smoothing
embarkedProb[1] = ((embarkedProb[1]+1) / length(which(passengerData[12] == "C")))
embarkedProb[2] = ((embarkedProb[2]+1) / length(which(passengerData[12] == "Q")))
embarkedProb[3] = ((embarkedProb[3]+1) / length(which(passengerData[12] == "S")))

######end of embarked prob calculation######



#prob = data.frame(pclassProb,sexProb,sibProb,parchProb,embarkedProb,)

#########TODO: calculate conditional prob P(pclass=..|alive) ....########
######pclass probability#########
pclassProbAlive = rep(0,3)
for (i in which(passengerData[2]==1)) { # for index in alive 
  pclassProbAlive[passengerData[i,3]] = pclassProbAlive[passengerData[i,3]] +  passengerData[i,2]
}
for (i in 1:3)
{
  # +1 for Laplace Smoothing
  pclassProbAlive[i] = ((pclassProbAlive[i]+1) / passengerData.row) / aliveProb #conditional prob.
}

######end of pclass prob calculation######

######sex probability#######
sexProbAlive = rep(0,2) #1 for male, 2 for female
for (i in which(passengerData[2]==1)) {
  if (as.character(passengerData[i,5]) == "male") {
    sexProbAlive[1] = sexProbAlive[1] + passengerData[i,2]
  }
  else {
    sexProbAlive[2] =  sexProbAlive[2] + passengerData[i,2]
  }
}
# +1 for Laplace Smoothing
sexProbAlive[1] = ((sexProbAlive[1]+1) / passengerData.row) / aliveProb
sexProbAlive[2] = ((sexProbAlive[2]+1) / passengerData.row) / aliveProb
######end of sex prob calculation######

######age probability#######
#todo this is floating data
ageProb=0
######end of age prob calculation######

######sibSp probability#######
sibProbAlive=rep(0,9) #0-8
for (i in which(passengerData[2]==1)) {
  sibProbAlive[passengerData[i,7]+1] = sibProbAlive[passengerData[i,7]+1] + passengerData[i,2]
}

for (i in 1: 9){
  # +1 for Laplace Smoothing
  sibProbAlive[i] = ((sibProbAlive[i]+1) /  passengerData.row) / aliveProb
}
######end of sibSp prob calculation######


#########parch Probability##########
parchProbAlive=rep(0,8)#0-6, last one is reserverd for exceptions
for (i in which(passengerData[2]==1)) {
  parchProbAlive[passengerData[i,8]+1] = parchProbAlive[passengerData[i,8]+1] + passengerData[i,2]
}

for (i in 1: 7){
  # +1 for Laplace Smoothing
  parchProbAlive[i] = ((parchProbAlive[i]+1) /   passengerData.row) / aliveProb
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
    embarkedProbAlive[1] = embarkedProbAlive[1] + passengerData[i,2]
  }
  else if (as.character(passengerData[i,12]) == "Q")
  {
    embarkedProbAlive[2] = embarkedProbAlive[2] + passengerData[i,2]
  }
  else
  {
    embarkedProbAlive[3] = embarkedProbAlive[3] + passengerData[i,2]
  }
}
# +1 for Laplace Smoothing
embarkedProbAlive[1] = ((embarkedProbAlive[1]+1) /passengerData.row) / aliveProb
embarkedProbAlive[2] = ((embarkedProbAlive[2]+1) / passengerData.row) / aliveProb
embarkedProbAlive[3] = ((embarkedProbAlive[3]+1) / passengerData.row) / aliveProb

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
  p = (pclassProbAlive[pclass]*sexProbAlive[sex]*sibProbAlive[sibSp]*parchProbAlive[parch]*embarkedProbAlive[embarked]*aliveProb)/(pclassProb[pclass]*sexProb[sex]*parchProb[parch]*sibProb[sibSp]*embarkedProb[embarked])
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