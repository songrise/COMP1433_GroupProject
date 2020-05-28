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
parchProb=rep(0,7)#0-6
for (i in 1:passengerData.row) {
  parchProb[passengerData[i,8]+1] = parchProb[passengerData[i,8]+1] + passengerData[i,2]
}

for (i in 1: 7){
  # +1 for Laplace Smoothing
  parchProb[i] = (parchProb[i]+1) /  length(which(passengerData[8] == i-1))
}

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


############Load Test data############
testData= read.csv("../Data/test.csv")
testData.row = nrow(testData)
#############end of loading###########

######Applying Naive Bayes######
for (i in testData.row) {
  palive  = 0
}
