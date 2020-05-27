###testing for Naive Bayes####

######loading data#######

passengerData = read.csv("../Data/train.csv")
passengerData.row = nrow(passengerData) #Number of entries in passengerData.
passengerData.column = ncol(passengerData)
#######end of loading data########


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
