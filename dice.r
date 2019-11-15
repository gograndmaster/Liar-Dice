#use to accumulat challenge success rate in Liar Dice
#create by lyp in 2019-11-15


# [LIBRARY] ---------------------------------------------------------------

library(ggplot2)


# [DEF FUNCTION] ----------------------------------------------------------
FtDice=function(playersNum,diceNum,bidPrevious,yourSpecificNum){
  otherDice=(playersNum-1)*diceNum
  otherBidPrevius=bidPrevious-yourSpecificNum
  challengeSuccessRate=pbinom(otherBidPrevius,otherDice,1/6)
  print(paste0("ChallengeSuccessRate=",challengeSuccessRate))
  distributionDiagram=barplot(dbinom(0:otherDice,otherDice,1/6),names=0:otherDice,xlab='',ylab='P(x)')
  print(distributionDiagram)
}

FtDice2=function(playersNum,diceNum,bidPrevious,yourSpecificNum){
  otherDice=(playersNum-1)*diceNum
  otherBidPrevius=bidPrevious-yourSpecificNum
  challengeSuccessRate=pbinom(otherBidPrevius,otherDice,1/6)
  distributionDF=data.frame(num=c(0:otherDice))
  distributionDF$prate=1-pbinom(distributionDF$num,otherDice,1/6)
  distributionDF$drate=dbinom(distributionDF$num,otherDice,1/6)
  succRatePlot=ggplot(distributionDF,aes(x=num,y=drate))+
    geom_bar(stat="identity")
  print(challengeSuccessRate)
  print(succRatePlot)
}
