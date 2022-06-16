# 100 replications with each replication having 10 trials. In each trial, outcome is 0 or 1
oneRep <- rep(NA,10)
container <- t(replicate(100,oneRep))
for (i in 1:100) {
  for (j in 1:10){
    container[i,j] <- sample(0:1,1)
  }
}
# calculate number of head in each replication and save the result in the dis100Rep vector
dis100Rep <- rowSums(container)
# create a dataframe
repLabel <- 1:100
simulatedDis <- cbind(repLabel,dis100Rep)
#simulatedDis$group <- 
simulatedDisM <- as.data.frame(simulatedDis)
# creat two groups(head number below 2 and above 2)
simulatedDisM$group <- dis100Rep>2
simulatedDisM$group[simulatedDisM$group==TRUE] <- "less and equal to two heads"
simulatedDisM$group[simulatedDisM$group==FALSE] <- "more than two heads"
# plot the simulated distribution in a dot plot. From the output, 33 out of 100 replications have 5 heads.
library(ggplot2)
ggplot(simulatedDisM, aes(x = dis100Rep,color=group,fill=group)) +
  geom_dotplot(method="histodot",binwidth = 0.2)+
  theme(
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )+
  scale_x_continuous(breaks=c(1:10))+
  labs(x="Number of Head")
# create a vector representing count of values in the dis100Rep vector
countSummary <- rep(NA,10)
for (i in 1:10){
countSummary[i] <- sum(dis100Rep==i)
}
# verify that the total number of replication is 100
sum(countSummary)
