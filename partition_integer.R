install.packages(c("partitions", "iterpc"))
library(partitions)
library(iterpc)
C = t(restrictedparts(100,5, include.zero=TRUE))
nrow(C)

filterC<-C[-c(which(C[,5]>floor((100-1)/20)+1),which(C[,4]>floor((100-1)/5)+1), which(C[,3]>floor((100-1)/5)+1),
        which(C[,2]>floor((100-1)/1)+1), which(C[,1]>floor((100-1)/1)+1)),]
nrow(filterC)

newfilterC<-filterC[-which(apply(filterC, 1, function(x){x[2]*1+x[3]*5+x[4]*5+x[5]*7})>100),]
nrow(newfilterC)

newfilterC<-newfilterC[-which(apply(newfilterC, 1, function(x){x[2]*0+x[3]*4+x[4]*4+x[5]*6})<x[1]),]
nrow(newfilterC)

C<-newfilterC
constraints<-import_constraints(path="C:/Users/Benjamin Delory/Documents/GitHub/heathland/inst/extdata")






