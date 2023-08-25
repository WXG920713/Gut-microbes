library(dplyr)
library(tidyr)
library(mice)
library(Hmisc)

data<-read.delim('data.txt',header=T,row.names=1,sep = '\t',stringsAsFactors = F, check.names = F) 

res2<-rcorr(as.matrix(data))

write.table(res2$r,"corr.csv",row.names=T,col.names=T,sep=",")



