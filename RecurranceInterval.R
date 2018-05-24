
library(devtools)
library(dataRetrieval)

#  to get help on each function - ?readNWISuv
# stat list: 00060 - discharge, 00065 - gage height, 00010 - temp, 00094 - spC, 00400 pH, 00001 - max, 00002 - min, 
#00003 - mean, 00008 median


#plug in site number or parameter code and start/end date
siteNo <- "07374000"
pCode <- c("00060" ,"00065")
start.date <- "2004-03-17"
end.date <- "2017-12-31"

miss <-readNWISuv(siteNumbers = siteNo, parameterCd = pCode,startDate = start.date,endDate = end.date)
names(miss)
colnames(miss)<- c("ag", "site", "dateTime","flow", "flcd", "stage","stagecd", "tz")


flow<-data.frame(sort(miss$flow, decreasing=TRUE))
#flow.ix<-order(miss$flow) #index of low to high 

flow.uq<-unique(flow)
rank<- 1:912
flow.rank<-data.frame(flow.uq, rank)

colnames(flow) <-"miss.flow"
colnames(flow.rank) <-c("miss.flow", "rank")

flow2<- merge(flow, flow.rank, by="miss.flow")

flow2$exd<- flow2$rank/418885
flow2$recInt<-1/flow2$exd

#exceedence probability 
plot(unique(flow2$miss.flow), unique(flow2$recInt), typ="l")
plot(unique(flow2$miss.flow), unique(flow2$exd), typ="l")
plot(miss$stage, miss$flow)


stage<-1:45
con<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)


mod1<-lm(miss$flow[miss$stage < 30] ~ miss$stage[miss$stage < 30])
mod2<-lm(miss$flow[miss$stage >= 30] ~ miss$stage[miss$stage >= 30])

ans<-predict(mod2, 30:45)
