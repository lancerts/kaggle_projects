library(Metrics)
library(data.table) 
library(wavethresh)
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
pred <- fread("submission/GAMmean3.csv")

data=pred$WnvPresent
length(data)<-2^ceiling(log(length(pred$WnvPresent),2)) 
data[is.na(data)]<-0 
wds = wd(data, filter.number=10, family="DaubLeAsymm", type='station', bc='periodic', verbose=FALSE)



aC = accessC(wds, level=16)[1:length(pred$WnvPresent)]
psubmit=exp(aC)/5
plot(psubmit)

submissionFile<-cbind(pred$Id,psubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/GAMmean3_wavelet_lv16.csv",row.names=FALSE,quote=FALSE)


#aC[1:length(pred$WnvPresent)]-pred$WnvPresent

#plot(pred)
