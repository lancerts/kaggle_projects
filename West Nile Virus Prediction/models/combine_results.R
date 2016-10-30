x1 <- fread("submission/train4B_test4B_gam222.csv")
x2 <- fread("submission/train4B_test4B_gam22.csv")
prob=(x1$WnvPresent+x2$WnvPresent)/2
summary(prob)
submissionFile<-cbind(test$Id,prob)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B_gam222_gam22.csv",row.names=FALSE,quote=FALSE)
