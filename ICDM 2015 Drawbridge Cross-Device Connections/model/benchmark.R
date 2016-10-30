library(data.table)
library(plyr)


dev_test_basic <- fread("../data/dev_test_basic.csv",stringsAsFactors =FALSE)
cookie_all_basic <- fread("../data/cookie_all_basic.csv",stringsAsFactors =FALSE)


myFun <- function(x){
    tbl <- table(x$cookie_id)
    x$freq <- rep(names(tbl)[which.max(tbl)],nrow(x))
    x
}


country_summary <- ddply(cookie_all_basic, .(country), .fun=myFun)
country_summary <- country_summary[,c(5,12)]


setkey(dev_test_basic,country)
country_summary <- unique(country_summary)
country_summary <- data.table(country_summary)
setkey(country_summary,country)


submission_country_popular_cookie <- merge(dev_test_basic,country_summary,all.x=TRUE)
submission_country_popular_cookie <- data.frame(submission_country_popular_cookie)
submission_country_popular_cookie <- submission_country_popular_cookie[,c("device_id","freq")]

names(submission_country_popular_cookie) <- c("device_id","cookie_id")
write.csv(submission_country_popular_cookie,file="../submission/benchmark_submission.csv",row.names=FALSE)




