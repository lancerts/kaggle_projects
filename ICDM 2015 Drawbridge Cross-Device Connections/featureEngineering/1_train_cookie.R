## Read training set
train <- read.csv("../data/original/dev_train_basic.csv")
## Read cookie data
cookie <- read.csv("../data/original/cookie_all_basic.csv")
## merge by drawbridge_handle
system.time(train_cookie <- merge(train, cookie, by="drawbridge_handle"))
##   user  system elapsed 
## 12.485   0.020  12.567
object.size(train_cookie) /1024/1024
## 169.103309631348 bytes
## rename the columns
names(train_cookie) <- c("drawbridge_handle", "device_id", "device_type", "device_os", "country.train", "c0.train", "c1.train", "c2.train", "5.train", "6.train", "7.train", "cookie_id", "computer_os_type", "computer_browser_version", "country.cookie", "c0.cookie", "c1.cookie", "c2.cookie", "5.cookie", "6.cookie", "7.cookie")
## save it
write.csv(train_cookie, "../data/customized/1_train_cookie.csv", row.names=F)

