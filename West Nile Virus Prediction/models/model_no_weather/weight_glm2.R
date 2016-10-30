
#x <- fread("data/train.csv")
y <- fread("data/train5.csv")

y1<-y[Year!=2011,]
ycv<-y[Year==2011,]


## fit a logistic regression model using just three key fields
fitCv2<-glm(WnvPresent ~  Week+Species + Block+Latitude+Longitude+NumMosquitos, data = y1, family = "binomial")  
coef(fitCv2) #weight=x1$NumMosquitos
