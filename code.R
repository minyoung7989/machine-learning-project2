####ROC
dat <- read.csv("anesthetic.csv")
library(pROC)

roc1 <- roc(dat$nomove~dat$conc, data = dat)
plot(roc1)

coords(roc1, "best", ret=c("threshold", "specificity", "sensitivity"))
abline(v=0.7142857, col='red')
abline(h=0.875, col='blue')

###로지스틱
aa <- glm(dat$nomove~dat$conc, family = binomial)
summary(aa)

as.numeric(dat$nomove)
str(dat$nomove)
gp <- predict(aa, type = "response")
gp <- ifelse(gp<0.5,0,1)
table(gp, Actual = dat$nomove)

###svm
library(e1071)

train <- sample(1:30, 20)
classifier <- svm(dat$nomove ~., data = dat, subset = train, type = "C-classification")
predict(classifier, dat[-train,])

testtable <- table(dat$nomove[-train], predict(classifier, dat[-train,]))
testtable

sum(testtable[row(testtable) == col(testtable)])/sum(testtable) #####정분류율
1-sum(testtable[row(testtable) == col(testtable)])/sum(testtable) #####오분류울

###KNN
library(class)
library(gmodels)
table(dat$nomove)
rs <- sample(2, nrow(dat), replace = TRUE, prob = c(0.67,0.33))

dat.train <- dat[rs == 1, 1]
dat.trainla <- dat[rs == 1, 2]

dat.test <- dat[rs == 2, 1]
dat.testla <- dat[rs == 2, 2]

model <- knn(data.frame(dat.train), data.frame(dat.test), dat.trainla, k =3)
table(model, dat.testla)

CrossTable(dat.testla, model, prop.chisq = F)


#######################################################
dat2 <- read.csv("crabs.csv")


roc2 <- roc(dat2$satell~dat2$weight, data = dat2)
plot(roc2)

coords(roc2, "best", ret=c("threshold", "specificity", "sensitivity"))
abline(v= 0.9032258, col='red')
abline(h=0.4774775, col='blue')

### 로지스틱
bb <- glm(dat2$satell~., data = dat2, family = binomial)
summary(bb)

as.numeric(dat2$satell)
str(dat2$satell)
gp <- predict(bb, type = "response")
gp <- ifelse(gp<0.5,0,1)
table(gp, dat2$satell)

#### svm
train2 <- sample(1:173, 140)
classifier2 <- svm(dat2$satell ~., data = dat2, subset = train2, type = "C-classification")
predict(classifier2, dat2[-train2,])

testtable2 <- table(dat2$satell[-train2], predict(classifier2, dat2[-train2,]))
testtable2

sum(testtable2[row(testtable2) == col(testtable2)])/sum(testtable2) #####dat
1-sum(testtable2[row(testtable2) == col(testtable2)])/sum(testtable2) #####오분류울

### knn
table(dat2$satell)
rs2 <- sample(2, nrow(dat2), replace = TRUE, prob = c(0.67,0.33))

dat.train2 <- dat2[rs2 == 1, 1:3]
dat.trainla2 <- dat2[rs2 == 1, 4]

dat.test2 <- dat2[rs2 == 2, 1:3]
dat.testla2 <- dat2[rs2 == 2, 4]

model2 <- knn(data.frame(dat.train2), data.frame(dat.test2), dat.trainla2, k =3)
table(model2, dat.testla2)

CrossTable(dat.testla2, model2, prop.chisq = F)
