install.packages("psych")
install.packages("psy")
install.packages("GPArotation")
install.packages("mice")
install.packages("missForest")
install.packages("VIM")
library(psy)
library(psych)
library(GPArotation)
library(stats)
library(VIM)
library(mice)
library(missForest)
x <-read.csv("C:\\Users\\sudee\\Desktop\\R\\1519386741_Toothpaste_svy.csv")
boxplot(x,horizontal = TRUE)

x[x>7]=NA
x[x<0]=NA
summary(x)
boxplot(x,horizontal = TRUE)

md.pattern(x)
par(mfrow=c(1,2))
m_p <-aggr(x, col=c('yellow','red'),numbers=TRUE,sortVars=TRUE,lables=names(x),cex.axis=.7,
           gap=3, ylab=c("missing data","pattern"))

summary(impute)
is.na(x)
x1<-x[c(1:151),]
impute <- mice(x1, m=3, maxit= 20)


compdata <- complete(impute,1)
compdata1 <-complete(impute,2)
compdata2 <-complete(impute,3)

summary(compdata)
boxplot(compdata, horizontal = TRUE)
h<- hist(x$StrongGum,breaks = c(0,1,2,3,4,5,6,7),plot = TRUE,freq = TRUE,col = 'cyan',main = paste("Histogram of the survey elements",lables= TRUE))

cor(compdata)
cor(compdata1)
cor(compdata2)



n.factors = 2
fact1<- factanal(compdata, n.factors , rotation="quartimax", scores="regression")
fact1
fact1$loadings
fact2<- fact1$loadings[,1:2]
fact2
par(mfrow=c(1,1))
plot(fact2,type="n")
text(fact2,labels=names(x),cex=.7)
fact1$scores
