#reading the myopia.csv file
myopia <- read.csv("D:\\Users\\astar\\Desktop\\STATISTICS\\f2821907\\myopia.csv",sep=";",header = TRUE)

#removing the id column, we will not need it anymore
myopia <- myopia[-1]
str(myopia)
#View(myopia)

#putting mommy and dadmy in a variable parentsmy
for( i in 1:nrow(myopia))
{
	if(myopia$MOMMY[i] == 1 | myopia$DADMY[i] == 1)
      { 
            myopia$PARENTSMY[i] = 1
      }else{
	  myopia$PARENTSMY[i] = 0
      }
}

str(myopia)
#deleting mommy and dadmy variables
myopia$MOMMY <- NULL
myopia$DADMY <- NULL

#defind factor columns
cols_f <- c("MYOPIC", "GENDER", "PARENTSMY")
#converting variables into factor variables
myopia[cols_f] <- lapply(myopia[cols_f], as.factor)  

#defind numeric columns
cols_n <- c("STUDYYEAR","AGE","SPORTHR","READHR","COMPHR","STUDYHR","TVHR","DIOPTERHR")
#converting variables into numeric variables
myopia[cols_n] <- lapply(myopia[cols_n], as.numeric)  
sapply(myopia, class)

#getting the numeric variables
require(psych)
index <- sapply(myopia, class) == "numeric"
num <- myopia[,index]
str(num)


par(mfrow=c(3,3))
#Visual Analysis for numerical variables
for( i in 1:7)
{
      hist(num[,i], main=names(num)[i])
}

n <- nrow(num)
#Visual Analysis for factor variables
fac <- myopia[,!index]
par(mfrow=c(1,1))
barplot(sapply(fac,table)/n, horiz=T, las=1, col=2:3, ylim=c(0,8), cex.names=0.6)
legend('top', fil=2:3, legend=c('0','1'), ncol=2, bty='n',cex=1.5)

#finding out correlations in graph
require(corrplot)
corrplot(cor(num))
corrplot(cor(num), method = "number") 


#stepwise for variable selection
myopic_step <- glm(MYOPIC~.,data = myopia, family = "binomial")
step(myopic_step, direction = "backward", k=log(500))


# Lasso with glmnet
library(glmnet)
lasso_m <- model.matrix(MYOPIC~., myopia)[,-1]
lambdas <- 10 ^ seq(8,-4,length=200)

# cross validation
LASSO.cv <- cv.glmnet(lasso_m,myopia$MYOPIC, alpha=1, lambda=lambdas, family="binomial")
#coef(LASSO.cv, s = "lambda.min")
coef(LASSO.cv, s = "lambda.1se")

#final model
final_model <- glm(MYOPIC~SPHEQ+PARENTSMY+SPORTHR,data=myopia, family = "binomial")
summary(final_model)

#checking for multicolinearity issue
vif(final_model)
#goodness of fit
with(pchisq(deviance, df.residual), data = final_model)







