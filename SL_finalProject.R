setwd("C:/Users/emily/Documents/uni/fourth/ss3850/group")
#data retrieved from Kaggle
myData<-read.csv("heart.csv", header = TRUE)

#install.packages("leaps")
library(leaps)
#cleaning up the data to remove unwanted symbols and change the column headings
names(myData) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
summary(myData)
boxplot(myData)
myData$ca[myData$ca == "?"] <- NA
myData$thal[myData$thal == "?"] <- NA

#using forward selection to find the best model
fit_fwd<-regsubsets(trestbps~., data = myData, method = "forward")
fit_summary_f<-summary(fit_fwd) #the asterisks indicate the corresponding predictor is in the model
fit_summary_f

names(fit_summary_f) #returns the R-squared, rss etc. values

#use these values to pick the best model
#plotting BIC, Cp, RSS and adjusted R-squared
par(mfrow=c(2,2))
plot(fit_summary_f$rss, xlab="Number of Variables", ylab="RSS") 
#finding the minimum in RSS
which.min(fit_summary_f$rss)
points(8, fit_summary_f$rss[8], col = "blue", cex = 2, pch = 20)

plot(fit_summary_f$adjr2, xlab="Number of Variables", ylab="Adjusted R^2") 
#finding maximum of adjusted r-sqaared
which.max(fit_summary_f$adjr2)
points(8, fit_summary_f$adjr2[8], col = "blue", cex = 2, pch = 20)

plot(fit_summary_f$rsq, xlab="Number of Variables", ylab="R-Squared") 
which.max(fit_summary_f$rsq)
points(8, fit_summary_f$rsq[8], col = "blue", cex = 2, pch = 20)

plot(fit_summary_f$bic, xlab="Number of Variables", ylab="BIC") 
which.min(fit_summary_f$bic)
points(3, fit_summary_f$bic[3], col = "blue", cex = 2, pch = 20)

#from these graphs we can see that the best model includes all 8 predictors except 
#with BIC we have 3 predictors

#fit model with all 8 predictors
mod_8<-lm(trestbps~., data=myData)
summary(mod_8) #from this model we notice the p-values of only 3 predictors is significant (<0.05)

#model with 3 predictors
coef(fit_fwd, 3) #shows the estimates for the model with 3 predictors
mod_3<-lm(trestbps~age + fbs + oldpeak , data=myData)
summary(mod_3)


#COMPARE DIFFERENT REGRESSION MODELS
#install.packages("glmnet", dependencies=TRUE)
library(glmnet)

#Split the data into training and testing data
samp_tr<-sample(1:dim(myData)[1], dim(myData)[1]/2)
samp_ts<- -samp_tr

training<-myData[samp_tr,]
testing<-myData[samp_ts,]

#multiple linear regression
fit1<-lm(trestbps~age + fbs + oldpeak, data=training)
summary(fit1)
pred1<-predict(fit1,testing, type="response")
err1<-mean((pred1-testing$trestbps)^2)
print(paste("MLR MSE: ", err1)) 


mat_train<-model.matrix(trestbps~age + fbs + oldpeak,data=training)
mat_test<-model.matrix(trestbps~age + fbs + oldpeak,data=testing)
#lambda values
grid=10^seq(4,-2, length=100)

#ridge regression
ridge_fit<-glmnet(mat_train, training$trestbps, alpha=0, lambda=grid, thresh=1e-12)
cv_rid_mod<-cv.glmnet(mat_train, training$trestbps, alpha=0, lambda=grid, thresh=1e-12)

best_ridge<-cv_rid_mod$lambda.min
print(paste("Best ridge lambda: ", best_ridge))

pred_rid<-predict(ridge_fit, s=best_ridge, newx = mat_test)
err2<-mean((pred_rid-testing$trestbps)^2)
print(paste("Ridge MSE: ", err2)) 

#Lasso regression
lasso_fit<-glmnet(mat_train, training$trestbps, alpha=1, lambda=grid, thresh=1e-12)
cv_las_mod<-cv.glmnet(mat_train, training$trestbps, alpha=1, lambda=grid, thresh=1e-12)

best_las<-cv_las_mod$lambda.min
print(paste("Best lasso lambda: ", best_las))

pred_las<-predict(lasso_fit, s=best_las, newx = mat_test)
err3<-mean((pred_las-testing$trestbps)^2)
print(paste("Lasso MSE: ", err3)) 

#PLS regression
y.test=y[samp_ts]
y=myData$trestbps
require(pls)
set.seed(2)
pcr.fit=pcr(trestbps~age + fbs + oldpeak, data=myData , scale=TRUE , validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")
pcr.fit_1=pcr(trestbps~age+oldpeak+fbs, data=myData , subset=samp_tr, scale=TRUE , validation ="CV")
validationplot(pcr.fit_1 ,val.type="MSEP")
pcr.pred=predict(pcr.fit_1 ,myData[samp_ts ,], ncomp=1)
err4<-mean((pcr.pred -y.test)^2)
summary(pcr.fit_1)

#Pcr regression
set.seed(1)
pls.fit=plsr(trestbps~age+oldpeak+fbs, data=myData , subset=samp_tr , scale=TRUE ,validation ="CV")
summary (pls.fit)
validationplot(pls.fit, val.type ="MSEP")
pls.pred=predict (pls.fit ,myData[samp_ts ,],ncomp =1)
err5<-mean((pls.pred -y.test)^2)
#The highest R-squared value is from the multiple linear regression but the value is only 0.131 which
#is not very high, this means around 13% of the data can be represented by the model


#comparing the R-squared values between models
avg<-mean(testing$trestbps)
ls_R2<-1-(err1/mean((avg-testing$trestbps)^2))
rid_R2<-1-(err2/mean((avg-testing$trestbps)^2))
las_R2<-1-(err3/mean((avg-testing$trestbps)^2))
pcr_R2<-1-(err4/mean((avg-testing$trestbps)^2))
pls_R2<-1-(err5/mean((avg-testing$trestbps)^2))

print(paste("LS R-squared: ", ls_R2))
print(paste("ridge R-squared: ", rid_R2))
print(paste("lasso R-squared: ", las_R2))
print(paste("PCR R-squared: ", pcr_R2))
print(paste("PLS R-squared: ", pls_R2))
#install.packages("car")
library(car)
leveragePlots(fit1)
leveragePlots.glm(fit1)
