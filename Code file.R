#R Business Analytics Case Study 2
#Linear Regression on Bank Data

getwd()

#Importing Dataset
require("readxl")
mydata <- read_xlsx("Linear Regression Case.xlsx")

#Some Feature Engineering
mydata$Total_Spent <- mydata$cardspent + mydata$card2spent

mydata <- within(mydata,rm(cardspent,card2spent,carditems,card2items)) #Removing the other 2 variables

#Removing ID variable
mydata<- within(mydata,rm(custid))

#Removing birthmonth variable from mydata for the meanwhile
birthmonth <- mydata$birthmonth
mydata <- within(mydata,rm(birthmonth))

#Descriptive statistics Function
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

#Applying the function to get the descriptve statistics
Diag_stats <- data.frame(t(apply(mydata,2,mystats)))


#Exporting Descriptive statistics
write.csv(Diag_stats,"Diag_stats.csv")

#-------------------------------------DATA PREPARATION-----------------------------------------------#

#Outlier Treatment
#Outlier's treatment 
#Outliers - capping at 95th,5th percentile
#Creating function
Out_fun <- function(x){
  quantiles = quantile(x,c(0.95,0.05),na.rm =T)
  x[x > quantiles[1]] = quantiles[1]
  x[x < quantiles[2]] = quantiles[2]
  return(x)
}

#Applying the function on our data
mydata <- data.frame(apply(mydata, 2, Out_fun))

#Checking data
Diag_stats2<-t(data.frame(apply(mydata, 2, mystats)))

#Missing Values Treatment
colSums(is.na(mydata))

#Removing variables with more than 10% Missing Values
which(colSums(is.na(mydata)/nrow(mydata)*100)> 10)

rmvars <- c("lntollmon","lntollten","lnequipmon","lnequipten","lncardmon","lncardten",
                                    "lnwiremon","lnwireten")

mydata <- mydata[,-which(names(mydata) %in% rmvars)]

colSums(is.na(mydata))

#Imputing the rest missing values
require(Hmisc)
mydata$townsize<- impute(mydata$townsize,mean)
mydata$lncreddebt <- impute(mydata$lncreddebt,mean)
mydata$lnothdebt <- impute(mydata$lnothdebt,mean)
mydata$commutetime <- impute(mydata$commutetime,mean)
mydata$longten <- impute(mydata$longten,mean)
mydata$lnlongten <- impute(mydata$lnlongten,mean)
mydata$cardten <- impute(mydata$cardten,mean)

mydata2 <- cbind(mydata,Birthmonth = birthmonth)
rm(birthmonth)

#Checking for normality of dependent variable
hist(mydata2$Total_Spent)

#Converting it using logarithmic method
hist(log(mydata2$Total_Spent))
mydata2$ln_Total_spent <- log(mydata2$Total_Spent)

#----------------------Variable Reduction---------------------!

#Performing ANOVA Test
anov= aov(formula = ln_Total_spent ~ ., data = mydata2)
summary(anov)

#Selecting only significant Categorical variables from ANOVA TEST 

selec_vars <- c("region",	"townsize","gender","age","agecat",
                "ed","union","employ","empcat","retire", 
                "lninc","debtinc","creddebt","lncreddebt","othdebt",
                "lnothdebt","jobsat","marital","spoused","spousedcat",
                "reside","pets","pets_birds","hometype","addresscat","carvalue",
                "carbought","carown","commutecat","commutetime","commutecarpool",
                "commutepublic","commutebike","commutenonmotor","reason",
                "polview","card","cardtenurecat","card2","card2benefit",
                "card2fee","tenure","churn","lnlongmon","longten",
                "tollten","equipten","cardmon","cardten","wiremon",
                "wireten","voice","internet","callid","ebill","hourstv","ownfax",
                "hourstv","owndvd","response_03","ln_Total_spent")


mydata3 <- mydata2[selec_vars]

#Running StepAIC for further variable reduction
require(MASS)
require(car)
fit <- lm(ln_Total_spent ~ .,data = mydata3)

step1 <- stepAIC(fit,direction="both")

step1$anova

#---------------Independent variables Selected after Step AIC--------------------- 

#Selecting variables after stepwise reduction
selec_vars2 <- c("region","gender","age","debtinc","lninc","lncreddebt","othdebt",
                 "jobsat","marital","spoused","spousedcat","pets_birds",
                 "commutecarpool","card","cardtenurecat","card2","card2fee","longten","cardmon",
                 "wireten","voice","owndvd","internet","ebill",
                 "response_03","Total_Spent","ln_Total_spent")


#Creating a new dataframe with selected variables from step process including "Total_Spent"
mydata4 <- mydata2[selec_vars2]

#Converting Categorical variables to factor
catvars<- c("region","gender","jobsat","marital",
            "spousedcat","pets_birds","commutecarpool","card","cardtenurecat",
            "card2","card2fee","voice","owndvd","internet","ebill","response_03")

mydata4[catvars] <- data.frame(apply(mydata4[catvars],2,as.factor))#Factor conversion

str(mydata4)

#Transforming other continuous variables in dataset
mydata4$ln_debtinc <- sqrt(mydata4$debtinc)
mydata4$lnothdebt <- log(mydata4$othdebt)
mydata4$lnlongten <- log10(mydata4$longten)

#Removing "Total Spent" as it has a transformed part already present
mydata4 <- within(mydata4,rm(Total_Spent))
#------------------------------Modelling-------------------------------------#
set.seed(1234)
#Splitting dataset into training and testing
train_ind <- sample(1:nrow(mydata4),size = floor(0.70*nrow(mydata4)))

training <- mydata4[train_ind,]
testing <- mydata4[-train_ind,]

names(training)

require(caret)
require(car)

fit1 <- lm(ln_Total_spent~.,data = training)
summary(fit1)

#Calculating cook's distance
training$cd <- cooks.distance(fit1) 
training1<-subset(training, cd< (4/3500))
  
#Running the regression model again
fit2 <- lm(ln_Total_spent~.,data = training1)

summary(fit2)

#Running Step AIC 
step2 <- stepAIC(fit2,direction = 'both')

step2$anova


fit3 <- lm(ln_Total_spent ~ region + gender + age + lninc + spoused + spousedcat + 
             pets_birds + commutecarpool + card + cardtenurecat + card2 + 
             card2fee + longten + cardmon + wireten + voice + owndvd + 
             internet + ebill + response_03 + lnlongten,data = training1)


summary(fit3)

vif(fit3)

#Taking fit3 as our final model
write.csv(data.frame(summary(fit3)$coefficients), file= "final_coeff.csv")

#------------------------SCORING USING PREDICT FUNCTION--------------------------------
#TRAINING SET
t1 <-cbind(training1,Predicted_Spent = exp(predict(fit3)),Total_Spend = exp(training1$ln_Total_spent))
t1<- transform(t1, APE = abs(Predicted_Spent - Total_Spend)/Total_Spend)

mean(t1$APE)
View(t1)
names(t1)

#Removing cook's distance calculated in new training dataset
t1 <- within(t1,rm(cd))


#TESTING SET
t2 <-cbind(testing, predicted_spent=exp(predict(fit3,testing)),Total_Spend = exp(testing$ln_Total_spent))

t2<- transform(t2, APE = abs(predicted_spent - Total_Spend)/Total_Spend)

mean(t2$APE)
names(t2)

#------------------------DECILE ANALYSIS-----------------------------------------
#Find the decile locations 
#TRAINING SET
decLocations <- quantile(t1$Predicted_Spent, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$Predicted_Spent,c(-Inf,decLocations, Inf))

require(sqldf)
t1_DA <- sqldf("select decile, count(decile) as count, avg(Predicted_Spent) as AVG_PREDICTED_SPENT,   
               avg(Total_Spend) as AVG_ACTUAL_SPENT
               from t1
               group by decile
               order by decile desc")

View(t1_DA)
write.csv(t1_DA,"Training_DecileAnalysis.csv")

#TESTING SET
#Find the decile locations 
decLocations <- quantile(t2$predicted_spent, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$predicted_spent,c(-Inf,decLocations, Inf))

require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(predicted_spent) as AVG_PREDICTED_SPENT,   
               avg(Total_Spend) as AVG_ACTUAL_SPENT
               from t2
               group by decile
               order by decile desc")

View(t2_DA)
write.csv(t2_DA,"Testing_DecileAnalysis.csv")
#-----------------------------------------END--------------------------------------------------#