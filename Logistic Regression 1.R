
# Logistic Regression for Bank X-Sell Product
######################################################

# getting some important packages for the analysis
install.packages("gmodels")
install.packages("Hmisc")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("car")
install.packages("caret")
install.packages("dplyr")
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)

# K - code
install.packages("glmulti")
library(glmulti)

cat("\014") # Clearing the screen

# Setting the working directory
setwd("C:/YYYYYY/AMMA/2017/Data/data_2017")
getwd()

# reading client datasets
df.client <- read.csv('bank_client.csv')
str(df.client)

# reading other attributes
df.attr <- read.csv('bank_other_attributes.csv')
str(df.attr)

# reading campaign data
df.campaign <- read.csv('latest_campaign.csv')
str(df.campaign)

# reading campaign outcome
df.campOutcome <- read.csv('campaign_outcome.csv')
str(df.campOutcome)

# Create campaign data by joining all tables together
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)
length(unique(df.data$Cust_id)) == nrow(df.data) #checking for any duplicate customer ID

# clearing out temporary tables
rm(df.temp1,df.temp2)

# see few observations of merged dataset
head(df.data)

########## K - code 

View(df.data)
#dd <- matrix(nc = 6, nr = 6)
#View(dd)
#help("glmulti")
#??glmulti
#??rma
install.packages("metafor")
library(glmulti)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre8')
install.packages("rJava")
install.packages("xlsx")
library(rJava)
library(metafor)

library(xlsx)
rma.glmulti <- function(formula, data, ...) {
  View(length(vi))
  rma(formula, vi, data=data, method="ML", ...)
}

    ### Test code ###
dat <- get(data(dat.bangertdrowns2004))
class(dat)
dat<-as.data.frame(dat)

res_test <- glmulti(yi ~ length + wic + feedback + info + pers + imag + meta, data=dat,
               level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)
View(dat)
    ### Test code End ###

df.data_final <- df.data
df.data_final$yact = ifelse(df.data$y == 'yes',1,0)
class(df.data_final)

df.data_final <- df.data_final[!apply(df.data_final[,c("age", "balance", "duration", "campaign", "pdays", "previous", "job","marital", "education", "default", "housing", "loan", "poutcome")], 1, anyNA),]
#df.data_final <- as.data.frame(df.data_final)
View(df.data_final)
result <- glmulti(yact ~ age + balance + duration + campaign + pdays + previous +
                 job + marital + education + default + housing + loan + poutcome, data=df.data_final,
               level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)
#testing one at a time
result_tentative <- glmulti(yact ~ age + balance + duration + campaign + pdays + previous +
                    job + marital + education + default + housing + loan + poutcome, data=df.data_final,
                  level=1, fitfunction=glm, crit="aicc", confsetsize=32, method = "d",marginality = FALSE)

#result_tentative <- glmulti(yact ~ age + balance + duration + campaign + pdays + previous +
#                    job + marital + education + default + housing + loan + poutcome, data=df.data_final,
#                  level=1, fitfunction=glm, crit="aicc", confsetsize=200, marginality = FALSE, maxK = 14)

rm(result_tentative)
result_tentative <- glmulti(yact ~ age + balance + duration + campaign + pdays + previous +
                                                  job + marital + education + default + housing + loan + poutcome, data=df.data_final,
                                              level=1, fitfunction=glm, crit="aicc", confsetsize=150, marginality = FALSE)

result_tentative

write(result_tentative, file = "Tentative_result.csv", ncolumns = if (is.character(Tentative_result)) 1 else 5, append = FALSE, sep = " ")

print(result_tentative)

plot(result_tentative)

  # top 10 models #
  tmp <- weightable(result_tentative)
  tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 1000,]
  tmp$aicc
  # top 10 models end #
  
  # summary of best model(s) #
  summary(result_tentative@objects[[1]])
  # summary of best model(s) end #
  
  # Variable importance #
  plot(result_tentative, type="s")
  # Variable importance #
  
  # Interactions are excluded, level =1, not level=2 #
  # testing one at a time end

############### K - code End
 
# see a quick summary view of the dataset
summary(df.data)

# see the tables structure
str(df.data)

# check the response rate
CrossTable(df.data$y)

# split the data into training and test
set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
nrow(df.train)
nrow(df.test)

# how the categorical variables are distributed and are related with target outcome
CrossTable(df.train$job, df.train$y)
CrossTable(df.train$marital, df.train$y)
CrossTable(df.train$education, df.train$y)
CrossTable(df.train$default, df.train$y)
CrossTable(df.train$housing, df.train$y)
CrossTable(df.train$loan, df.train$y)
CrossTable(df.train$poutcome, df.train$y)


#-------------------------------------------------------------------------------------


# let see how the numerical variables are distributed
hist(df.train$age)
hist(df.train$balance)
hist(df.train$duration)
hist(df.train$campaign)
hist(df.train$pdays)
hist(df.train$previous)
describe(df.train[c("age", "balance", "duration", "campaign", "pdays", "previous")])



#-------------------------------------------------------------------------------------

# running a full model  
df.train$yact = ifelse(df.train$y == 'yes',1,0)
full.model <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                    job + marital + education + default + housing + loan + poutcome, 
                  data=df.train, family = binomial)
summary(full.model)

# check for vif
fit <- lm(formula <- yact ~ age + balance + duration + campaign + pdays + previous +
                    job + marital + education + default + housing + loan + poutcome, 
                  data=df.train)
vif(fit)

# automated variable selection - Backward
backward <- step(full.model, direction = 'backward')
summary(backward)

# training probabilities and roc
df.train$prob = predict(full.model, type=c("response"))
q <- roc(y ~ prob, data = df.train)
plot(q)
auc(q)

# variable importance
varImp(full.model, scale = FALSE)

# Perform HL Test
hl <- hoslem.test(df.train$y, df.train$prob, g=10)
hl # we should not be able to reject null hypothesis - not a required test always

# confusion matrix
df.train$ypred = ifelse(df.train$prob>=.5,'pred_yes','pred_no')
table(df.train$ypred,df.train$y)

# decile chart and decision making
df.train$decile <- with(df.train, cut(prob, 
                                breaks=quantile(prob, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                                include.lowest=TRUE))
df.train$decile <- as.numeric(df.train$decile)
df.train <- df.train[order(-df.train$decile),]
train.group <- df.train %>% group_by(decile)
train.group %>% summarise(total_yes = sum(yact), total = count(yact))

decile <- aggregate(df.train$yact, by=list(df.train$decile), FUN = sum)
decile <- decile[order(-decile$Group.1),]
colnames(decile) <- c('decile', 'total_yes')
decile$random_yes <- round(sum(decile$total_yes)/10)
decile$pcnt_yes <- decile$total_yes/sum(decile$total_yes)
decile$cum_pcnt_yes <- cumsum(decile$pcnt_yes)
print(decile)

# 
#-------------------------------------------------------------------------------------

