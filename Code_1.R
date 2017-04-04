#remove all variables from env
rm(list = ls(all.names = T))


# libraries

library(data.table)
library(caret)
library(e1071)
library(randomForest)
library(stringr)
library(MASS)
library(xgboost) 
library(dplyr)
# setting workspace
setwd("~/Data Science/Haker Earth/Loan Default/Data")





#import data
train_file = "/Users/raki/Data Science/Haker Earth/Loan Default/Data/train_indessa.csv"
test_file = "/Users/raki/Data Science/Haker Earth/Loan Default/Data/test_indessa.csv"
train_dataset <- read.csv(train_file, header = T)
test_dataset <- read.csv(test_file, header = T, nrows = 1000)
str(train_dataset)
dim(train_dataset)
dim(test_dataset)
View(head(train_dataset))
unique(train_dataset$emp_length)
View(train_dataset[train_dataset$emp_length=="n/a",])
unique(train_dataset$home_ownership)

unique(train_dataset$initial_list_status)
# Data cleaning 
# Write a funciton to replace a string with another string
replace <- function(column, replace_with, replace_what){
  gsub(replace_what, replace_with, column)
}
dataclean<- function(dataset){
  col_nbr_emp_length = which(colnames(dataset)=="emp_length")
  #paste(col_nbr_emp_length)
  # Collapse all spaces
  dataset[,col_nbr_emp_length] <- replace(column = dataset[,col_nbr_emp_length], replace_what = " ", replace_with = "")
  # Now replace the string "years" with nothing
  dataset[,col_nbr_emp_length] <- replace(column = dataset[,col_nbr_emp_length], replace_what = "years", replace_with = "")
  # Now replace the string "year" with nothing
  dataset[,col_nbr_emp_length] <- replace(column = dataset[,col_nbr_emp_length], replace_what = "year", replace_with = "")
  #replacing <1 to zero and 10+to 10
  dataset[,col_nbr_emp_length] <- replace(column = dataset[,col_nbr_emp_length], replace_what = "<1", replace_with = "0")
  dataset[,col_nbr_emp_length] <- replace(column = dataset[,col_nbr_emp_length], replace_what = "\\+", replace_with = "")
  
  # Finally Replace "n/a" with zero
  dataset[,col_nbr_emp_length] <- replace(column = dataset[,col_nbr_emp_length], replace_what = "n/a", replace_with = "-1")
  #changing the column to numbers
  dataset[,col_nbr_emp_length] <- as.numeric(dataset[,col_nbr_emp_length])
  #change 
  col_nbr_term = which(colnames(dataset)=="term")
  dataset[,col_nbr_term] <- replace(column = dataset[,col_nbr_term], replace_what = " months", replace_with = "")
  dataset[,col_nbr_term] <- as.numeric(dataset[,col_nbr_term])
  
  #similarly changning last_week_pay
  col_nbr_last_week_pay = which(colnames(dataset)=="last_week_pay")
  dataset[,col_nbr_last_week_pay] <- replace(column = dataset[,col_nbr_last_week_pay], replace_what = "th week", replace_with = "")
  dataset[,col_nbr_last_week_pay] <- as.numeric(dataset[,col_nbr_last_week_pay] )

  #Similarly removing last two characters in zip_code
  col_nbr_zip_code = which(colnames(dataset)=="zip_code")
  dataset[,col_nbr_zip_code] <- replace(column = dataset[,col_nbr_zip_code], replace_what = "xx", replace_with = "")
  
  #Replacing applicaiotn type with individual or joint
  col_nbr_app_type = which(colnames(dataset)=="application_type")
  dataset[,col_nbr_app_type] <- replace(column = dataset[,col_nbr_app_type], replace_what = "INDIVIDUAL", replace_with = "1")
  dataset[,col_nbr_app_type] <- replace(column = dataset[,col_nbr_app_type], replace_what = "JOINT", replace_with = "2")
  dataset[,col_nbr_app_type] <- as.factor(as.character(dataset[,col_nbr_app_type] ))
  
  #Replacing Verification status too with Not Verified =0, Source Verified
  col_nbr_ver_status = which(colnames(dataset)=="verification_status")
  dataset[,col_nbr_ver_status] <- replace(column = dataset[,col_nbr_ver_status], replace_what = "Not Verified", replace_with = "0")
  dataset[,col_nbr_ver_status] <- replace(column = dataset[,col_nbr_ver_status], replace_what = "Source Verified", replace_with = "1")
  dataset[,col_nbr_ver_status] <- replace(column = dataset[,col_nbr_ver_status], replace_what = "Verified", replace_with = "1")
  dataset[,col_nbr_ver_status] <- as.factor(as.character(dataset[,col_nbr_ver_status] ))
  
  #cleaning home ownership
  col_nbr_home_ownership = which(colnames(dataset)=="home_ownership")
  dataset[,col_nbr_home_ownership] = replace(column = dataset[,col_nbr_ver_status], replace_what = "NONE", replace_with = "0")
  dataset[,col_nbr_home_ownership] = replace(column = dataset[,col_nbr_ver_status], replace_what = "OTHER", replace_with = "1")
  dataset[,col_nbr_home_ownership] = replace(column = dataset[,col_nbr_ver_status], replace_what = "OWN", replace_with = "2")
  dataset[,col_nbr_home_ownership] = replace(column = dataset[,col_nbr_ver_status], replace_what = "ANY", replace_with = "3")
  dataset[,col_nbr_home_ownership] = replace(column = dataset[,col_nbr_ver_status], replace_what = "MORTGAGE", replace_with = "4")
  dataset[,col_nbr_home_ownership] = replace(column = dataset[,col_nbr_ver_status], replace_what = "RENT", replace_with = "5")
  
  #cleaning initial_list_status
  col_nbr_initial_list_status = which(colnames(dataset)=="initial_list_status")
  dataset[,col_nbr_initial_list_status] = replace(column = dataset[,col_nbr_initial_list_status], replace_what = "f", replace_with = "0")
  dataset[,col_nbr_initial_list_status] = replace(column = dataset[,col_nbr_initial_list_status], replace_what = "w", replace_with = "1")
  
  
  #returning the dataset
  return (dataset)
}

train_2<-dataclean(train_dataset)
summary(train_2$last_week_pay)

View(train_2[1:10,])

# Playing with data
#1. What is the distribution of loan amount applied for data set
par( mfrow = c( 1, 1 ) )
hist(train_2$loan_amnt, breaks = 15)
boxplot(train_2$loan_amnt~train_2$loan_status)
ggplot(data = train_2, aes(x=loan_amnt, fill= loan_status)) + geom_bar(stat = "bin", position = "stack") + labs(x="Loan Status", y="Count")
qplot(loan_amnt, data = train_2 , geom = "auto", fill = loan_status) + scale_y_continuous(breaks=seq(0,400000,3000)) + scale_x_continuous(breaks=seq(0,30000,3000))
colnames(train_2) 
prop.table(table(train_2$loan_status))
#2 What years of experiece are 
boxplot(train_2$loan_amnt[train_2$loan_status==1]~train_2$emp_length[train_2$loan_status==1])
barplot(prop.table(table(train_2$emp_length)))
emp_length_default <- function(){
  result_table <- data.frame(cbind(yrs_of_exp = factor(10), non_default = numeric(10),default = numeric(10), stringsAsFactors = FALSE))
for (len in -1:10){
  table = round(prop.table(table(train_2$emp_length[train_2$emp_length==len], train_2$loan_status[train_2$emp_length==len])), 6)*100
  result_table$yrs_of_exp[len]<-len
  result_table$non_default[len]<-table[1,1]
  result_table$default[len]<-table[1,2]
  
}
  return (result_table)
}
emp_length_default()
#Insight: People with 6 years experience are defaulting the most.
#People with >= 10 years of experiece are defaulting the least

result_table <- data.frame(cbind(len = factor(10), non_default = numeric(10),default = numeric(10), stringsAsFactors = FALSE))
table =round(prop.table(table(train_2$emp_length[train_2$emp_length==1], train_2$loan_status[train_2$emp_length==1])), 3)*100
table[1,2]
round(prop.table(table(train_2$emp_length[train_2$emp_length==0], train_2$loan_status[train_2$emp_length==0])), 3)*100
round(prop.table(table(train_2$emp_length[train_2$emp_length==10], train_2$loan_status[train_2$emp_length==10])), 3)*100
round(prop.table(table(train_2$emp_length[train_2$emp_length==9], train_2$loan_status[train_2$emp_length==9])), 3)*100


hist(train_2$loan_amnt[train_2$emp_length==1], breaks = 40)
hist(train_2$loan_amnt[train_2$emp_length==2], breaks = 40)
hist(train_2$loan_amnt[train_2$emp_length==3], breaks = 40)
max(train_2$loan_amnt)
train_2$loan_amnt[train_2$emp_length==1]
colnames(train_2)
str(train_2)
train_2$loan_amnt[train_2$emp_length==1]
# Remove columns that are not required

colnames(train_2)
stay_dataset <- function(dataset){
  rem_cols<- c("member_id" ,"batch_enrolled" , "emp_title", "desc", "title", "addr_state", "mths_since_last_delinq", 
             "mths_since_last_record"  , "mths_since_last_major_derog", "verification_status_joint" )
  stay_cols <- setdiff(colnames(dataset) , rem_cols)
  dataset2 <- dataset[,stay_cols]
  
  factor_cols <- c("grade", "sub_grade", "home_ownership", "verification_status", "pymnt_plan",
                   "purpose", "initial_list_status", "application_type", "loan_status")
  numeric_cols <- setdiff(stay_cols, factor_cols)
  
  factor_dataset <- data.frame(sapply(dataset2[,factor_cols],as.factor))
  numeric_dataset <- data.frame(sapply(dataset2[,numeric_cols],as.numeric))
  dataset2 <- data.frame((cbind(numeric_dataset,factor_dataset)))
  
  return (dataset2)
}

# Feature Engineering
# Feature 1: New variable with 
feature-engineerd_dataset <- function(dataset){
  feature1  <- log(annual_inc/loan_amnt)
  total_recd_amt <- total_rec_int + total_rec_late_fee
  
}
train_stay_cols <- stay_dataset(train_2)
colnames(train_stay_cols)
str(train_stay_cols)
summary(train_stay_cols)
#Exploring stay dataset
num_col<-colnames(train_stay_cols)[sapply(train_stay_cols, is.numeric)]
num_col <- num_col[!(num_col %in% c("member_id","loan_status"))]
corrplot::corrplot(cor(train_stay_cols[,num_col]),method = "number", is.corr = T)

numeric_cols <- setdiff(stay_cols, factor_cols)


corrplot::corrplot(cor(train_stay_cols[,num_col,with=F]),method = "number")
View(cor(train_stay_cols[,num_col])>0.8)
# Huge correlation between funded_amnt_inv, funded_amnt and loan amount.
# Huge correlation between recoveries and recovery fee
# So removing funded_amnt_inv, and funded_amnt, recoverry_fee
col_nbr_funded_amnt_inv = which(colnames(train_stay_cols)=="funded_amnt_inv")
col_nbr_funded_amnt = which(colnames(train_stay_cols)=="funded_amnt")
col_nbr_collection_recovery_fee = which(colnames(train_stay_cols)=="collection_recovery_fee")

train_stay_cols<-train_stay_cols[,-c(col_nbr_funded_amnt_inv,col_nbr_funded_amnt,col_nbr_collection_recovery_fee , sub_grade )]
#train_stay_cols[,c("funded_amnt","funded_amnt_inv","collection_recovery_fee") := NULL]
#train_stay_cols <- subset(train_stay_cols, -c(funded_amnt_inv,funded_amnt, collection_recovery_fee ))

chisq.test(train_stay_cols$sub_grade, train_stay_cols$grade)
#Grade and subgrade are related






# temporarily deleting columns having NAs.. Do What needs to be done later. Study the data
temp_NA_remove_func <- function(dataset){
  col_nbr_collections_12_mths_ex_med = which(colnames(dataset)=="collections_12_mths_ex_med")
  col_nbr_last_week_pay = which(colnames(dataset)=="last_week_pay")
  col_nbr_acc_now_delinq = which(colnames(dataset)=="acc_now_delinq")
  col_nbr_tot_coll_amt = which(colnames(dataset)=="tot_coll_amt")
  dataset2 <- dataset[, -c(col_nbr_collections_12_mths_ex_med, col_nbr_last_week_pay, 
                                        col_nbr_acc_now_delinq, col_nbr_tot_coll_amt)]

return (dataset2)
}

train_stay_cols<- temp_NA_remove_func(train_stay_cols)
colnames(train_stay_cols)
str(train_stay_cols)
summary(train_stay_cols)



# Splitting data into train and test
train_rows <- createDataPartition( train_stay_cols$loan_status, times = 1, p = 0.7, list = F)
train_data <- train_stay_cols[train_rows, ]
test_data <- train_stay_cols[-train_rows, ]

summary(train_data)
prop.table(table(train_data$loan_status))
prop.table(table(test_data$loan_status))


# trying a naive randaom forest model

nb_model<- naiveBayes(loan_status~.,data = train_data, laplace =1 )
summary(nb_model)

my_predict_function <- function(dataset, model){
  col_nbr_loan_status <- which(colnames(dataset)=="loan_status")
  # removing response variable
  dataset2 <- dataset[, -col_nbr_loan_status] 
  # prediction on dataset
  predict_on_train <- predict(model, newdata = dataset2, type = "class")
  return (predict_on_train)
  
}
predict_on_train <- my_predict_function(train_data, nb_model)
View(head(data.frame(predict_on_train)))

conf.mat_train = table(train_data$loan_status,predict_on_train)
accuracy_train = sum(diag(conf.mat_train))/sum(conf.mat_train)
precision_train = conf.mat_train[2,2]/sum(conf.mat_train[,2])
recall_train = conf.mat_train[2,2]/sum(conf.mat_train[2,])


#auc calculaiton
library(ROCR)
p <- predict(nb_model,train_data[,-c(32)])
pr <- prediction(as.numeric(predict_on_train), as.numeric(train_data$loan_status))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0, b= 1)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # very low


# Naive bayes is not gicing the required performance
# Lets try random forest

RF_model <- randomForest(loan_status~.,data = train_data)
varImpPlot(RF_model)
colnames(train_data)
ind_Attr <- setdiff(colnames(train_data), "loan_status")
# Trying Boosting model

View(head(train_data))
dtrain = xgb.DMatrix(data = as.matrix(train_data[,ind_Attr]),
                     label = train_data$loan_status)
model = xgboost(data = dtrain, max.depth = 2, 
                eta = 1, nthread = 2, nround = 2, 
                objective = "binary:logistic", verbose = 1)



## Running on Submission Data
test_data<- dataclean(test_dataset)
