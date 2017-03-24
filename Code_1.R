#remove all variables from env
rm(list = ls(all.names = T))

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

View(train_2[1:10,])

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
train_stay_cols <- stay_dataset(train_2)
colnames(train_stay_cols)
str(train_stay_cols)
