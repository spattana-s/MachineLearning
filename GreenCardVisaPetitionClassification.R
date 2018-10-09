#CS513 - Knowledge Distribution and Data Mining
#Section: CS513 B
#Aim: To classify if a Visa petition as Certified or Denied
#Assumptions -
#1) Not considering Withdrawn petitions
#2) Certified Expired have been assumed to be Certified as they got expired because the employer did not follow up

#US Green Card Visa dataset

#Uncomment these for the first run and then comment again
#install.packages('rstudioapi')
#install.packages('readr')

#Clean the workspace first
rm(list=ls())

#importing packages
library(rstudioapi) 
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
#setting the Working Directory for referencing the CSV datafile
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#importing the Data
dataset <- read_csv("us_perm_visas.csv")
#summary(dataset)

#Reading the Dates from Character vectors
dataset$decision_date<-parse_date(dataset$decision_date)
dataset$case_received_date<-parse_date(dataset$case_received_date)

#An attempt to use  balanced dataset for the analysis
sampling_dataset <- dataset
#start by dropping withdrawals
sampling_dataset<- sampling_dataset[!sampling_dataset$case_status=="Withdrawn",]

#get balanced sampled dataset
sampling_dataset[sampling_dataset$case_status=="Certified-Expired","case_status"] <- "Certified"
certified_dataset <- sampling_dataset[sampling_dataset$case_status=="Certified",]
denied_dataset <- sampling_dataset[sampling_dataset$case_status=="Denied",]
sampling_percentage <- 0.50
balanced_dataset <- rbind(certified_dataset[sample(nrow(certified_dataset),size=nrow(denied_dataset)*sampling_percentage),],denied_dataset[sample(nrow(denied_dataset),size=nrow(denied_dataset)*sampling_percentage),])
# Uncommment this if you need to run the analysis on a Balanced Dataset
#clean_dataset<- balanced_dataset

#Assigning the csv dataset to another variable for modification
clean_dataset <- dataset


#Read dimensions
dim(clean_dataset)

#Plot the CASE STATUS graph
ggplot(data = clean_dataset) +
  geom_bar(mapping = aes(x = clean_dataset$case_status))

#start by dropping withdrawals
clean_dataset<- clean_dataset[!clean_dataset$case_status=="Withdrawn",]

#change status for certified-expired to certified - Read Assumption 2 above
clean_dataset[clean_dataset$case_status=="Certified-Expired","case_status"] <- "Certified"

#get month and year from decision date
clean_dataset$decision_month <- as.factor(months(clean_dataset$decision_date))
clean_dataset$decision_year <- as.factor(year(clean_dataset$decision_date))
clean_dataset$decision_date<-NULL

#get month and year from decision date
clean_dataset$received_month <- as.factor(months(clean_dataset$case_received_date))
clean_dataset$received_year <- as.factor(year(clean_dataset$case_received_date))
clean_dataset$case_received_date<-NULL

#Plot of certified -denied vs Decision Month
ggplot() +
  geom_bar(data = clean_dataset,aes(y = clean_dataset$case_status, x = clean_dataset$decision_month, fill = clean_dataset$case_status=="De"),stat="identity")

clean_dataset %>% count(case_status)

#####start cleaning dataset#############

#pass values from country of citizenship 2 to the blank rows of country of citizenship 1
clean_dataset$country_of_citizenship = ifelse(is.na(clean_dataset$country_of_citizenship), clean_dataset$country_of_citzenship, clean_dataset$country_of_citizenship)
#eliminate extra column for country of citizenship
clean_dataset$country_of_citzenship <- NULL

#jobTitle column merged to PW_Job_Title
clean_dataset$add_these_pw_job_title_9089 = ifelse(is.na(clean_dataset$add_these_pw_job_title_9089), clean_dataset$pw_job_title_9089, clean_dataset$add_these_pw_job_title_9089)
clean_dataset$add_these_pw_job_title_9089 = ifelse(is.na(clean_dataset$add_these_pw_job_title_9089), clean_dataset$pw_job_title_908, clean_dataset$add_these_pw_job_title_9089)
clean_dataset["PW_Job_Title"]<-clean_dataset$add_these_pw_job_title_9089

#drop all other columns for job title
clean_dataset$add_these_pw_job_title_9089<-NULL
clean_dataset$pw_job_title_9089<-NULL
clean_dataset$pw_job_title_908<-NULL


#eliminate the obvious non-meaningful fields
clean_dataset$employer_address_1 <- NULL
clean_dataset$employer_address_2 <- NULL
clean_dataset$employer_phone <- NULL
clean_dataset$employer_phone_ext <- NULL
clean_dataset$case_no <- NULL
clean_dataset$case_number <- NULL

#this is the Employer POC - Point of Contact Title hence deleting
clean_dataset$employer_decl_info_title <- NULL
#retaining the employer state and removing postal codes 
clean_dataset$EMPLOYER_POSTAL_CODE <- NULL

#replace NA values with Others and removing postal code and info city column for foreign worker
clean_dataset$foreign_worker_info_state = ifelse(is.na(clean_dataset$foreign_worker_info_state), "Others", clean_dataset$foreign_worker_info_state)
clean_dataset$foreign_worker_info_education = ifelse(is.na(clean_dataset$foreign_worker_info_education), "Others", clean_dataset$foreign_worker_info_education)
clean_dataset$job_info_foreign_ed = ifelse(is.na(clean_dataset$job_info_foreign_ed), "Others", clean_dataset$job_info_foreign_ed)
clean_dataset$job_info_foreign_lang_req = ifelse(is.na(clean_dataset$job_info_foreign_lang_req), "Others", clean_dataset$job_info_foreign_lang_req)
clean_dataset$agent_firm_name = ifelse(is.na(clean_dataset$agent_firm_name), "Others", clean_dataset$agent_firm_name)

clean_dataset$employer_yr_estab = ifelse(is.na(clean_dataset$employer_yr_estab), "Not Available", clean_dataset$employer_yr_estab)
clean_dataset$agent_state = ifelse(is.na(clean_dataset$agent_state), "Others", clean_dataset$agent_state)
clean_dataset$employer_country = ifelse(is.na(clean_dataset$employer_country), "Others", clean_dataset$employer_country)
clean_dataset$preparer_info_emp_completed = ifelse(is.na(clean_dataset$preparer_info_emp_completed), "Others", clean_dataset$preparer_info_emp_completed)
clean_dataset$employer_num_employees<-as.integer(clean_dataset$employer_num_employees)
clean_dataset$recr_info_coll_univ_teacher = ifelse(is.na(clean_dataset$recr_info_coll_univ_teacher), "Others", clean_dataset$recr_info_coll_univ_teacher)
clean_dataset$ri_layoff_in_past_six_months = ifelse(is.na(clean_dataset$ri_layoff_in_past_six_months), "No Response", clean_dataset$ri_layoff_in_past_six_months)
clean_dataset$job_info_alt_combo_ed_exp = ifelse(is.na(clean_dataset$job_info_alt_combo_ed_exp), "No Response", clean_dataset$job_info_alt_combo_ed_exp)
clean_dataset$recr_info_employer_rec_payment = ifelse(is.na(clean_dataset$recr_info_employer_rec_payment), "No Response", clean_dataset$recr_info_employer_rec_payment)
clean_dataset$job_info_training = ifelse(is.na(clean_dataset$job_info_training), "No Response", clean_dataset$job_info_training)
clean_dataset$recr_info_professional_occ = ifelse(is.na(clean_dataset$recr_info_professional_occ), "No Response", clean_dataset$recr_info_professional_occ)
clean_dataset$job_info_alt_field = ifelse(is.na(clean_dataset$job_info_alt_field), "No Response", clean_dataset$job_info_alt_field)
clean_dataset$job_info_experience = ifelse(is.na(clean_dataset$job_info_experience), "No Response", clean_dataset$job_info_experience)
clean_dataset$job_info_combo_occupation = ifelse(is.na(clean_dataset$job_info_combo_occupation), "No Response", clean_dataset$job_info_combo_occupation)
clean_dataset$job_info_education = ifelse(is.na(clean_dataset$job_info_education), "Not Available", clean_dataset$job_info_education)
clean_dataset$job_info_job_title = ifelse(is.na(clean_dataset$job_info_job_title), "Others", clean_dataset$job_info_job_title)
clean_dataset$job_info_major = ifelse(is.na(clean_dataset$job_info_major), "Others", clean_dataset$job_info_major)




#Replacing number of employess with mean number of employees
clean_dataset$employer_num_employees = ifelse(is.na(clean_dataset$employer_num_employees), mean(clean_dataset$employer_num_employees,na.rm = TRUE), clean_dataset$employer_num_employees)
clean_dataset$received_month = ifelse(is.na(clean_dataset$received_month), "Others", clean_dataset$received_month)
clean_dataset$received_year = ifelse(is.na(clean_dataset$received_year), "Others", clean_dataset$received_year)

#Dropping irrelevant columns
clean_dataset$foreign_worker_info_city <- NULL
clean_dataset$preparer_info_title <- NULL
clean_dataset$agent_city <- NULL
clean_dataset$schd_a_sheepherder <- NULL
clean_dataset$foreign_worker_info_postal_code <- NULL
clean_dataset$orig_case_no <- NULL
clean_dataset$job_info_job_req_normal<-NULL
clean_dataset$agent_city <- NULL
clean_dataset$job_info_work_city <- NULL
clean_dataset$job_info_work_postal_code <- NULL
clean_dataset$employer_city <- NULL
clean_dataset$job_info_work_city <- NULL
clean_dataset$foreign_worker_info_inst<-NULL
clean_dataset$pw_track_num<-NULL
clean_dataset$ri_job_search_website_from<-NULL
clean_dataset$recr_info_sunday_newspaper<-NULL
clean_dataset$foreign_worker_info_major<-NULL
clean_dataset$foreign_worker_info_inst<-NULL
clean_dataset$recr_info_swa_job_order_end<-NULL
clean_dataset$recr_info_second_ad_start<-NULL
clean_dataset$recr_info_swa_job_order_start<-NULL
clean_dataset$ri_2nd_ad_newspaper_or_journal<-NULL
clean_dataset$recr_info_first_ad_start<-NULL
clean_dataset$ri_2nd_ad_newspaper_name<-NULL
clean_dataset$ri_1st_ad_newspaper_name<-NULL
clean_dataset$pw_expire_date<-NULL
clean_dataset$pw_determ_date<-NULL
clean_dataset$ri_posted_notice_at_worksite<-NULL
clean_dataset$ji_live_in_domestic_service<-NULL
clean_dataset$ri_job_search_website_to<-NULL
clean_dataset$case_received_date<-NULL
clean_dataset$wage_offer_unit_of_pay_9089<-NULL
clean_dataset$employer_yr_estab<-NULL



#check for completeness of the data
percentageNull<-sort(colSums(is.na(clean_dataset)/nrow(clean_dataset))*100)

#column null threshold at 50 % and Row at 1%
NA_percentage_threshold_column <- .5
NA_percentage_threshold_row <- .01

column_names <- colnames(clean_dataset)
for (column_name in column_names){
  NA_percentage <- nrow(clean_dataset[is.na(clean_dataset[,column_name]),column_name]) / nrow(clean_dataset)
  if (NA_percentage > NA_percentage_threshold_column){
    #drop the entire column
    columnsDropped=as.data.frame(cbind(column_name,NA_percentage))
    clean_dataset[,column_name] <- NULL
  } else {
    if(NA_percentage<NA_percentage_threshold_row){
      #drop the rows
      clean_dataset <- clean_dataset[!is.na(clean_dataset[,column_name]),]
    }
  }
}

#Handle Prewailing Wage for the Job
#converting units
clean_dataset$pw_unit_of_pay_9089 <- ifelse(clean_dataset$pw_unit_of_pay_9089=='yr',"Year" , clean_dataset$pw_unit_of_pay_9089)
clean_dataset$pw_unit_of_pay_9089 <- ifelse(is.na(clean_dataset$pw_unit_of_pay_9089),"Year" , clean_dataset$pw_unit_of_pay_9089)
clean_dataset$pw_unit_of_pay_9089 <- ifelse(clean_dataset$pw_unit_of_pay_9089=='bi',"Bi-Weekly" , clean_dataset$pw_unit_of_pay_9089)
clean_dataset$pw_unit_of_pay_9089 <- ifelse(clean_dataset$pw_unit_of_pay_9089=='hr',"Hour" , clean_dataset$pw_unit_of_pay_9089)
clean_dataset$pw_unit_of_pay_9089 <- ifelse(clean_dataset$pw_unit_of_pay_9089=='wk',"Week" , clean_dataset$pw_unit_of_pay_9089)
clean_dataset$pw_unit_of_pay_9089 <- ifelse(clean_dataset$pw_unit_of_pay_9089=='mth',"Month" , clean_dataset$pw_unit_of_pay_9089)


listofUnits=unique(clean_dataset$pw_unit_of_pay_9089)
 for (unit in listofUnits){
   if (unit == "Hour"){
     clean_dataset[clean_dataset$pw_unit_of_pay_9089 == unit, 'pw_amount_9089']<-clean_dataset[clean_dataset$pw_unit_of_pay_9089 == unit, 'pw_amount_9089'] * 8 * 250
   }
   if (unit == "Week"){
     clean_dataset[clean_dataset$pw_unit_of_pay_9089 == unit, 'pw_amount_9089']<-clean_dataset[clean_dataset$pw_unit_of_pay_9089 == unit, 'pw_amount_9089'] * 56
   }
   if (unit == "Month"){
     clean_dataset[clean_dataset$pw_unit_of_pay_9089 == unit, 'pw_amount_9089']<-clean_dataset[clean_dataset$pw_unit_of_pay_9089 == unit, 'pw_amount_9089'] * 12
   }
   if (unit == "Bi-Weekly"){
     clean_dataset[clean_dataset$pw_unit_of_pay_9089 == unit, 'pw_amount_9089']<-clean_dataset[clean_dataset$pw_unit_of_pay_9089 == unit, 'pw_amount_9089'] * 28
   }

 }

#Function to get the major occupational category from Prewailing Wage SOC Code
cleanPWSOCCode<-function(x){
  # #Converting values to lower case
  l<-split(x,'-')
  x<-as.factor(substr(x,0,2))
}
 #Filling missing values with median 
 clean_dataset$pw_amount_9089 <- ifelse(is.na(clean_dataset$pw_amount_9089),median(clean_dataset$pw_amount_9089,na.rm = TRUE) , clean_dataset$pw_amount_9089)
 #ggplot() +
  # geom_bar(data = clean_dataset,aes(y = clean_dataset$pw_amount_9089, x = clean_dataset$case_status, fill = clean_dataset$case_status),stat="identity")
#Unit of Pay is no longer required as all values have been converted to Yearly amounts 
clean_dataset$pw_unit_of_pay_9089<-NULL

#replacing class of admission by mode value
ll<-data.frame(table(clean_dataset$class_of_admission))
mlv_class_of_adm <- ll[which.max(ll$Freq),]
mlv_class_of_adm <- as.character(mlv_class_of_adm$Var1[1])
clean_dataset[is.na(clean_dataset$class_of_admission),"class_of_admission"] <- mlv_class_of_adm

#replacing Prewailing Wage Level by mode value
ll<-data.frame(table(clean_dataset$pw_level_9089))
mlv_pw_level <- ll[which.max(ll$Freq),]
mlv_pw_level <- as.character(mlv_pw_level$Var1[1])
clean_dataset[is.na(clean_dataset$pw_level_9089),"pw_level_9089"] <- mlv_pw_level

#Get the major occupationcal category
clean_dataset$major_occup<-sapply(clean_dataset$pw_soc_code,cleanPWSOCCode)
unique(clean_dataset$job_info_work_state)
unique(clean_dataset$major_occup)
#States of US - Abbreviation list
states <- read.csv("USstateAbbreviations.csv",stringsAsFactors = FALSE)
#SOC Codes from Department of Labour
soc1 <- read.csv("SOC_Codes.csv",stringsAsFactors = FALSE)
#Convert SOC Codes to Titles
for (i in 1:length(soc1$SOCCode)){
  subset <- !is.na(clean_dataset$major_occup) & clean_dataset$major_occup==(soc1[i,"SOCCode"])
  clean_dataset[subset,"Job_Category"] <- as.character(soc1[i,"SOCTitle"])
}
#Cnvert all states to abbreviations
for (i in 1:length(states$Name)){
  clean_dataset[clean_dataset$job_info_work_state==toupper(states[i,"Name"]),"job_info_work_state"] <- as.character(states[i,"ANSI.letters"])
  clean_dataset[clean_dataset$agent_state==toupper(states[i,"Name"]),"agent_state"] <- as.character(states[i,"ANSI.letters"])
  clean_dataset[clean_dataset$employer_state==toupper(states[i,"Name"]),"employer_state"] <- as.character(states[i,"ANSI.letters"])
  clean_dataset[clean_dataset$foreign_worker_info_state==toupper(states[i,"Name"]),"foreign_worker_info_state"] <- as.character(states[i,"ANSI.letters"])
  
}

#replace empty postal code for employers with the most frequent one from the job_info_work_state
# empty_postal_code_states <- unique(clean_dataset[is.na(clean_dataset$employer_postal_code),"job_info_work_state"])
# empty_postal_code_states <- empty_postal_code_states[[1]]
# for (empty_pc_state in empty_postal_code_states){
#   ll<-data.frame(table(clean_dataset[clean_dataset$job_info_work_state==empty_pc_state,"employer_postal_code"]))
#   mlv_postal_code <- ll[which.max(ll$Freq),]
#   mlv_postal_code <- as.numeric(mlv_postal_code$Var1[1])
#   clean_dataset[is.na(clean_dataset$employer_postal_code)&clean_dataset$job_info_work_state==empty_pc_state,"employer_postal_code"] <- mlv_postal_code
# }
#Removing Postal Codes and Occupational codes and PW source names
clean_dataset$employer_postal_code<-NULL
clean_dataset$major_occup<-NULL
clean_dataset$pw_soc_code<-NULL
clean_dataset$pw_soc_title<-NULL
clean_dataset$pw_source_name_9089<-NULL

#no NA values at this point
sum(is.na(clean_dataset))

#drop any rows that have empty records
clean_dataset<-na.omit(clean_dataset)

#convert categorical variables as factors
clean_dataset$case_status <- as.factor(clean_dataset$case_status)
clean_dataset$class_of_admission <- as.factor(clean_dataset$class_of_admission)
clean_dataset$country_of_citizenship <- as.factor(clean_dataset$country_of_citizenship)
clean_dataset$job_info_work_state <- as.factor(clean_dataset$job_info_work_state)
clean_dataset$pw_level_9089 <- as.factor(clean_dataset$pw_level_9089)
clean_dataset$ri_layoff_in_past_six_months <- as.factor(clean_dataset$ri_layoff_in_past_six_months)
clean_dataset$agent_state <- as.factor(clean_dataset$agent_state)
clean_dataset$received_year <- as.factor(clean_dataset$received_year)
clean_dataset$received_month <- as.factor(clean_dataset$received_month)
#install.packages("binr")
clean_dataset$foreign_worker_info_education <- as.factor(clean_dataset$foreign_worker_info_education)
clean_dataset$foreign_worker_info_state <- as.factor(clean_dataset$foreign_worker_info_state)
clean_dataset$employer_country <- as.factor(clean_dataset$employer_country)
clean_dataset$job_info_alt_combo_ed_exp <- as.factor(clean_dataset$job_info_alt_combo_ed_exp)
clean_dataset$job_info_alt_field <- as.factor(clean_dataset$job_info_alt_field)
clean_dataset$job_info_combo_occupation <- as.factor(clean_dataset$job_info_combo_occupation)
clean_dataset$job_info_education <- as.factor(clean_dataset$job_info_education)
clean_dataset$job_info_experience <- as.factor(clean_dataset$job_info_experience)
clean_dataset$job_info_foreign_ed <- as.factor(clean_dataset$job_info_foreign_ed)
clean_dataset$job_info_foreign_lang_req <- as.factor(clean_dataset$job_info_foreign_lang_req)
clean_dataset$preparer_info_emp_completed <- as.factor(clean_dataset$preparer_info_emp_completed)
clean_dataset$recr_info_coll_univ_teacher <- as.factor(clean_dataset$recr_info_coll_univ_teacher)
clean_dataset$recr_info_employer_rec_payment <- as.factor(clean_dataset$recr_info_employer_rec_payment)
clean_dataset$recr_info_professional_occ <- as.factor(clean_dataset$recr_info_professional_occ)
clean_dataset$recr_info_coll_univ_teacher <- as.factor(clean_dataset$recr_info_coll_univ_teacher)
clean_dataset$job_info_training = as.factor(clean_dataset$job_info_training)
clean_dataset$Job_Category = as.factor(clean_dataset$Job_Category)
#Function to clean the Employer Names , Firm Names and Job Titles of Punctuation marks
cleanString <-function(x){
    x=gsub("[[:punct:]]"," ",x)
}
#String cleaning
clean_dataset$employer_name<-sapply(clean_dataset$employer_name,cleanString)
clean_dataset$agent_firm_name<-sapply(clean_dataset$agent_firm_name,cleanString)
clean_dataset$PW_Job_Title<-sapply(clean_dataset$PW_Job_Title,cleanString)
clean_dataset$job_info_job_title<-sapply(clean_dataset$job_info_job_title,cleanString)
clean_dataset$job_info_major<-sapply(clean_dataset$job_info_major,cleanString)

#Random Forest cannot handle more than 53 categories - Methods to group the states based on number of applications
employer_states<-table(clean_dataset$employer_state)
foreign_worker_states<-table(clean_dataset$foreign_worker_info_state)
citizenshipCount<-table(clean_dataset$country_of_citizenship)
jobStateCount<-table(clean_dataset$job_info_work_state)
agentStatesCount<-table(clean_dataset$agent_state)

#for Reducing number of foreign worker states
reduceFWStates <-function(x){
  c<-as.integer(foreign_worker_states[x])
  ifelse(c<75,"Others",as.character(x))
}
#for Reducing number of agent states
reduceAgentStates <-function(x){
  c<-as.integer(agentStatesCount[x])
  ifelse(c<10,"Others",as.character(x))
}
#for Reducing number of countries of citizenship
reduceCountries <-function(x){
  c<-as.integer(citizenshipCount[x])
  ifelse(c<450,"Others",as.character(x))
}
#for Reducing number of work location states
reduceStates <-function(x){
  c<-as.integer(jobStateCount[x])
  ifelse(c<115,"Others",as.character(x))
}

reduceEmployerStates <-function(x){
  c<-as.integer(employer_states[x])
  ifelse(c<100,"Others",as.character(x))
}
clean_dataset$employer_state<-sapply(clean_dataset[["employer_state"]],reduceEmployerStates) 
clean_dataset$employer_state<-as.factor(clean_dataset$employer_state)
clean_dataset$foreign_worker_info_state<-sapply(clean_dataset[["foreign_worker_info_state"]],reduceFWStates) 
clean_dataset$foreign_worker_info_state = as.factor(clean_dataset$foreign_worker_info_state)


clean_dataset$agent_state<-sapply(clean_dataset[["agent_state"]],reduceAgentStates) 
clean_dataset$agent_state = as.factor(clean_dataset$agent_state)
clean_dataset$Citizenship_Country<-sapply(clean_dataset[["country_of_citizenship"]],reduceCountries) 
clean_dataset$Citizenship_Country = as.factor(clean_dataset$Citizenship_Country)

#new column for Work State
clean_dataset$Job_State<-sapply(clean_dataset[["job_info_work_state"]],reduceStates) 
clean_dataset$Job_State = as.factor(clean_dataset$Job_State)
clean_dataset$country_of_citizenship<-NULL
clean_dataset$job_info_work_state<-NULL

#Merging Visa Categories to reduce number of factors
clean_dataset[clean_dataset$class_of_admission=="R-2","class_of_admission"] <- "R-1"
clean_dataset[clean_dataset$class_of_admission=="Parol","class_of_admission"] <- "Parolee"
clean_dataset[clean_dataset$class_of_admission=="AOS/H-1B","class_of_admission"] <- "H1B"
clean_dataset[clean_dataset$class_of_admission=="AOS","class_of_admission"] <- "H1B"
clean_dataset$class_of_admission=droplevels.factor(clean_dataset$class_of_admission)

#Final Structure
str(clean_dataset)
#No NAS
sum(is.na(clean_dataset))
clean_dataset<-na.omit(clean_dataset)

#Random Forest Begins

rf<-clean_dataset
#dropping Character variables
rf$agent_firm_name<-NULL
rf$employer_name<-NULL
rf$PW_Job_Title<-NULL
rf$pw_soc_code<-NULL
rf$pw_soc_title<-NULL
rf$job_info_job_title<-NULL
rf$job_info_major<-NULL
#setting all the month and years as zero as these are not in applicant's control
rf$decision_month<-NULL
rf$decision_year<-NULL
#rf$received_month<-NULL
rf$received_year<-NULL


set.seed(123)
#train test split
indexRf<-sort(sample(nrow(rf),round(.30*nrow(rf))))
trainingRF<-rf[-indexRf,]
testRF<-rf[indexRf,]
################RANDOM FOREST#######################################
#install.packages("rfUtilities")
library(randomForest)
rf.fit <- randomForest(case_status~., data=trainingRF, importance=TRUE, ntree=50,max_depth=25,max_features=8,n_estimators=30)

summary(rf.fit)
features<-randomForest::importance(rf.fit)
View(features)
varImpPlot(rf.fit)
Prediction <- predict(rf.fit,testRF[,-2] )
table(actual=testRF$case_status,Prediction)

wrong_RF<- (testRF$case_status!=Prediction )
RF_error_rate<-sum(wrong_RF)/length(wrong_RF)
paste0('The  Random Forest model gives an Error Rate of:',round(RF_error_rate*100,2),'%')


################Final Dataset#######################################
#Subsetting the RF Dataset with the important features from RF - 
#employer_state,Job_State,Citizenship_Country,class_of_admission,Job_Category,pw_amount_9089
retainedFeatures<-c("employer_state","Job_State","Citizenship_Country","class_of_admission","Job_Category","pw_amount_9089","case_status")
final_ds<-rf[,retainedFeatures]
set.seed(123)
#train test split
index<-sort(sample(nrow(final_ds),round(.30*nrow(final_ds))))
train<-final_ds[-index,]
test<-final_ds[index,]


################RERUN RANDOM FOREST WITH NEW VARIABLES#######################################
rf_final <- randomForest(case_status~., data=train, importance=TRUE, ntree=50,max_depth=25,max_features=8,n_estimators=30)

summary(rf_final)
features_final<-randomForest::importance(rf_final)
View(features_final)
varImpPlot(rf_final)
rf_prediction <- predict(rf_final,test[,-7] )
table(actual=test$case_status,rf_prediction)

wrong_RF_final<- (test$case_status!=rf_prediction )
RF_error_rate_final<-sum(wrong_RF_final)/length(wrong_RF_final)
paste0('The  Random Forest model gives an Error Rate of:',round(RF_error_rate_final*100,2),'%')
###############NAIVE BAYES#######################################
#run Naive Bayes
library(rattle)
library(e1071)
nBayes_all <- naiveBayes(case_status ~., data =train)
category_all<-predict(nBayes_all,test[,-7])

table(NBayes_all=category_all,case_status=test$case_status)
NB_wrong<-sum(category_all!=test$case_status)
NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate
paste0('The  Naive Bayes model gives an Error Rate of:',round(NB_error_rate*100,2),'%')

################.CART#######################################
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle
CART_class<-rpart( case_status~.,data=train)

CART_Predicted<-predict(CART_class ,test[,-7] , type="class" )
table(CART_Prediction=CART_Predicted,Actual=test$case_status)
CARTwrong<- (testRF$case_status!=CART_Predicted)
error_rateCART<-sum(CARTwrong)/length(test$case_status)
paste0('The  CART model gives an Error Rate of:',round(error_rateCART*100,2),'%')

################C50#######################################
# #install.packages("C50")
library('C50')
C50_class1 <-C5.0(case_status~.,data=train )
summary(C50_class1 )
C50_predict<-predict( C50_class1 ,testRF[,-2] , type="class" )
table(actual=testRF$case_status,C50=C50_predict)
c5wrong<- (testRF$case_status!=C50_predict)
error_ratec5<-sum(c5wrong)/length(testRF$case_status)
paste0('The  C 5.0 model gives an Error Rate of:',round(error_ratec5*100,2),'%')

