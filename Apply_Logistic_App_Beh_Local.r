

################################################################################
#               Joint script for Application and Behavioral scoring            #
#      Apply Logistic Regression on all products (Flexcredit Moldova)          #
#                          Version 1.0 (2023/12/15)                            #
################################################################################



########################
### Initial settings ###
########################

# Library
suppressMessages(suppressWarnings(library(RMariaDB)))
suppressMessages(suppressWarnings(library(DBI)))
suppressMessages(suppressWarnings(library(Rcpp)))
#suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(require(jsonlite)))
suppressMessages(suppressWarnings(require(stringi)))
suppressMessages(suppressWarnings(require(lubridate)))


# Database
db_user <- "analysis1"
db_password <- "2hg7@nkK7b"
db_name <- "cm"
db_host <- "167.235.252.204"
df_port <- 6033
con <- dbConnect(RMariaDB::MariaDB(),dbname = db_name,host = db_host,
                 port = df_port,user = db_user,password = db_password)


# Define work directory
main_dir <- paste0("\\\\192.168.2.30\\Analyses\\Shared\\Scorecards\\",
  "Moldova\\Apply_Models_Moldova\\")


# Read argument of ID
args <- commandArgs(trailingOnly = TRUE)
loan_id <- args[1]
#loan_id <- 25351
product_id <- NA


# Set working directory for input (R data for logistic regression) and output #
setwd(main_dir)
source(paste(main_dir,"Cutoffs.r", sep=""))
source(paste(main_dir,"SQL_queries.r", sep=""))
source(paste(main_dir,"Demographic_Variables.r", sep=""))
source(paste(main_dir,"Logistic_App_Flexcredit.r", sep=""))
source(paste(main_dir,"Logistic_Beh_Flexcredit.r", sep=""))
source(paste(main_dir,"Misc_Functions_Local.r", sep=""))
source(paste(main_dir,"Generate_Adjust_Score.r", sep=""))


# Load modeling libraries
load("rdata\\flexcredit_app.rdata")


####################################
### Read database and build data ###
####################################

# Read credits applications
all_df <- gen_query(con,gen_big_sql_query(db_name,loan_id))


# Get income 
all_df$income <- ifelse(length(
  gen_query(con,gen_income_query(db_name,all_df))$income)==0,NA,
  gen_query(con,gen_income_query(db_name,all_df))$income)


# Apply some checks to main credit dataframe
if(!is.na(product_id)){
  all_df$product_id <- product_id
}
if(nrow(all_df)>1){
  all_df <- all_df[!duplicated(all_df$loan_id),]
}


# Read product's periods and amounts
products <- gen_query(con,gen_products_query(db_name,all_df))


# Get closets to product amount and installments 
all_df$installments <- products$installments[
  which.min(abs(products$installments - all_df$installments))]
all_df$amount <- products$amount[
  which.min(abs(products$amount - all_df$amount))]


# Check all credits of client
all_credits <- gen_query(con,gen_all_credits_query(db_name,all_df))


# Compute flag repeats
flag_beh <- ifelse(nrow(all_credits)>0,1,0)


# Demographical criteria
all_df <- gen_demographic_stats(all_df)


# Apply scoring model
scoring_df <- gen_apply_score(all_df,products,flag_beh,df_Log_App,df_Log_App)


# Add fields
scoring_df$created_at <- Sys.time()


# Reorder columns 
scoring_df <- scoring_df[,c("loan_id","amount","installments","score",
                            "display_score","color","pd","created_at")]


# Rejoin score for anlysis sake
all_df$flag_beh <- flag_beh
final <- merge(all_df,scoring_df,by.x = c("loan_id","amount","installments"),
   by.y = c("loan_id","amount","installments"),all.x = TRUE)[,c("loan_id",
  "flag_beh","score","display_score","pd","age","gender","ownership",
  "total_work_experience","income")]


# Read and write
file <- paste0("\\\\192.168.2.30\\Analyses\\Shared\\",
 "Scorecards\\Moldova\\Manual_Score\\Manual_Score.xlsx")
final_exists <- read.xlsx(file)
final <- rbind(final_exists,final)
write.xlsx(final,file)





# End