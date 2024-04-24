

################################################################################
#               Joint script for Application and Behavioral scoring            #
#      Apply Logistic Regression on all products (Flexcredit Moldova)          #
#                          Version 1.0 (2023/12/15)                            #
################################################################################



########################
### Initial settings ###
########################

# Library
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(require(jsonlite)))
suppressMessages(suppressWarnings(require(stringi)))
suppressMessages(suppressWarnings(require(lubridate)))

# Database
db_user <- "root"
db_password <- "123456"
db_name <- "moldova"
db_host <- "127.0.0.1"
df_port <- 3306
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)


# Define work directory
main_dir <- "C:\\Users\\nlangov\\Documents\\Moldova\\Scoring\\Scorecard\\"


# Read argument of ID
args <- commandArgs(trailingOnly = TRUE)
loan_id <- args[1]
loan_id <- 12789
# loan_id<-107
# loan_id<-6
loan_id<-343
product_id <- NA


# Set working directory for input (R data for logistic regression) and output #
setwd(main_dir)
source(paste(main_dir,"Cutoffs.r", sep=""))
source(paste(main_dir,"SQL_queries.r", sep=""))
source(paste(main_dir,"Demographic_Variables.r", sep=""))
source(paste(main_dir,"Logistic_App_Flexcredit.r", sep=""))
source(paste(main_dir,"Logistic_Beh_Flexcredit.r", sep=""))
source(paste(main_dir,"Misc_Functions.r", sep=""))
source(paste(main_dir,"Generate_Adjust_Score.r", sep=""))

# Load modeling libraries
load("rdata\\app_beh_model.rdata")



gen_big_sql_query <- function(db_name,loan_id){
  big_sql_query <- paste("SELECT
    ",db_name,".loans.id AS loan_id,
    ",db_name,".loans.amount,
    ",db_name,".loans.master_client_id,
    ",db_name,".loans.installments,
    ",db_name,".loans.created_at,
    ",db_name,".loans.product_id,
    ",db_name,".loans.office_id,
    ",db_name,".clients.marital_status,
    ",db_name,".clients.ownership, 
    ",db_name,".clients.on_address,
    ",db_name,".clients.household_total,
    ",db_name,".clients.household_children,
    ",db_name,".clients.education,
    ",db_name,".personal_data.cnp,
    ",db_name,".personal_data.birth_date,
    ",db_name,".personal_data.middle_name AS first_name,
    ",db_name,".personal_data.first_name AS family_name,
    ",db_name,".client_employer.total_work_experience,
    ",db_name,".client_employer.work_experience
    FROM ",db_name,".loans
    LEFT JOIN ",db_name,".clients
    ON ",db_name,".loans.master_client_id = ",db_name,
                         ".clients.id
    LEFT JOIN ",db_name,".client_employer
    ON ",db_name,".loans.master_client_id = ",db_name,
                         ".client_employer.client_id
    LEFT JOIN ",db_name,".personal_data
    ON ",db_name,".loans.master_client_id = ",db_name,
                         ".personal_data.personable_id
    WHERE loans.id =", loan_id," 
    AND personal_data.personable_type = 
    'App\\\\Models\\\\Clients\\\\Client';", sep="")
  return(big_sql_query)
}

####################################
### Read database and build data ###
####################################

# Read credits applications
all_df <- suppressWarnings(dbFetch(dbSendQuery(con, 
                    gen_big_sql_query(db_name,loan_id)), n=-1))

# Apply some checks to main credit dataframe
if(!is.na(product_id)){
  all_df$product_id <- product_id
}
if(nrow(all_df)>1){
  all_df <- all_df[!duplicated(all_df$application_id),]
}

# Read product's periods and amounts
products  <- suppressWarnings(dbFetch(dbSendQuery(con, 
                      gen_products_query(db_name,all_df)), n=-1))

# Get closets to product amount and installments 
all_df$installments <- products$installments[
  which.min(abs(products$installments - all_df$installments))]
all_df$amount <- products$amount[
  which.min(abs(products$amount - all_df$amount))]


# Check all credits of client
all_credits <- suppressWarnings(dbFetch(dbSendQuery(con, 
                        gen_all_credits_query(db_name,all_df)), n=-1))

# Compute flag repeats
flag_beh <- ifelse(nrow(all_credits)>0,1,0)


# Demographical criteria
all_df<-gen_demographic_stats(all_df)

# Apply scoring model
scoring_df<-gen_apply_score(all_df,products,flag_beh)

# Apply policy rules (to be included in later version)
scoring_df$display_score<-"Yes"
scoring_df$created_at<-Sys.time()

# Reorder columns 
scoring_df <- scoring_df[,c("loan_id","amount","installments","score",
                            "display_score","color","pd","created_at")]