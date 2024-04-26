

################################################################################
#               Joint script for Application and Behavioral scoring            #
#      Apply Logistic Regression on all products (Flexcredit Moldova)          #
#                          Version 1.0 (2023/12/15)                            #
################################################################################



########################
### Initial settings ###
########################

# Library
suppressMessages(suppressWarnings(require(RMySQL)))
suppressMessages(suppressWarnings(require(here)))
suppressMessages(suppressWarnings(require(dotenv)))
suppressMessages(suppressWarnings(require(reshape)))
suppressMessages(suppressWarnings(require(openxlsx)))
suppressMessages(suppressWarnings(require(jsonlite)))
suppressMessages(suppressWarnings(require(stringi)))
suppressMessages(suppressWarnings(require(lubridate)))

# Defines the directory where custom .env file is located
load_dot_env(file = here('.env'))


#########################
### Command arguments ###
#########################

args <- commandArgs(trailingOnly = TRUE)
loan_id <- args[1]
product_id <- args[2]


#######################
### Manual settings ###
#######################

# Defines the directory where the RScript is located
base_dir <- Sys.getenv("RSCRIPTS_PATH", unset = "", names = FALSE)



#####################
####### MySQL #######
#####################

db_host <- Sys.getenv("SCORING_DB_HOST", 
                      unset = "localhost", 
                      names = FALSE)
db_port <- strtoi(Sys.getenv("SCORING_DB_PORT", 
                             unset = "3306", 
                             names = FALSE))
db_name <- Sys.getenv("SCORING_DB_DATABASE", 
                      unset = "citycash", 
                      names = FALSE)
db_username <- Sys.getenv("SCORING_DB_USERNAME", 
                          unset = "root", 
                          names = FALSE)
db_password <- Sys.getenv("SCORING_DB_PASSWORD", 
                          unset = "secret", 
                          names = FALSE)
con <- dbConnect(MySQL(), user=db_username, 
                 password=db_password, dbname=db_name, 
                 host=db_host, port = db_port)

### Mode
sqlMode <- paste("SET sql_mode=''", sep ="")
suppressWarnings(dbSendQuery(con, sqlMode))

### Encoding
suppressWarnings(dbSendQuery(con,
  "SET names 'utf8mb4' COLLATE 'utf8mb4_romanian_ci';"))



#################################
####### Load source files #######
#################################

# Load other r files
source(file.path(base_dir,"Cutoffs.r"))
source(file.path(base_dir,"SQL_Queries.r"))
source(file.path(base_dir,"Demographic_Variables.r"))
source(file.path(base_dir,"Logistic_App_Flexcredit.r"))
source(file.path(base_dir,"Logistic_Beh_Flexcredit.r"))
source(file.path(base_dir,"Misc_Functions.r"))
source(file.path(base_dir,"Generate_Adjust_Score.r"))

######################
####### Models #######
######################

# Load predefined libraries
rdata <- file.path(base_dir, "rdata","flexcredit_app.rdata")
load(rdata)


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


# Update table credits applications scoring
write_sql_query <- paste("
  DELETE FROM ",db_name,".loan_scoring WHERE loan_id=",loan_id, sep="")
suppressMessages(dbSendQuery(con,write_sql_query))
suppressMessages(dbWriteTable(con, name = "loan_scoring", 
    value = scoring_df,field.types = c(loan_id="numeric", amount="integer", 
    installments="integer", score="character(20)",
    color="integer", display_score="character(20)",pd="numeric",
    created_at="datetime"),row.names = F, append = T))


#######
# END #
#######