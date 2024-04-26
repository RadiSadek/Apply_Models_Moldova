
##################################
######## Define SQL queries ######
##################################

# Define big query which reads from credits_applications
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
    ",db_name,".personal_data.sex AS gender,
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
    'App\\\\Models\\\\Clients\\\\Client'", sep="")
  return(big_sql_query)
}

gen_all_credits_query <- function(db_name,all_df){
  return(paste("SELECT id AS loan_id, 
  client_id, master_client_id, 
  status, created_at, activated_at, 
  amount, finished_at AS deactivated_at, 
  installments, product_id FROM ",db_name,".loans 
  WHERE master_client_id=",all_df$master_client_id,"
  AND id < ",all_df$loan_id,"
  AND status IN (9,10,11,12);", sep =""))
}

# Define query for products periods and amounts
gen_products_query <- function(db_name,all_df){
  return(paste("SELECT * FROM ", db_name, ".product_amounts_and_installments
                WHERE product_id IN (",all_df$product_id, ")", sep=""))
}

# Get client income
gen_income_query <- function(db_name,all_df){
  return(paste("SELECT incomeable_id AS client_id, amount AS income 
    FROM ",db_name,".client_incomes WHERE incomeable_id = ",
    all_df$master_client_id,"
    AND incomeable_type = 'App\\\\Models\\\\Clients\\\\Client'",sep=""))
}

