
###################################################
######## Define some extra help functions  ########
###################################################

# Define function to generate query
gen_query <- function(con,input){
  return(suppressWarnings(fetch(dbSendQuery(con,input),n=-1)))
}

## scoring table
gen_scoring_table <- function(all_df,products){
  scoring_df <- products[,c("amount","installments","id")]
  names(scoring_df)<-c("amount","installments","loan_id")
  for(var in names(scoring_df)){
    if(is.factor(scoring_df[,var])){
      scoring_df[,var]<-as.numeric(levels(scoring_df[,var]))
    }
  }
  scoring_df$loan_id<-all_df$loan_id
  return(scoring_df[which(scoring_df$amount<=all_df$amount),
                    c("loan_id","amount","installments")])
}

## pd to score
gen_group_scores <- function(var,flag_beh){
  
  if(flag_beh==1){
    cutoffs <- cu_app
  } else {
    cutoffs <- cu_rep
  }
  
  output<-ifelse(var>cutoffs[1],"Bad",
          ifelse(var>cutoffs[2],"Indeterminate",
          ifelse(var>cutoffs[3],"Good 1",
          ifelse(var>cutoffs[4],"Good 2",
          ifelse(var>cutoffs[5],"Good 3","Good 4")))))
  return (output)
}