
##############################################################################
######## Functions to apply logisit regression on application City Cash ######
##############################################################################

gen_app_flex <- function(df,products,model){
  
  scoring_df<-gen_scoring_table(all_df,products)
  
  # Cut and bin
  df$age_cut <- 
    ifelse(is.na(df$age),"31_47",
    ifelse(df$age<=30,"18_30",
    ifelse(df$age<=47,"31_47","48+")))
  df$age <- as.factor(df$age_cut)
  
  df$ownership_cut <- 
    ifelse(is.na(df$ownership),"other",
    ifelse(df$ownership==2,"2",
    ifelse(df$ownership!=2,"other","other")))
  df$ownership <- as.factor(df$ownership_cut)
  
  df$gender <- as.factor(df$gender)
  
  df$marital_status_cut <- 
    ifelse(is.na(df$marital_status),"other",
    ifelse(df$marital_status %in% c(1,2),"other",
    ifelse(df$marital_status %in% c(5),"5","other")))        
  df$marital_status <- as.factor(df$marital_status_cut)
  
  df$income_cut <- 
    ifelse(is.na(df$income),"0_5000",
    ifelse(df$income<=5000,"0_5000",
    ifelse(df$income<=8000,"5000_8000","8000+")))
  df$income <- as.factor(df$income_cut)
  
  # Apply logisic regression
  scoring_df$pd <- predict(df_Log_App,newdata=df,type="response")

  # Get score
  scoring_df$score <- gen_group_scores(scoring_df$pd,0)
  
  # Color
  scoring_df$color <- 0
  scoring_df$color <- ifelse(scoring_df$color==1 | scoring_df$score=="Bad", 1, 
                      ifelse(scoring_df$score=="Indeterminate", 2,
                      ifelse(scoring_df$score=="Good 1", 3,
                      ifelse(scoring_df$score=="Good 2", 4,
                      ifelse(scoring_df$score=="Good 3", 5,
                      ifelse(scoring_df$score=="Good 4", 6, NA))))))
scoring_df$display_score <- 
  ifelse(is.na(scoring_df$color),"NULL",
  ifelse(scoring_df$score=="NULL","NULL",
  ifelse(scoring_df$color==1,"No","Yes")))
  return(scoring_df)
}
