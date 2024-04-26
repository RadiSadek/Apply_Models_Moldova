
##############################################################################
######## Functions to apply logisit regression on application City Cash ######
##############################################################################

gen_app_flex <- function(all_df,products,model){
  
  scoring_df<-gen_scoring_table(all_df,products)
  
  # Cut and bin
  all_df$age_group <-
    ifelse(is.na(all_df$age),"18-25",
    ifelse(all_df$age<=25,"18-25",
    ifelse(all_df$age<=50,"26-50","51+")))
  
  all_df$marital_status_group <-
    ifelse(is.na(all_df$marital_status),"Married/Widowed",
    ifelse(all_df$marital_status %in% c(1,4,5),"Married/Widowed",
    ifelse(all_df$marital_status %in% c(2),"Single","Divorced")))
  
  # Apply logisic regression
  scoring_df$pd<-predict(model, newdata=all_df, type="response")
  
  # Adjust pd for close values (to be removed for later iterations)
  scoring_df$pd<-ifelse(scoring_df$pd>0.386&scoring_df$pd<0.3867,0.386,
                        round(scoring_df$pd,3))
  
  # Get score
  scoring_df$score <- gen_group_scores(scoring_df$pd,0)
  
  # Color
  scoring_df$color <- 0
  scoring_df$color <- ifelse(scoring_df$color==1 | scoring_df$score=="Bad", 2, 
                      ifelse(scoring_df$score=="Indeterminate", 3,
                      ifelse(scoring_df$score=="Good 1", 3,
                      ifelse(scoring_df$score=="Good 2", 4,
                      ifelse(scoring_df$score=="Good 3", 5,
                      ifelse(scoring_df$score=="Good 4", 6, NA))))))
  return(scoring_df)
}
