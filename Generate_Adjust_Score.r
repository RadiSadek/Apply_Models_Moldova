
#######################################################
######## Apply logistic models and policy rule ########
#######################################################

# Function to apply scoring model 
gen_apply_score <- function(all_df,products,flag_beh){
  
  # Apply model coefficients according to type of credit 
  if (flag_beh==1){
    model<-app_beh_model
    scoring_df<-gen_beh_flex(all_df,products,model)
  } else {
    model<-app_beh_model
    scoring_df<-gen_app_flex(all_df,products,model)
  }
  scoring_df$display_score<-ifelse(scoring_df$score=="Bad",
                                   "Indeterminate",scoring_df$score)
  
  return(scoring_df)
}