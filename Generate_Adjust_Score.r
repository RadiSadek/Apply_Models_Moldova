
#######################################################
######## Apply logistic models and policy rule ########
#######################################################

# Function to apply scoring model 
gen_apply_score <- function(all_df,products,flag_beh,model_app,model_beh){
  
  # Apply model coefficients according to type of credit 
  if (flag_beh==1){
    model <- model_beh
    scoring_df<-gen_beh_flex(all_df,products,model)
  } else {
    model <- model_app
    scoring_df<-gen_app_flex(all_df,products,model)
  }
  
  return(scoring_df)
}