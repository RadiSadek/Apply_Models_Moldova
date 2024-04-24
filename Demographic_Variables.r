
##############################################
######## Define demographic variables ########
##############################################

# validate birth date
check_valid_birthday<-function(birthday){
  result<-as.character(NA)
  if(!is.na(birthday)){
    result<-paste(ifelse(substr(birthday,1,2)%in%c("19","20"),
                         substr(birthday,1,2),"19"),substr(birthday,3,10),sep="")
  }
  return(result)
}

gen_gender_by_name<-function(name){
  return(ifelse(name %in% c("Mircea","Mihnea","Nikola","Nicola"),"M",
         ifelse(name %in% c("Carmen"),"F",
         ifelse(stri_sub(name,-1,-1)=="a","F","M"))))
}

# Function to generate main demographic fields
gen_demographic_stats<-function(all_df){
  # age
  all_df$birth_date<-check_valid_birthday(all_df$birth_date)
  all_df$age<-year(Sys.Date())-year(all_df$birth_date)+
    ifelse(substr(all_df$birth_date,6,10)<=substr(Sys.Date(),6,10),0,-1)
  # gender
  if((!"gender" %in% names(all_df))){
    all_df$gender<-gen_gender_by_name(all_df$first_name)
  } else if(is.na(all_df$gender)){
    all_df$gender<-gen_gender_by_name(all_df$first_name)
  }
  return(all_df)
}