library(readxl)      #for excel, csv sheets manipulation
library(sdcMicro)    #sdcMicro package with functions for the SDC process 
library(tidyverse)   #for data cleaning

#Import data
setwd("C:/Users/LENOVO T46OS/Desktop/SDC-Somalia-Reach-Initiative")
file <- read_excel("C:/Users/LENOVO T46OS/Desktop/SDC-Somalia-Reach-Initiative/data.xlsx")
                   
#Somalia Joint Multi Cluster Needs Assessment Final Dataset - https://data.humdata.org/dataset/reach-somalia-joint-multi-cluster-needs-assessment-final-dataset
selectedKeyVars <- c('idp_settlement',  'resp_gender',
                     'breadwinner',	'household_expenditure', 'hh_children',
                     'settlement_idp', 'arrived_current','girls_harsh_work_type',
                     'boys_harsh_work_type', 'not_safe_men_boys_where_other',
                     'not_safe_women_girls', 'not_safe_women_girls_where',
                     'separated_members','separations_number',	
                     'separated_boys', 'separated_girls',	
                     'separated_male_assets', 'separated_female_assets',	
                     'separated_male_other','separated_female_other',
                     'separation_circumstances', 'sep_accidental',
                     'sep_accidental.acc_conflict',	'sep_accidental.acc_displacement',	
                     'sep_accidental.other', 'sep_accidental.dontknow',	
                     'sep_voluntary','sep_voluntary.vol_violence',
                     'sep_voluntary.vol_caregiver','sep_voluntary.vol_reunification',
                     'sep_voluntary.other','sep_voluntary.dontknow',
                     'sep_voluntary.vol_assistance', 'sep_voluntary.vol_other',	
                     'sep_forced',	'sep_forced.for_abducted',
                     'sep_forced.for_groups',	'sep_forced.for_marriage',
                     'sep_forced.other', 'sep_forced.dontknow', 'not_safe_men_boys',
                     'not_safe_men_boys_where', 'not_safe_women_girls_where',
                     'health_issues_adult', 'child_feeding_6months',	
                     'child_feeding_over6m','language_spoken', 'Target.Site')

pramvars <-c('males_0_6m',	'females_0_6m', 'males_6m_4y', 'females_6m_4y',
            'males_5_12','females_5_12', 'males_13_15', 'females_13_15',
            'males_16_17', 'females_16_17','males_18_40','females_18_40',
            'males_41_59', 'females_41_59',	'males_60_over',
            'females_60_over', 'total_hh', 'children_vaccine_age', 'children_0_4',
            'school_age_male','school_age_female', 'school_age_total', 'total_children', 
            'disabled_chronic',	'sick_children', 'mental', 'note_vuln_gender',
            'disabled_chronic_male', 'disabled_chronic_female',	
            'sick_boys_under5',	'sick_girls_under5',	'stress_boys',
            'stress_girls','stress_men','stress_women','region',
            'district','settlement','settlement_other', 'idp_settlement_name', 
            'returnee_country', 'returnee_country_other',	'returnee_area',
            'returnee_area_other', 'returnee_settlement',
            'returnee_settlement_other',	'registered_return',
            'refugee',	'country_origin')

#Convert variables into factors
factor_cols = c('resp_gender',	'resp_age', 'breadwinner',	'household_expenditure',
                'males_0_6m',	'females_0_6m', 'males_6m_4y', 'females_6m_4y',
                'males_5_12','females_5_12', 'males_13_15', 'females_13_15',
                'males_16_17', 'females_16_17','males_18_40','females_18_40',
                'males_41_59', 'females_41_59',	'males_60_over',
                'females_60_over', 'total_hh', 'children_vaccine_age', 
                'children_0_4','school_age_male','school_age_female',
                'school_age_total', 'total_children', 'disabled_chronic',	
                'sick_children', 'mental', 'note_vuln_gender',
                'disabled_chronic_male', 'disabled_chronic_female',	
                'sick_boys_under5',	'sick_girls_under5',	'stress_boys',
                'stress_girls','stress_men','stress_women','region',
                'district','settlement','settlement_other', 'idp_settlement_name', 
                'returnee_country', 'returnee_country_other',	'returnee_area',
                'returnee_area_other', 'returnee_settlement',
                'returnee_settlement_other',	'registered_return',
                'refugee',	'country_origin')
file[,factor_cols] <- lapply(file[,factor_cols], factor)

#Subset fil
subvars <- c('id', pramvars, selectedKeyVars)

# Convert the sub file into dataframe
fileRes<-file[,subvars]
#subvars <- subvars[which(!duplicated(subvars$id)),] 
fileRes <- as.data.frame(fileRes)

#Assess the disclosure risk
objSDC <- createSdcObj(dat = fileRes, keyVars = selectedKeyVars, pramVars = pramvars )

#Apply PRAM
set.seed(123)
sdcObj <- pram(objSDC)
table(objSDC@pramVars)

#Appply local suppression to gender, breadwinner and houshold_expenditure variables
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'household_expenditure')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'resp_gender')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'girls_harsh_work_type')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'boys_harsh_work_type')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'not_safe_men_boys_where_other')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'not_safe_women_girls')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'health_issues_adult')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'not_safe_women_girls_where')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'breadwinner')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'separated_female_assets')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'separation_circumstances')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'settlement_idp')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'arrived_current')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'hh_children')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'separations_number')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'separated_members')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'separated_boys')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'separated_girls')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'separated_male_assets')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'separated_male_other')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'separated_female_other')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_accidental')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_accidental.acc_conflict')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_accidental.acc_displacement')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_accidental.other')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_accidental.dontknow')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_voluntary')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_voluntary.vol_violence')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_voluntary.vol_caregiver')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_voluntary.vol_reunification')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_voluntary.other')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_voluntary.dontknow')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_voluntary.vol_assistance')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_voluntary.vol_other')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_forced')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_forced.for_abducted')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_forced.for_groups')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_forced.for_marriage')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_forced.other')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'sep_forced.dontknow')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'not_safe_men_boys')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'child_feeding_6months')	
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'child_feeding_over6m')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'language_spoken')
objSDC<- localSupp(objSDC, threshold = 0.3, keyVar = 'Target.Site')

# Extract and store anonymized data
dataAnon <- extractManipData(objSDC)
fileUnanonymized<-file
fileUnanonymized[,c('id','males_0_6m','females_0_6m','males_6m_4y','females_6m_4y',
                    'males_5_12','females_5_12','males_13_15','females_13_15',
                    'males_16_17','females_16_17','males_18_40','females_18_40',
                    'males_41_59','females_41_59','males_60_over','females_60_over',
                    'total_hh', 'children_vaccine_age', 'children_0_4',
                    'school_age_male','school_age_female','school_age_total', 
                    'total_children','disabled_chronic','sick_children', 'mental', 
                    'note_vuln_gender','disabled_chronic_male', 'disabled_chronic_female',	
                    'sick_boys_under5','sick_girls_under5','stress_boys',
                    'stress_girls','stress_men','stress_women','region',
                    'district','settlement','settlement_other', 'idp_settlement_name', 
                    'returnee_country','returnee_country_other','returnee_area',
                    'returnee_area_other','returnee_settlement',
                    'returnee_settlement_other','registered_return',
                    'refugee','country_origin','idp_settlement','resp_gender',
                    'breadwinner','household_expenditure','hh_children',
                    'settlement_idp','arrived_current','girls_harsh_work_type',
                    'boys_harsh_work_type','not_safe_men_boys_where_other',
                    'not_safe_women_girls','not_safe_women_girls_where',
                    'separated_members','separations_number',	
                    'separated_boys','separated_girls',	
                    'separated_male_assets','separated_female_assets',	
                    'separated_male_other','separated_female_other',
                    'separation_circumstances','sep_accidental',
                    'sep_accidental.acc_conflict','sep_accidental.acc_displacement',	
                    'sep_accidental.other','sep_accidental.dontknow',	
                    'sep_voluntary','sep_voluntary.vol_violence',
                    'sep_voluntary.vol_caregiver','sep_voluntary.vol_reunification',
                    'sep_voluntary.other','sep_voluntary.dontknow',
                    'sep_voluntary.vol_assistance', 'sep_voluntary.vol_other',	
                    'sep_forced',	'sep_forced.for_abducted',
                    'sep_forced.for_groups','sep_forced.for_marriage',
                    'sep_forced.other', 'sep_forced.dontknow', 'not_safe_men_boys',
                    'not_safe_men_boys_where','not_safe_women_girls_where',
                    'health_issues_adult','child_feeding_6months',	
                    'child_feeding_over6m','language_spoken','Target.Site')]<-list(NULL)
fileCombined<-bind_cols(x=dataAnon, y=fileUnanonymized)
write.csv(fileCombined,'Somalia_Joint_Multi_Cluster_Needs_Assessment_Final_anonymized_dataset.csv') 

#Disclosure risk re-assessment 
print(objSDC, "risk")

#Generating an internal (extensive) report
report(objSDC, filename = "index", internal = TRUE) 

# Check the number of missing values (NA) after local suppression application
namesKeyVars<- names(objSDC@manipKeyVars) 
NAcount <- matrix(NA, nrow = 2, ncol = length(namesKeyVars)) 
colnames(NAcount)  <- c(paste0('NA', namesKeyVars))
rownames(NAcount)  <- c('initial', 'treated')
for(i in 1:length(namesKeyVars)) 
{
  NAcount[1, i] <- sum(is.na(objSDC@origData[,namesKeyVars[i]]))
  NAcount[2, i] <- sum(is.na(objSDC@manipKeyVars[,i]))
}   
NAcount

#Assess number of records changed for the PRAMmed variables
namesPramVars <- names(objSDC@manipPramVars) 
recordChanged<- rep(0, length(namesPramVars))  
names(recordChanged)  <- c(paste0('RC', namesPramVars)) 
for(j in 1:length(namesPramVars)) 
{comp <- objSDC@origData[namesPramVars[j]] != objSDC@manipPramVars[namesPramVars[j]] 
temp1 <- sum(comp, na.rm = TRUE)
temp2 <- sum(is.na(comp)) 
temp3 <- sum(is.na(objSDC@origData[namesPramVars[j]]) +
               is.na(objSDC@manipPramVars[namesPramVars[j]])==2)
recordChanged[j] <- temp1 + temp2 - temp3 
}
recordChanged