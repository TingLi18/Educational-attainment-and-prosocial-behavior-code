# Educational-attainment-and-prosocial-behavior-code
Educational-attainment-and-prosocial-behavior-code
## preparing programming
library(foreign)

library(psych)

library(GPArotation)

library(dplyr) 

library(lavaan)

library(readr)

library(corrplot)

library(RColorBrewer)

library(tidyverse)

library(naniar)

library(Hmisc)

library(tidySEM)

library(ppcor)
#### preparing the dataset
setwd("C:/Ting/Ting_2021_work/Other_cohorts_for_Prosocial_behavior/WLS/R_analysis/prosocial_behavior")


#### read WLS cohort version 13_08
WLS<-read.spss("wls_bl_13_08.sav", use.value.labels=F, to.data.frame=TRUE)

dim(WLS)

#####################----------------------------------------------------####################################################################
##### select variables in R5. Variables related to education, volunteering behavior, social participantion, self-rated subjective health ####. 
#####################----------------------------------------------------####################################################################

names(WLS)
dput(names(WLS))
va<-list()
va$items<-c("idpub", "selsibidpub","rtype", "personid","selsibtype", "z_brdxdy", "z_sexrsp","sibcount","edfa57q","edmo57q", 
 "z_ga003re", "z_gb103red", "z_gb104red", "z_gb005rec","z_gb007rec", "z_gc001re", "z_ic001rer","z_gx201re","z_gp226re","z_gx201re","z_gr020rp",
 "z_gv111re", "z_gv112rea","z_gv114re", "z_gv001res","z_gv020re", "z_gv101re","z_gv102re", "z_iv001re", "z_iv002re","z_iv003re", "z_iv004re",
 "z_iv005re","z_iv101rer","z_iv006re", "z_iv007re", "z_iv008re","z_iv009re", "z_iv010re", "z_iv011re", "z_iv102rer","z_iv012re","z_iv013re",
 "z_iv014re", "z_iv015re","z_iv016re",  "z_iv017re","z_iv103rer","z_iv018re","z_iv019re","z_iv020re","z_iv021re","z_iv022re","z_iv023re","z_iv104rer",
  "z_iv024re","z_gv104re", "z_gv105re","z_gv106re","z_gv107re","z_gv108re","z_gv109re","z_gv110rea","z_gv115re",
  "z_gv116re","z_gv117re","z_gv118re","gv112reb","gv113re","z_iv201rer", "z_gq700r", "z_gq701r", "gv112reb","z_gv110rea")
  
R5_screening = WLS[,va$items]
dim(R5_screening)
summary(R5_screening)
#####################----------------------------------------------------####################################################################
#### Descriptive analysis of the dataset
#####################----------------------------------------------------####################################################################
#### Select interesting variables: 
## idpub        ID
## z_ga003re   R5 Respondent's age at time of interview; 
## z_sexrsp    RS Sex of participant; 
### Education
## z_gb103red	 R5 How many years of education does R have based on his or her highest degree?
## z_gx201re   R5 In general, would you say your health is excellent, very good, good, fair, or poor?
## z_gp226re   R5 To what extent are you satisfied with your present financial situation?
## z_gr020rp	 R5 Do you own your own home, or are you renting?

### General information of volunteering behavior
## z_gv101re	R5 Did R do volunteer work in last 10 years?
## z_gv111re	R5 How many hours did participant volunteer during a typical month in the last 12 months? 
## gv113re	  R5 Number of hours per month graduate spent volunteering in a typical month during the last year.  
## z_gv102re	R5 How regularly did graduate volunteer in the last 10 years?
## gv112reb	  R5 For seasonal volunteers, typically how many hours did graduate spend volunteering in the last 12 months?
### Blood donation 
## z_gv115re	R5 Has R ever given a unit of blood for own use?
## z_gv116re	R5 Has R ever given blood for use by others?
## z_gv117re	R5 How many times has R given blood for use by others (over R's lifetime)?
## z_gv118re	R5 Has R given blood in the last 12 months (for use by others)?
#############################------------------------------------------------#####################################3
#### data cleaning  
### rename the variables

R5_volunteering<-R5_screening

R5_volunteering<-R5_volunteering%>%
  rename(
    age_at_R5 = z_ga003re,
    marrige_state=z_ic001rer,
    gender = z_sexrsp,
    Education_mother = edmo57q,
    Education_father = edfa57q, 
    Financial_Satisfaction = z_gp226re,
    Home_ownship = z_gr020rp,
    Years_of_Education = z_gb103red,
    Health_statement = z_gx201re,
    Ever_volunteering = z_gv101re,
    Regular_Volunteer_last_10_years = z_gv102re,
    Hours_Volunteering = z_gv111re,
    Hours_Volunteering_1_year = gv112reb,
    Hours_Volunteering_1_month = gv113re,	
    Ever_blood_donation = z_gv116re, 
    Blood_donation_last_year = z_gv118re,
    Blood_for_own_use = z_gv115re,
    Number_blood_donation = z_gv117re,
    Ever_give_care = z_gv001res,
    On_personal_care = z_gv020re,
    Ever_help_behavior = z_iv001re,
    Ever_house_work_help = z_iv007re,
    Ever_emotional_support = z_iv013re,
    Ever_babysitting = z_iv019re,
    Types_of_volunteering = z_gv110rea,
    Charitable_contribution  = z_gq700r,
    Amount_of_charitable_contribution = z_gq701r,
  )
#####################----------------------------------------------------####################################################################
###  Remove missing varibles

vec <- c(-1, -2, -3, -4, -5)
R5_volunteering<- R5_volunteering %>% replace_with_na_all(condition = ~.x %in% vec)

#####################----------------------------------------------------####################################################################
#### Reverse the coding 

R5_volunteering = R5_volunteering %>%
  mutate_at(c( "marrige_state","Ever_volunteering", "Ever_blood_donation", "Blood_for_own_use",
               "Blood_donation_last_year", "Ever_give_care", "On_personal_care",
               "Ever_help_behavior","z_iv002re","z_iv003re","z_iv004re","z_iv005re","z_iv101rer","z_iv006re",   
               "Ever_house_work_help","z_iv008re","z_iv009re","z_iv010re","z_iv011re","z_iv102rer","z_iv012re",
               "Ever_emotional_support","z_iv014re", "z_iv015re", "z_iv016re", "z_iv017re" , "z_iv103rer","z_iv018re",
               "Ever_babysitting","z_iv020re","z_iv021re","z_iv022re","z_iv023re", "z_iv104rer","z_iv024re",
               "z_gv104re","z_gv105re","z_gv106re","z_gv107re","z_gv108re","Charitable_contribution","Amount_of_charitable_contribution"),
            ~ recode(., "1" = 1, "2" = 0 ))

R5_volunteering = R5_volunteering %>%
  mutate_at(c( "Health_statement","Financial_Satisfaction"),
            ~ recode(., "1" = 5, "2" = 4, "3" = 3, 
                     "4" = 2, "5" = 1))
R5_volunteering = R5_volunteering %>%
  mutate_at(c( "Home_ownship"),
            ~ recode(.,  "2" = 0, "3" = 0, 
                     "4" = 0, "5" = 0, "6" = 0 , "7"=0, "8"=0,"9"=0, "19"=0,"20"=0))

#####################----------------------------------------------------####################################################################
##### The informal prosocial beahavior

attach(R5_volunteering)

R5_volunteering$Index_help_behavior<-z_iv002re+z_iv003re+z_iv004re +z_iv005re+z_iv101rer+ z_iv006re

R5_volunteering$Index_house_work_help<-z_iv008re+z_iv009re+ z_iv010re +z_iv011re+z_iv102rer+ z_iv012re

R5_volunteering$Index_emotional_support<-z_iv014re+z_iv015re+z_iv016re + z_iv017re+z_iv103rer+z_iv018re

R5_volunteering$Index_babysitting<-z_iv020re+z_iv021re+z_iv022re+z_iv023re+z_iv104rer+z_iv024re

R5_volunteering$Index_organization<-z_gv104re+z_gv105re+z_gv106re+z_gv107re+z_gv108re

detach(R5_volunteering)

#####################----------------------------------------------------####################################################################

### transform the respondents who never donate blood/ involved in help/emotional support/ house_work_help/ babysitting as 0 


R5_volunteering<-transform(R5_volunteering, Blood_donation_last_year = ifelse(Ever_blood_donation == 0, 0, Blood_donation_last_year))
R5_volunteering<-transform(R5_volunteering, Number_blood_donation = ifelse(Ever_blood_donation == 0, 0, Number_blood_donation))
R5_volunteering<-transform(R5_volunteering, On_personal_care = ifelse(Ever_give_care== 0, 0, On_personal_care))
R5_volunteering<-transform(R5_volunteering, Index_help_behavior = ifelse(Ever_help_behavior == 0, 0, Index_help_behavior))
R5_volunteering<-transform(R5_volunteering, Index_house_work_help = ifelse(Ever_house_work_help == 0, 0, Index_house_work_help))
R5_volunteering<-transform(R5_volunteering, Index_emotional_support = ifelse (Ever_emotional_support == 0, 0, Index_emotional_support))
R5_volunteering<-transform(R5_volunteering, Index_babysitting = ifelse(Ever_babysitting == 0, 0, Index_babysitting))
R5_volunteering<-transform(R5_volunteering, Amount_of_charitable_contribution  = ifelse(Charitable_contribution == 0, 0, Amount_of_charitable_contribution))
R5_volunteering<-transform(R5_volunteering, Index_organization  = ifelse(Ever_volunteering == 0, 0, Index_organization))

#####################----------------------------------------------------####################################################################
#### remove some variables without renamed
R5_volunteering <- select(R5_volunteering, idpub, rtype, gender, Education_father, Education_mother, age_at_R5,Years_of_Education,           
                          marrige_state, Health_statement,Financial_Satisfaction, Home_ownship, Hours_Volunteering,  Ever_give_care,
                          On_personal_care, Ever_volunteering, Regular_Volunteer_last_10_years, 
                          Ever_help_behavior,Ever_house_work_help,Ever_emotional_support, Ever_babysitting,                      
                          Types_of_volunteering,Blood_for_own_use,  Ever_blood_donation,Number_blood_donation,Blood_donation_last_year,Hours_Volunteering_1_year,
                          Hours_Volunteering_1_month, Charitable_contribution, Amount_of_charitable_contribution, Index_help_behavior,Index_house_work_help,
                          Index_emotional_support,Index_babysitting,Index_organization )
#####################----------------------------------------------------####################################################################
##### Description of variables
dim(R5_volunteering)
summary(R5_volunteering)

hist(R5_volunteering$Years_of_Education,xlab = "The years of education",col="orange",ylim = c(0,3000),xlim = c(8,21),main="Educational attainment")
hist(R5_volunteering$Education_mother,xlab = "The years of education",col="orange",ylim = c(0,3000),xlim = c(6,18),main="Mothers' education")
hist(R5_volunteering$Education_father,xlab = "The years of education",col="orange",ylim = c(0,3000),xlim = c(6,18),main="Fathers' education")


##################-------------------------------#################################

#### read Polygenic score for education file from WLS Lee 
Educational_PRS<-read_tsv("Lee_idpub_shuffled.txt", col_names = TRUE)
dim(Educational_PRS)

#####################----------------------------------------------------####################################################################
#### Select the participants Graduators, not the sibling
Graduate_Educational_PRS<-Educational_PRS[which(Educational_PRS$rtype=="g"),]
dim(Graduate_Educational_PRS)

Graduate_R5_volunteering<-R5_volunteering[which(R5_volunteering$rtype=="1"),]
dim(Graduator_R5_volunteering)

###### Merging two files R5_G and Educational_PRS_G file for Graduates
Graduate_volunteering_PRS <- merge(Graduator_R5_volunteering,Graduate_Educational_PRS,by=c("idpub"))
dim(Graduate_volunteering_PRS)

##########################---------------------------##########################################3

#### Select the Siblings, not the graduate
Siblings_Educational_PRS<-Educational_PRS[which(Educational_PRS$rtype=="s"),]
dim(Siblings_Educational_PRS)

Siblings_R5_volunteering<-R5_volunteering[which(R5_volunteering$rtype=="2"),]
dim(Siblings_R5_volunteering)

###### Merging two files R5_G and Educational_PRS_G file for Graduates
Siblings_volunteering_PRS <- merge(Siblings_R5_volunteering,Siblings_Educational_PRS,by=c("idpub"))
dim(Siblings_volunteering_PRS)
#######################---------------------------------------------------------------------------------------

##### Prosocial behavior formal and informal format #######################################################
###################  Model for listwise deletion n = 2592 #### 

Graduate_prosocial_behavior<-Graduate_volunteering_PRS[, c(1,3,4,5,6,7,9,10,11,15,23,28,30,31,32,33,34,36:51)]

Graduate_prosocial_behaviors <- Graduate_prosocial_behavior[complete.cases(Graduate_prosocial_behavior), ]
dim(Graduate_prosocial_behaviors)

################### Analysis_for n=2592 ##################################################################
################ standardized the polygenic score of educational attainment ###############################

Graduate_prosocial_behaviors$Educational_PS_standardized <-(Graduate_prosocial_behaviors$Educational_PS-mean(Graduate_prosocial_behaviors$Educational_PS))/sd(Graduate_prosocial_behaviors$Educational_PS)

### descriptive analysis 
describe(Graduate_prosocial_behaviors)
hist(Graduate_prosocial_behaviors$Years_of_Education,xlab = "The years of education",col="orange",ylim = c(0,3000),xlim = c(12,21),main="Educational attainment")
hist(Graduate_prosocial_behaviors$Educational_PS,col="orange",ylim = c(0,800),xlim = c(-0.6, 0.4),main="Educational polygenic score")
hist(Graduate_prosocial_behaviors$Education_mother,xlab = "The years of education",col="orange",ylim = c(0,3000),xlim = c(6,18),main="Mothers' education")
hist(Graduate_prosocial_behaviors$Education_father,xlab = "The years of education",col="orange",ylim = c(0,3000),xlim = c(6,18),main="Fathers' education")

### build SEM model two factors model
m1_formal_informal <-
  '
#measurement model
Informal_behavior =~ Index_help_behavior+Index_house_work_help+Index_emotional_support+Index_babysitting
Formal_behavior =~ Charitable_contribution + Ever_blood_donation + Index_organization
# regressions
Prosocial_behavior =~ Formal_behavior+Informal_behavior' 

fit_formal_informal <- sem(m1_formal_informal, data = Graduate_prosocial_behaviors)

summary(fit_formal_informal, standardized=TRUE, fit.measures=TRUE)
### save the latent factor for Formal and informal prosocial behavior B version
FormalInformalPredict_b<- as.data.frame(predict(fit_formal_informal))
Graduate_prosocial_behaviors<-cbind(Graduate_prosocial_behaviors, FormalInformalPredict_b)

### visuliazation 
graph_sem(fit_formal_informal,rect_width = 1.6,variance_diameter = 0.3)

### build SEM model one factor model
model2 <-'
# regressions
Prosocial_behavior_One_factor =~ Charitable_contribution + Ever_blood_donation + Index_organization
                     +Index_help_behavior+Index_house_work_help+Index_emotional_support+Index_babysitting
' 
fit_model2<- sem(model2, data = Graduate_prosocial_behaviors)
summary(fit_model2, standardized=TRUE, fit.measures=TRUE)

#####################----------------------------------------------------####################################################################
### correlation analysis between the participants' educational attainment, Educational-PS, formal and informal prosocial behavior 
mat<- Graduate_prosocial_behaviors[, c( 37,2,5,6,3,4,7,8,9,36,35,34)]
mydata.rcorr = rcorr(as.matrix(mat))
mydata.rcorr
corrplot(mydata.rcorr$r, type = "upper", order = "FPC", tl.col = "black", tl.srt = 45)
corrplot(mydata.rcorr$r, type = "upper", tl.col = "black", tl.srt = 45)

#### regression analysis for predicting prosocial behavior 
fit_prosocial_behavior1<-lm(Prosocial_behavior~ Educational_PS_standardized+Health_statement+age_at_R5+gender+Home_ownship+
                              Education_mother+Education_father+Financial_Satisfaction+pc1_shuffled+pc2_shuffled+pc3_shuffled+pc3_shuffled
                            +pc4_shuffled+pc5_shuffled+pc6_shuffled+pc7_shuffled+pc8_shuffled+pc9_shuffled+pc10_shuffled, 
                            data=Graduate_prosocial_behaviors)
summary(fit_prosocial_behavior1)


fit_prosocial_behavior2<-lm(Prosocial_behavior~ Educational_PS_standardized+Years_of_Education+Health_statement+age_at_R5+gender+Home_ownship+
                              Education_mother+Education_father+Financial_Satisfaction+pc1_shuffled+pc2_shuffled+pc3_shuffled+pc3_shuffled
                            +pc4_shuffled+pc5_shuffled+pc6_shuffled+pc7_shuffled+pc8_shuffled+pc9_shuffled+pc10_shuffled, 
                            data=Graduate_prosocial_behaviors)
summary(fit_prosocial_behavior2)

#### regression analysis for predicting educational attainment  
fit_educational_attainment<-lm(Years_of_Education ~ Educational_PS_standardized+Health_statement+age_at_R5+gender+Home_ownship+
                              Education_mother+Education_father+Financial_Satisfaction+pc1_shuffled+pc2_shuffled+pc3_shuffled+pc3_shuffled
                            +pc4_shuffled+pc5_shuffled+pc6_shuffled+pc7_shuffled+pc8_shuffled+pc9_shuffled+pc10_shuffled, 
                            data=Graduate_prosocial_behaviors)
summary(fit_educational_attainment)

#### regression analysis for predicting Formal prosocial behavior 
fit_Formal_prosocial_behavior1<-lm(Formal_behavior~ Educational_PS_standardized+Health_statement+age_at_R5+gender+Home_ownship+
                                     Education_mother+Education_father+Financial_Satisfaction+pc1_shuffled+pc2_shuffled+pc3_shuffled+pc3_shuffled
                                   +pc4_shuffled+pc5_shuffled+pc6_shuffled+pc7_shuffled+pc8_shuffled+pc9_shuffled+pc10_shuffled, 
                                   data=Graduate_prosocial_behaviors)
summary(fit_Formal_prosocial_behavior1)

fit_Formal_prosocial_behavior2<-lm(Formal_behavior~ Educational_PS_standardized+Years_of_Education+Health_statement+age_at_R5+gender+Home_ownship+
                                     Education_mother+Education_father+Financial_Satisfaction+pc1_shuffled+pc2_shuffled+pc3_shuffled+pc3_shuffled
                                   +pc4_shuffled+pc5_shuffled+pc6_shuffled+pc7_shuffled+pc8_shuffled+pc9_shuffled+pc10_shuffled, 
                                   data=Graduate_prosocial_behaviors)
summary(fit_Formal_prosocial_behavior2)


#### regression analysis for predicting informal prosocial behavior 


fit_Informal_prosocial_behavior1<-lm(Informal_behavior~ Educational_PS_standardized+Health_statement+age_at_R5+gender+Home_ownship+
                                       Education_mother+Education_father+Financial_Satisfaction+pc1_shuffled+pc2_shuffled+pc3_shuffled+pc3_shuffled
                                     +pc4_shuffled+pc5_shuffled+pc6_shuffled+pc7_shuffled+pc8_shuffled+pc9_shuffled+pc10_shuffled, 
                                     data=Graduate_prosocial_behaviors)
summary(fit_Informal_prosocial_behavior1)

fit_Informal_prosocial_behavior2<-lm(Informal_behavior~ Educational_PS_standardized+Years_of_Education+Health_statement+age_at_R5+gender+Home_ownship+
                                       Education_mother+Education_father+Financial_Satisfaction+pc1_shuffled+pc2_shuffled+pc3_shuffled+pc3_shuffled
                                     +pc4_shuffled+pc5_shuffled+pc6_shuffled+pc7_shuffled+pc8_shuffled+pc9_shuffled+pc10_shuffled, 
                                     data=Graduate_prosocial_behaviors)
summary(fit_Informal_prosocial_behavior2)

#### mediation analysis
#### for prosocial behavior

process (data=Graduate_prosocial_behaviors,y="Prosocial_behavior",x="Educational_PS_standardized",m=c("Years_of_Education"),
         cov=c("age_at_R5","gender","Health_statement","Home_ownship", "Financial_Satisfaction","Education_mother","Education_father", "pc1_shuffled",
               "pc2_shuffled", "pc3_shuffled","pc4_shuffled","pc5_shuffled","pc6_shuffled", "pc7_shuffled", "pc8_shuffled", "pc9_shuffled",
               "pc10_shuffled"),model=4,contrast=1,normal=1,conf=90,save=1, stand=1)        

#### for Formal prosocial behavior

process (data=Graduate_prosocial_behaviors,y="Formal_behavior",x="Educational_PS_standardized",m=c("Years_of_Education"),
         cov=c("age_at_R5","gender","Health_statement","Home_ownship", "Financial_Satisfaction","Education_mother","Education_father", "pc1_shuffled",
               "pc2_shuffled", "pc3_shuffled","pc4_shuffled","pc5_shuffled","pc6_shuffled", "pc7_shuffled", "pc8_shuffled", "pc9_shuffled",
               "pc10_shuffled"),model=4,contrast=1,normal=1,conf=90,save=1, stand=1)

################### Save the working datafile and results#######################################3

write.csv(fit_Informal_prosocial_behavior2, "fit_Informal_prosocial_behavior2.csv",row.names=T)

write.csv(Formal_Informal_behavior2,"Graduate_prosocial_behaviors_results.csv", row.names = FALSE)

##### save graphs
pdf("Description of WLS (n=2592).pdf")
hist(Graduate_prosocial_behaviors$Educational_PS,col="orange",ylim = c(0,800),xlim = c(-0.6, 0.4),main="Educational polygenic score")
corrplot(mydata.rcorr$r, type = "upper", order = "FPC", tl.col = "black", tl.srt = 45)
dev.off()

###### save results###################################

sink("Informal_prosocial_behavior2(n=2592).txt")
print(describe(Formal_Informal_behavior2))
print("######## Correlation coefficients")
print(mydata.rcorr)

print("######## regression analysis for predicting prosocial behavior")
print(summary(fit_prosocial_behavior1))
print(summary(fit_prosocial_behavior2))

print("######## regression analysis for predicting formal prosocial behavior")
print(summary(fit_Formal_prosocial_behavior1))
print(summary(fit_Formal_prosocial_behavior2))


print("######## regression analysis for predicting informal prosocial behavior")
print(summary(fit_Informal_prosocial_behavior1))
print(summary(fit_Informal_prosocial_behavior2))

print("######## Mediation analysis for prosocial behavior")

print(process (data=Graduate_prosocial_behaviors,y="Prosocial_behavior",x="Educational_PS_standardized",m=c("Years_of_Education"),
               cov=c("age_at_R5","gender","Health_statement","Home_ownship", "Financial_Satisfaction","Education_mother","Education_father", "pc1_shuffled",
                     "pc2_shuffled", "pc3_shuffled","pc4_shuffled","pc5_shuffled","pc6_shuffled", "pc7_shuffled", "pc8_shuffled", "pc9_shuffled",
                     "pc10_shuffled"),model=4,contrast=1,normal=1,conf=90,save=1, stand=1))

print("######## Mediation analysis for formal prosocial behavior")
process (data=Graduate_prosocial_behaviors,y="Formal_behavior",x="Educational_PS_standardized",m=c("Years_of_Education"),
         cov=c("age_at_R5","gender","Health_statement","Home_ownship", "Financial_Satisfaction","Education_mother","Education_father", "pc1_shuffled",
               "pc2_shuffled", "pc3_shuffled","pc4_shuffled","pc5_shuffled","pc6_shuffled", "pc7_shuffled", "pc8_shuffled", "pc9_shuffled",
               "pc10_shuffled"),model=4,contrast=1,normal=1,conf=90,save=1, stand=1)
sink()
