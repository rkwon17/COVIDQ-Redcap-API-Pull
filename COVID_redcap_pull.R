#RK wrote script 3/12/21 for covid questionnaire packet: pulls all the COVID questionnaire data
#from redcap and wrangles dataframe to be in analyzable format. 

#contains EPII (initial+monthly), PSS (initial+ monthly), Schooling Q (initial,monthly (TBD)
#EPII - still needs to be scored (item level + impact)

#install packages if do not have already
library(magrittr)
library(foreign)
library(tidyr)
library(texreg)
library(REDCapR)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
#library(Hmisc)
#detach("package:Hmisc", unload=TRUE) #use when want to use psych
library(psych)
library(eeptools)
library(lubridate)
###---call API token here (different for each person)------
#redcap api token call (will need to request API token from redcap admin)
source("~/Desktop/config.R")
####------- filter for rdocdb eligible and filter out excluded subjects------
subjlist_erp<- c("subj","eligibility", "exclusion_criteria")
eligible_erp <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = subjlist_erp
)$data
#excluded subs nirs
subjlist_nirs<- c("subj","eligibility", "exclusion_criteria")
eligible_nirs <- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = subjlist_nirs
)$data


erplist <- eligible_erp %>%
  filter(eligibility ==1) 
nirslist<- eligible_nirs %>%
  filter(eligibility ==1) 
#full eligible subject list


#relevant dates
demographics <- read.csv("/Volumes/dmc-nelson/Groups/DMC-Emotion-Project/Groups/Data/Redcap/Demographics/Emo-Demographics.csv")
imp_dates <- demographics %>% select(subj,DOBi,DOBf,DOBm)

fullsubjlist <- full_join(erplist,nirslist) %>%select(subj,eligibility)
fullsubjlist <- left_join(fullsubjlist,imp_dates)


###EPII - initial  API PULLL-----------------------------------------------------------------------------
epiiInit_erp <- c("subj","covid1_date","ques_respondent_covid1","covid1_epii_1","covid1_epii_2","covid1_epii_3","covid1_epii_4","covid1_epii_5","covid1_epii_6","covid1_epii_7","covid1_epii_8",
                  "covid1_epii_9","covid1_epii_10","covid1_epii_11","covid1_epii_workall","covid1_epii_12","covid1_epii_13","covid1_epii_edu","covid1_epii14","covid1_epii15",
"covid1_epii16","covid1_epii17","covid1_epii18","covid1_epii19","covid1_epii20","covid1_epii21","covid1_epii22","covid1_epii23","covid1_epii24","covid1_epii25",
"covid1_epii26","covid1_epii_home","covid1_epii27","covid1_epii28","covid1_epii29","covid1_epii30","covid1_epii31","covid1_epii32","covid1_epii33","covid1_epii34",
"covid1_epii35","covid1_epii36","covid1_epii_social","covid1_epii37","covid1_epii38","covid1_epii39","covid1_epii40","covid1_epii41","covid1_epii_economic","covid1_epii42",
"covid1_epii43","covid1_epii44","covid1_epii45","covid1_epii46","covid1_epii47","covid1_epii48","covid1_epii49","covid1_epii_emotion","covid1_epii50","covid1_epii51",
"covid1_epii52","covid1_epii53","covid1_epii54","covid1_epii55","covid1_epii56","covid1_epii57","covid1_epii_health","covid1_epii58","covid1_epii59","covid1_epii60",
"covid1_epii61","covid1_epii62","covid1_epii63","covid1_epii64","covid1_epii65","covid1_epii_quarantine","covid1_epii66","covid1_epii67","covid1_epii68","covid1_epii69",
"covid1_epii70","covid1_epii71","covid1_epii72","covid1_epii73","covid1_epii_infection","covid1_epii74","covid1_epii75","covid1_epii76","covid1_epii77","covid1_epii78",
"covid1_epii79","covid1_epii80","covid1_epii81","covid1_epii82","covid1_epii83","covid1_epii84","covid1_epii85","covid1_epii86","covid1_epii87","covid1_epii88","covid1_epii89","covid1_epii90",
"covid1_epii91","covid1_epii92","covid1_epii_positive","covid1_epii_overall","covid1_epii_stressors","covid1_epii_other")


epiiInitERP <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = epiiInit_erp
)$data

epiiInit_nirs <- c("subj","covid1_date","ques_respondent_covid1","covid1_epii_1","covid1_epii_2","covid1_epii_3","covid1_epii_4","covid1_epii_5","covid1_epii_6","covid1_epii_7","covid1_epii_8",
                   "covid1_epii_9","covid1_epii_10","covid1_epii_11","covid1_epii_workall","covid1_epii_12","covid1_epii_13","covid1_epii_edu","covid1_epii14","covid1_epii15",
                   "covid1_epii16","covid1_epii17","covid1_epii18","covid1_epii19","covid1_epii20","covid1_epii21","covid1_epii22","covid1_epii23","covid1_epii24","covid1_epii25",
                   "covid1_epii26","covid1_epii_home","covid1_epii27","covid1_epii28","covid1_epii29","covid1_epii30","covid1_epii31","covid1_epii32","covid1_epii33","covid1_epii34",
                   "covid1_epii35","covid1_epii36","covid1_epii_social","covid1_epii37","covid1_epii38","covid1_epii39","covid1_epii40","covid1_epii41","covid1_epii_economic","covid1_epii42",
                   "covid1_epii43","covid1_epii44","covid1_epii45","covid1_epii46","covid1_epii47","covid1_epii48","covid1_epii49","covid1_epii_emotion","covid1_epii50","covid1_epii51",
                   "covid1_epii52","covid1_epii53","covid1_epii54","covid1_epii55","covid1_epii56","covid1_epii57","covid1_epii_health","covid1_epii58","covid1_epii59","covid1_epii60",
                   "covid1_epii61","covid1_epii62","covid1_epii63","covid1_epii64","covid1_epii65","covid1_epii_quarantine","covid1_epii66","covid1_epii67","covid1_epii68","covid1_epii69",
                   "covid1_epii70","covid1_epii71","covid1_epii72","covid1_epii73","covid1_epii_infection","covid1_epii74","covid1_epii75","covid1_epii76","covid1_epii77","covid1_epii78",
                   "covid1_epii79","covid1_epii80","covid1_epii81","covid1_epii82","covid1_epii83","covid1_epii84","covid1_epii85","covid1_epii86","covid1_epii87","covid1_epii88","covid1_epii89","covid1_epii90",
                   "covid1_epii91","covid1_epii92","covid1_epii_positive","covid1_epii_overall","covid1_epii_stressors","covid1_epii_other")

epiiInitNIRS <- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = epiiInit_nirs
)$data

epiiInitERP <- epiiInitERP %>%
  filter(redcap_event_name=="covid_questionnair_arm_1")
epiiInitNIRS <- epiiInitNIRS%>%
  filter(redcap_event_name=="covid_questionnair_arm_1")

epii_initial <- full_join(epiiInitERP,epiiInitNIRS)
#exclude ineligible subjects
epii_initial <- left_join(fullsubjlist,epii_initial)

epii <- epii_initial[!is.na(epii_initial$covid1_date), ]

### clean matrices for initial epii --------------

#for now hard coding this but see if can be placed in a for loop or make shorter code
epii <- epii %>% 
  mutate(epii_init_1=case_when(
    covid1_epii_1___1=='1'~'1',
    covid1_epii_1___2=='1'~'2',
    covid1_epii_1___0=='1'~'0',
    covid1_epii_1___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_1___1, -covid1_epii_1___2, -covid1_epii_1___0,-covid1_epii_1___9) %>%
  mutate(epii_init_2=case_when(
    covid1_epii_2___1=='1'~'1',
    covid1_epii_2___2=='1'~'2',
    covid1_epii_2___0=='1'~'0',
    covid1_epii_2___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_2___1, -covid1_epii_2___2, -covid1_epii_2___0,-covid1_epii_2___9) %>%
  mutate(epii_init_3=case_when(
    covid1_epii_3___1=='1'~'1',
    covid1_epii_3___2=='1'~'2',
    covid1_epii_3___0=='1'~'0',
    covid1_epii_3___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_3___1, -covid1_epii_3___2, -covid1_epii_3___0,-covid1_epii_3___9) %>%
  mutate(epii_init_4=case_when(
    covid1_epii_4___1=='1'~'1',
    covid1_epii_4___2=='1'~'2',
    covid1_epii_4___0=='1'~'0',
    covid1_epii_4___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_4___1, -covid1_epii_4___2, -covid1_epii_4___0,-covid1_epii_4___9) %>%
  mutate(epii_init_5=case_when(
    covid1_epii_5___1=='1'~'1',
    covid1_epii_5___2=='1'~'2',
    covid1_epii_5___0=='1'~'0',
    covid1_epii_5___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_5___1, -covid1_epii_5___2, -covid1_epii_5___0,-covid1_epii_5___9) %>%
  mutate(epii_init_6=case_when(
    covid1_epii_6___1=='1'~'1',
    covid1_epii_6___2=='1'~'2',
    covid1_epii_6___0=='1'~'0',
    covid1_epii_6___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_6___1, -covid1_epii_6___2, -covid1_epii_6___0,-covid1_epii_6___9) %>%
  mutate(epii_init_7=case_when(
    covid1_epii_7___1=='1'~'1',
    covid1_epii_7___2=='1'~'2',
    covid1_epii_7___0=='1'~'0',
    covid1_epii_7___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_7___1, -covid1_epii_7___2, -covid1_epii_7___0,-covid1_epii_7___9) %>%
  mutate(epii_init_8=case_when(
    covid1_epii_8___1=='1'~'1',
    covid1_epii_8___2=='1'~'2',
    covid1_epii_8___0=='1'~'0',
    covid1_epii_8___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_8___1, -covid1_epii_8___2, -covid1_epii_8___0,-covid1_epii_8___9) %>%
  mutate(epii_init_9=case_when(
    covid1_epii_9___1=='1'~'1',
    covid1_epii_9___2=='1'~'2',
    covid1_epii_9___0=='1'~'0',
    covid1_epii_9___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_9___1, -covid1_epii_9___2, -covid1_epii_9___0,-covid1_epii_9___9) %>%
  mutate(epii_init_10=case_when(
    covid1_epii_10___1=='1'~'1',
    covid1_epii_10___2=='1'~'2',
    covid1_epii_10___0=='1'~'0',
    covid1_epii_10___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_10___1, -covid1_epii_10___2, -covid1_epii_10___0,-covid1_epii_10___9) %>%
  mutate(epii_init_11=case_when(
    covid1_epii_11___1=='1'~'1',
    covid1_epii_11___2=='1'~'2',
    covid1_epii_11___0=='1'~'0',
    covid1_epii_11___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii_11___1, -covid1_epii_11___2, -covid1_epii_11___0,-covid1_epii_11___9) %>%
  mutate(epii_init_12=case_when(
    covid1_epii_12___1=='1'~'1',
    covid1_epii_12___2=='1'~'2',
    covid1_epii_12___0=='1'~'0',
    covid1_epii_12___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii_12___1, -covid1_epii_12___2, -covid1_epii_12___0,-covid1_epii_12___9) %>%
  mutate(epii_init_13=case_when(
    covid1_epii_13___1=='1'~'1',
    covid1_epii_13___2=='1'~'2',
    covid1_epii_13___0=='1'~'0',
    covid1_epii_13___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii_13___1, -covid1_epii_13___2, -covid1_epii_13___0,-covid1_epii_13___9) %>%
  mutate(epii_init_14=case_when(
    covid1_epii14___1=='1'~'1',
    covid1_epii14___2=='1'~'2',
    covid1_epii14___0=='1'~'0',
    covid1_epii14___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii14___1, -covid1_epii14___2, -covid1_epii14___0,-covid1_epii14___9) %>%
  mutate(epii_init_15=case_when(
    covid1_epii15___1=='1'~'1',
    covid1_epii15___2=='1'~'2',
    covid1_epii15___0=='1'~'0',
    covid1_epii15___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii15___1, -covid1_epii15___2, -covid1_epii15___0,-covid1_epii15___9) %>%
  mutate(epii_init_16=case_when(
    covid1_epii16___1=='1'~'1',
    covid1_epii16___2=='1'~'2',
    covid1_epii16___0=='1'~'0',
    covid1_epii16___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii16___1, -covid1_epii16___2, -covid1_epii16___0,-covid1_epii16___9) %>%
  mutate(epii_init_17=case_when(
    covid1_epii17___1=='1'~'1',
    covid1_epii17___2=='1'~'2',
    covid1_epii17___0=='1'~'0',
    covid1_epii17___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii17___1, -covid1_epii17___2, -covid1_epii17___0,-covid1_epii17___9) %>%
  mutate(epii_init_18=case_when(
    covid1_epii18___1=='1'~'1',
    covid1_epii18___2=='1'~'2',
    covid1_epii18___0=='1'~'0',
    covid1_epii18___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii18___1, -covid1_epii18___2, -covid1_epii18___0,-covid1_epii18___9) %>%
  mutate(epii_init_19=case_when(
    covid1_epii19___1=='1'~'1',
    covid1_epii19___2=='1'~'2',
    covid1_epii19___0=='1'~'0',
    covid1_epii19___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii19___1, -covid1_epii19___2, -covid1_epii19___0,-covid1_epii19___9) %>%
  mutate(epii_init_20=case_when(
    covid1_epii20___1=='1'~'1',
    covid1_epii20___2=='1'~'2',
    covid1_epii20___0=='1'~'0',
    covid1_epii20___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii20___1, -covid1_epii20___2, -covid1_epii20___0,-covid1_epii20___9) %>%
  mutate(epii_init_21=case_when(
    covid1_epii21___1=='1'~'1',
    covid1_epii21___2=='1'~'2',
    covid1_epii21___0=='1'~'0',
    covid1_epii21___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii21___1, -covid1_epii21___2, -covid1_epii21___0,-covid1_epii21___9) %>%
  mutate(epii_init_22=case_when(
    covid1_epii22___1=='1'~'1',
    covid1_epii22___2=='1'~'2',
    covid1_epii22___0=='1'~'0',
    covid1_epii22___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii22___1, -covid1_epii22___2, -covid1_epii22___0,-covid1_epii22___9) %>%
  mutate(epii_init_23=case_when(
    covid1_epii23___1=='1'~'1',
    covid1_epii23___2=='1'~'2',
    covid1_epii23___0=='1'~'0',
    covid1_epii23___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii23___1, -covid1_epii23___2, -covid1_epii23___0,-covid1_epii23___9) %>%
  mutate(epii_init_24=case_when(
    covid1_epii24___1=='1'~'1',
    covid1_epii24___2=='1'~'2',
    covid1_epii24___0=='1'~'0',
    covid1_epii24___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii24___1, -covid1_epii24___2, -covid1_epii24___0,-covid1_epii24___9) %>%
  mutate(epii_init_25=case_when(
    covid1_epii25___1=='1'~'1',
    covid1_epii25___2=='1'~'2',
    covid1_epii25___0=='1'~'0',
    covid1_epii25___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii25___1, -covid1_epii25___2, -covid1_epii25___0,-covid1_epii25___9) %>%
  mutate(epii_init_26=case_when(
    covid1_epii26___1=='1'~'1',
    covid1_epii26___2=='1'~'2',
    covid1_epii26___0=='1'~'0',
    covid1_epii26___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii26___1, -covid1_epii26___2, -covid1_epii26___0,-covid1_epii26___9) %>%
  mutate(epii_init_27=case_when(
    covid1_epii27___1=='1'~'1',
    covid1_epii27___2=='1'~'2',
    covid1_epii27___0=='1'~'0',
    covid1_epii27___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii27___1, -covid1_epii27___2, -covid1_epii27___0,-covid1_epii27___9) %>%
  mutate(epii_init_28=case_when(
    covid1_epii28___1=='1'~'1',
    covid1_epii28___2=='1'~'2',
    covid1_epii28___0=='1'~'0',
    covid1_epii28___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii28___1, -covid1_epii28___2, -covid1_epii28___0,-covid1_epii28___9) %>%
  mutate(epii_init_29=case_when(
    covid1_epii29___1=='1'~'1',
    covid1_epii29___2=='1'~'2',
    covid1_epii29___0=='1'~'0',
    covid1_epii29___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii29___1, -covid1_epii29___2, -covid1_epii29___0,-covid1_epii29___9) %>%
  mutate(epii_init_30=case_when(
    covid1_epii30___1=='1'~'1',
    covid1_epii30___2=='1'~'2',
    covid1_epii30___0=='1'~'0',
    covid1_epii30___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii30___1, -covid1_epii30___2, -covid1_epii30___0,-covid1_epii30___9) %>%
  mutate(epii_init_31=case_when(
    covid1_epii31___1=='1'~'1',
    covid1_epii31___2=='1'~'2',
    covid1_epii31___0=='1'~'0',
    covid1_epii31___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii31___1, -covid1_epii31___2, -covid1_epii31___0,-covid1_epii31___9) %>%
  mutate(epii_init_32=case_when(
    covid1_epii32___1=='1'~'1',
    covid1_epii32___2=='1'~'2',
    covid1_epii32___0=='1'~'0',
    covid1_epii32___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii32___1, -covid1_epii32___2, -covid1_epii32___0,-covid1_epii32___9) %>%
  mutate(epii_init_33=case_when(
    covid1_epii33___1=='1'~'1',
    covid1_epii33___2=='1'~'2',
    covid1_epii33___0=='1'~'0',
    covid1_epii33___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii33___1, -covid1_epii33___2, -covid1_epii33___0,-covid1_epii33___9) %>%
  mutate(epii_init_34=case_when(
    covid1_epii34___1=='1'~'1',
    covid1_epii34___2=='1'~'2',
    covid1_epii34___0=='1'~'0',
    covid1_epii34___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii34___1, -covid1_epii34___2, -covid1_epii34___0,-covid1_epii34___9) %>%
  mutate(epii_init_35=case_when(
    covid1_epii35___1=='1'~'1',
    covid1_epii35___2=='1'~'2',
    covid1_epii35___0=='1'~'0',
    covid1_epii35___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii35___1, -covid1_epii35___2, -covid1_epii35___0,-covid1_epii35___9) %>%
  mutate(epii_init_36=case_when(
    covid1_epii36___1=='1'~'1',
    covid1_epii36___2=='1'~'2',
    covid1_epii36___0=='1'~'0',
    covid1_epii36___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii36___1, -covid1_epii36___2, -covid1_epii36___0,-covid1_epii36___9) %>%
  mutate(epii_init_37=case_when(
    covid1_epii37___1=='1'~'1',
    covid1_epii37___2=='1'~'2',
    covid1_epii37___0=='1'~'0',
    covid1_epii37___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii37___1, -covid1_epii37___2, -covid1_epii37___0,-covid1_epii37___9) %>%
  mutate(epii_init_38=case_when(
    covid1_epii38___1=='1'~'1',
    covid1_epii38___2=='1'~'2',
    covid1_epii38___0=='1'~'0',
    covid1_epii38___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii38___1, -covid1_epii38___2, -covid1_epii38___0,-covid1_epii38___9) %>%
  mutate(epii_init_39=case_when(
    covid1_epii39___1=='1'~'1',
    covid1_epii39___2=='1'~'2',
    covid1_epii39___0=='1'~'0',
    covid1_epii39___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii39___1, -covid1_epii39___2, -covid1_epii39___0,-covid1_epii39___9) %>%
  mutate(epii_init_40=case_when(
    covid1_epii40___1=='1'~'1',
    covid1_epii40___2=='1'~'2',
    covid1_epii40___0=='1'~'0',
    covid1_epii40___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii40___1, -covid1_epii40___2, -covid1_epii40___0,-covid1_epii40___9) %>%
  mutate(epii_init_41=case_when(
    covid1_epii41___1=='1'~'1',
    covid1_epii41___2=='1'~'2',
    covid1_epii41___0=='1'~'0',
    covid1_epii41___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii41___1, -covid1_epii41___2, -covid1_epii41___0,-covid1_epii41___9) %>%
  # mutate(epii_init_42=case_when(
  #  covid1_epii42___1=='1'~'1',
  # covid1_epii42___2=='1'~'2',
  #covid1_epii42___0=='1'~'0',
  #covid1_epii42___9=='1'~'9',
  #TRUE ~NA_character_
  #)) %>%
  #select(-covid1_epii42___1, -covid1_epii42___2, -covid1_epii42___0,-covid1_epii42___9) %>%
  #mutate(epii_init_43=case_when(
  # covid1_epii43___1=='1'~'1',
  #covid1_epii43___2=='1'~'2',
#covid1_epii43___0=='1'~'0',
#covid1_epii43___9=='1'~'9',
#   TRUE ~NA_character_
#  )) %>%
# select(-covid1_epii43___1, -covid1_epii43___2, -covid1_epii43___0,-covid1_epii43___9) %>%
mutate(epii_init_44=case_when(
  covid1_epii44___1=='1'~'1',
  covid1_epii44___2=='1'~'2',
  covid1_epii44___0=='1'~'0',
  covid1_epii44___9=='1'~'9',
  TRUE ~NA_character_
)) %>%
  select(-covid1_epii44___1, -covid1_epii44___2, -covid1_epii44___0,-covid1_epii44___9) %>%
  mutate(epii_init_45=case_when(
    covid1_epii45___1=='1'~'1',
    covid1_epii45___2=='1'~'2',
    covid1_epii45___0=='1'~'0',
    covid1_epii45___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii45___1, -covid1_epii45___2, -covid1_epii45___0,-covid1_epii45___9) %>%
  mutate(epii_init_46=case_when(
    covid1_epii46___1=='1'~'1',
    covid1_epii46___2=='1'~'2',
    covid1_epii46___0=='1'~'0',
    covid1_epii46___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii46___1, -covid1_epii46___2, -covid1_epii46___0,-covid1_epii46___9) %>%
  mutate(epii_init_47=case_when(
    covid1_epii47___1=='1'~'1',
    covid1_epii47___2=='1'~'2',
    covid1_epii47___0=='1'~'0',
    covid1_epii47___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii47___1, -covid1_epii47___2, -covid1_epii47___0,-covid1_epii47___9) %>%
  mutate(epii_init_48=case_when(
    covid1_epii48___1=='1'~'1',
    covid1_epii48___2=='1'~'2',
    covid1_epii48___0=='1'~'0',
    covid1_epii48___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii48___1, -covid1_epii48___2, -covid1_epii48___0,-covid1_epii48___9) %>%
  mutate(epii_init_49=case_when(
    covid1_epii49___1=='1'~'1',
    covid1_epii49___2=='1'~'2',
    covid1_epii49___0=='1'~'0',
    covid1_epii49___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii49___1, -covid1_epii49___2, -covid1_epii49___0,-covid1_epii49___9) %>%
  mutate(epii_init_50=case_when(
    covid1_epii50___1=='1'~'1',
    covid1_epii50___2=='1'~'2',
    covid1_epii50___0=='1'~'0',
    covid1_epii50___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii50___1, -covid1_epii50___2, -covid1_epii50___0,-covid1_epii50___9) %>%
  mutate(epii_init_51=case_when(
    covid1_epii51___1=='1'~'1',
    covid1_epii51___2=='1'~'2',
    covid1_epii51___0=='1'~'0',
    covid1_epii51___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii51___1, -covid1_epii51___2, -covid1_epii51___0,-covid1_epii51___9) %>%
  mutate(epii_init_52=case_when(
    covid1_epii52___1=='1'~'1',
    covid1_epii52___2=='1'~'2',
    covid1_epii52___0=='1'~'0',
    covid1_epii52___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii52___1, -covid1_epii52___2, -covid1_epii52___0,-covid1_epii52___9) %>%
  mutate(epii_init_53=case_when(
    covid1_epii53___1=='1'~'1',
    covid1_epii53___2=='1'~'2',
    covid1_epii53___0=='1'~'0',
    covid1_epii53___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii53___1, -covid1_epii53___2, -covid1_epii53___0,-covid1_epii53___9) %>%
  mutate(epii_init_54=case_when(
    covid1_epii54___1=='1'~'1',
    covid1_epii54___2=='1'~'2',
    covid1_epii54___0=='1'~'0',
    covid1_epii54___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii54___1, -covid1_epii54___2, -covid1_epii54___0,-covid1_epii54___9) %>%
  mutate(epii_init_55=case_when(
    covid1_epii55___1=='1'~'1',
    covid1_epii55___2=='1'~'2',
    covid1_epii55___0=='1'~'0',
    covid1_epii55___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii55___1, -covid1_epii55___2, -covid1_epii55___0,-covid1_epii55___9) %>%
  mutate(epii_init_56=case_when(
    covid1_epii56___1=='1'~'1',
    covid1_epii56___2=='1'~'2',
    covid1_epii56___0=='1'~'0',
    covid1_epii56___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii56___1, -covid1_epii56___2, -covid1_epii56___0,-covid1_epii56___9) %>%
  mutate(epii_init_57=case_when(
    covid1_epii57___1=='1'~'1',
    covid1_epii57___2=='1'~'2',
    covid1_epii57___0=='1'~'0',
    covid1_epii57___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii57___1, -covid1_epii57___2, -covid1_epii57___0,-covid1_epii57___9) %>%
  mutate(epii_init_58=case_when(
    covid1_epii58___1=='1'~'1',
    covid1_epii58___2=='1'~'2',
    covid1_epii58___0=='1'~'0',
    covid1_epii58___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii58___1, -covid1_epii58___2, -covid1_epii58___0,-covid1_epii58___9) %>%
  mutate(epii_init_59=case_when(
    covid1_epii59___1=='1'~'1',
    covid1_epii59___2=='1'~'2',
    covid1_epii59___0=='1'~'0',
    covid1_epii59___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii59___1, -covid1_epii59___2, -covid1_epii59___0,-covid1_epii59___9) %>%
  mutate(epii_init_60=case_when(
    covid1_epii60___1=='1'~'1',
    covid1_epii60___2=='1'~'2',
    covid1_epii60___0=='1'~'0',
    covid1_epii60___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii60___1, -covid1_epii60___2, -covid1_epii60___0,-covid1_epii60___9) %>%
  mutate(epii_init_61=case_when(
    covid1_epii61___1=='1'~'1',
    covid1_epii61___2=='1'~'2',
    covid1_epii61___0=='1'~'0',
    covid1_epii61___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii61___1, -covid1_epii61___2, -covid1_epii61___0,-covid1_epii61___9) %>%
  mutate(epii_init_62=case_when(
    covid1_epii62___1=='1'~'1',
    covid1_epii62___2=='1'~'2',
    covid1_epii62___0=='1'~'0',
    covid1_epii62___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii62___1, -covid1_epii62___2, -covid1_epii62___0,-covid1_epii62___9) %>%
  mutate(epii_init_63=case_when(
    covid1_epii63___1=='1'~'1',
    covid1_epii63___2=='1'~'2',
    covid1_epii63___0=='1'~'0',
    covid1_epii63___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii63___1, -covid1_epii63___2, -covid1_epii63___0,-covid1_epii63___9) %>%
  mutate(epii_init_64=case_when(
    covid1_epii64___1=='1'~'1',
    covid1_epii64___2=='1'~'2',
    covid1_epii64___0=='1'~'0',
    covid1_epii64___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii64___1, -covid1_epii64___2, -covid1_epii64___0,-covid1_epii64___9) %>%
  mutate(epii_init_65=case_when(
    covid1_epii65___1=='1'~'1',
    covid1_epii65___2=='1'~'2',
    covid1_epii65___0=='1'~'0',
    covid1_epii65___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii65___1, -covid1_epii65___2, -covid1_epii65___0,-covid1_epii65___9) %>%
  mutate(epii_init_66=case_when(
    covid1_epii66___1=='1'~'1',
    covid1_epii66___2=='1'~'2',
    covid1_epii66___0=='1'~'0',
    covid1_epii66___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii66___1, -covid1_epii66___2, -covid1_epii66___0,-covid1_epii66___9) %>%
  mutate(epii_init_67=case_when(
    covid1_epii67___1=='1'~'1',
    covid1_epii67___2=='1'~'2',
    covid1_epii67___0=='1'~'0',
    covid1_epii67___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii67___1, -covid1_epii67___2, -covid1_epii67___0,-covid1_epii67___9) %>%
  mutate(epii_init_68=case_when(
    covid1_epii68___1=='1'~'1',
    covid1_epii68___2=='1'~'2',
    covid1_epii68___0=='1'~'0',
    covid1_epii68___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii68___1, -covid1_epii68___2, -covid1_epii68___0,-covid1_epii68___9) %>%
  mutate(epii_init_69=case_when(
    covid1_epii69___1=='1'~'1',
    covid1_epii69___2=='1'~'2',
    covid1_epii69___0=='1'~'0',
    covid1_epii69___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii69___1, -covid1_epii69___2, -covid1_epii69___0,-covid1_epii69___9) %>%
  mutate(epii_init_70=case_when(
    covid1_epii70___1=='1'~'1',
    covid1_epii70___2=='1'~'2',
    covid1_epii70___0=='1'~'0',
    covid1_epii70___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii70___1, -covid1_epii70___2, -covid1_epii70___0,-covid1_epii70___9) %>%
  mutate(epii_init_71=case_when(
    covid1_epii71___1=='1'~'1',
    covid1_epii71___2=='1'~'2',
    covid1_epii71___0=='1'~'0',
    covid1_epii71___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii71___1, -covid1_epii71___2, -covid1_epii71___0,-covid1_epii71___9) %>%
  mutate(epii_init_72=case_when(
    covid1_epii72___1=='1'~'1',
    covid1_epii72___2=='1'~'2',
    covid1_epii72___0=='1'~'0',
    covid1_epii72___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii72___1, -covid1_epii72___2, -covid1_epii72___0,-covid1_epii72___9) %>%
  mutate(epii_init_73=case_when(
    covid1_epii73___1=='1'~'1',
    covid1_epii73___2=='1'~'2',
    covid1_epii73___0=='1'~'0',
    covid1_epii73___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii73___1, -covid1_epii73___2, -covid1_epii73___0,-covid1_epii73___9) %>%
  mutate(epii_init_74=case_when(
    covid1_epii74___1=='1'~'1',
    covid1_epii74___2=='1'~'2',
    covid1_epii74___0=='1'~'0',
    covid1_epii74___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii74___1, -covid1_epii74___2, -covid1_epii74___0,-covid1_epii74___9) %>%
  mutate(epii_init_75=case_when(
    covid1_epii75___1=='1'~'1',
    covid1_epii75___2=='1'~'2',
    covid1_epii75___0=='1'~'0',
    covid1_epii75___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii75___1, -covid1_epii75___2, -covid1_epii75___0,-covid1_epii75___9) %>%
  mutate(epii_init_76=case_when(
    covid1_epii76___1=='1'~'1',
    covid1_epii76___2=='1'~'2',
    covid1_epii76___0=='1'~'0',
    covid1_epii76___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii76___1, -covid1_epii76___2, -covid1_epii76___0,-covid1_epii76___9) %>%
  mutate(epii_init_77=case_when(
    covid1_epii77___1=='1'~'1',
    covid1_epii77___2=='1'~'2',
    covid1_epii77___0=='1'~'0',
    covid1_epii77___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii77___1, -covid1_epii77___2, -covid1_epii77___0,-covid1_epii77___9) %>%
  mutate(epii_init_78=case_when(
    covid1_epii78___1=='1'~'1',
    covid1_epii78___2=='1'~'2',
    covid1_epii78___0=='1'~'0',
    covid1_epii78___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii78___1, -covid1_epii78___2, -covid1_epii78___0,-covid1_epii78___9) %>%
  mutate(epii_init_79=case_when(
    covid1_epii79___1=='1'~'1',
    covid1_epii79___2=='1'~'2',
    covid1_epii79___0=='1'~'0',
    covid1_epii79___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii79___1, -covid1_epii79___2, -covid1_epii79___0,-covid1_epii79___9) %>%
  mutate(epii_init_80=case_when(
    covid1_epii80___1=='1'~'1',
    covid1_epii80___2=='1'~'2',
    covid1_epii80___0=='1'~'0',
    covid1_epii80___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii80___1, -covid1_epii80___2, -covid1_epii80___0,-covid1_epii80___9) %>%
  mutate(epii_init_81=case_when(
    covid1_epii81___1=='1'~'1',
    covid1_epii81___2=='1'~'2',
    covid1_epii81___0=='1'~'0',
    covid1_epii81___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii81___1, -covid1_epii81___2, -covid1_epii81___0,-covid1_epii81___9) %>%
  mutate(epii_init_82=case_when(
    covid1_epii82___1=='1'~'1',
    covid1_epii82___2=='1'~'2',
    covid1_epii82___0=='1'~'0',
    covid1_epii82___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii82___1, -covid1_epii82___2, -covid1_epii82___0,-covid1_epii82___9) %>%
  mutate(epii_init_83=case_when(
    covid1_epii83___1=='1'~'1',
    covid1_epii83___2=='1'~'2',
    covid1_epii83___0=='1'~'0',
    covid1_epii83___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii83___1, -covid1_epii83___2, -covid1_epii83___0,-covid1_epii83___9) %>%
  mutate(epii_init_84=case_when(
    covid1_epii84___1=='1'~'1',
    covid1_epii84___2=='1'~'2',
    covid1_epii84___0=='1'~'0',
    covid1_epii84___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii84___1, -covid1_epii84___2, -covid1_epii84___0,-covid1_epii84___9) %>%
  mutate(epii_init_85=case_when(
    covid1_epii85___1=='1'~'1',
    covid1_epii85___2=='1'~'2',
    covid1_epii85___0=='1'~'0',
    covid1_epii85___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii85___1, -covid1_epii85___2, -covid1_epii85___0,-covid1_epii85___9) %>%
  mutate(epii_init_86=case_when(
    covid1_epii86___1=='1'~'1',
    covid1_epii86___2=='1'~'2',
    covid1_epii86___0=='1'~'0',
    covid1_epii86___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii86___1, -covid1_epii86___2, -covid1_epii86___0,-covid1_epii86___9) %>%
  mutate(epii_init_87=case_when(
    covid1_epii87___1=='1'~'1',
    covid1_epii87___2=='1'~'2',
    covid1_epii87___0=='1'~'0',
    covid1_epii87___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii87___1, -covid1_epii87___2, -covid1_epii87___0,-covid1_epii87___9) %>%
  mutate(epii_init_88=case_when(
    covid1_epii88___1=='1'~'1',
    covid1_epii88___2=='1'~'2',
    covid1_epii88___0=='1'~'0',
    covid1_epii88___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii88___1, -covid1_epii88___2, -covid1_epii88___0,-covid1_epii88___9) %>%
  mutate(epii_init_89=case_when(
    covid1_epii89___1=='1'~'1',
    covid1_epii89___2=='1'~'2',
    covid1_epii89___0=='1'~'0',
    covid1_epii89___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii89___1, -covid1_epii89___2, -covid1_epii89___0,-covid1_epii89___9) %>%
  mutate(epii_init_90=case_when(
    covid1_epii90___1=='1'~'1',
    covid1_epii90___2=='1'~'2',
    covid1_epii90___0=='1'~'0',
    covid1_epii90___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii90___1, -covid1_epii90___2, -covid1_epii90___0,-covid1_epii90___9) %>%
  mutate(epii_init_91=case_when(
    covid1_epii91___1=='1'~'1',
    covid1_epii91___2=='1'~'2',
    covid1_epii91___0=='1'~'0',
    covid1_epii91___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid1_epii91___1, -covid1_epii91___2, -covid1_epii91___0,-covid1_epii91___9) %>%
  mutate(epii_init_92=case_when(
    covid1_epii92___1=='1'~'1',
    covid1_epii92___2=='1'~'2',
    covid1_epii92___0=='1'~'0',
    covid1_epii92___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid1_epii92___1, -covid1_epii92___2, -covid1_epii92___0,-covid1_epii92___9) %>%
  #rename 42 and 43  
  rename(epii_init_42 = covid1_epii42, epii_init_43=covid1_epii43,epii_init_workall =covid1_epii_workall,epii_init_edu =covid1_epii_edu,
         epii_init_home=covid1_epii_home,epii_init_social=covid1_epii_social,epii_init_economic=covid1_epii_economic,
         epii_init_emotion=covid1_epii_emotion,epii_init_health=covid1_epii_health,epii_init_quarantine=covid1_epii_quarantine,
         epii_init_infection=covid1_epii_infection,epii_init_positive=covid1_epii_positive,epii_init_overall=covid1_epii_overall,
         epii_init_stressors=covid1_epii_stressors,epii_init_other=covid1_epii_other) %>%
  relocate(epii_init_42,.after=epii_init_41)%>%relocate(epii_init_43,.after=epii_init_42)

####------monthly EPII data --------
epiimonth_erp <- c("subj","covid_mo_date", "ques_respondent_covid_mo","covid_mo_epii_1","covid_mo_epii_2","covid_mo_epii_3","covid_mo_epii_4","covid_mo_epii_5",
"covid_mo_epii_6","covid_mo_epii_7","covid_mo_epii_8","covid_mo_epii_9","covid_mo_epii_10","covid_mo_epii_11","covid_mo_epii_workall","covid_mo_epii_12",
"covid_mo_epii_13","covid_mo_epii_edu","covid_mo_epii14","covid_mo_epii15","covid_mo_epii16","covid_mo_epii17","covid_mo_epii18","covid_mo_epii19",
"covid_mo_epii20","covid_mo_epii21","covid_mo_epii22","covid_mo_epii23","covid_mo_epii24","covid_mo_epii25","covid_mo_epii26","covid_mo_epii_home",
"covid_mo_epii27","covid_mo_epii28","covid_mo_epii29","covid_mo_epii30","covid_mo_epii31","covid_mo_epii32","covid_mo_epii33","covid_mo_epii34",
"covid_mo_epii35","covid_mo_epii36","covid_mo_epii_social","covid_mo_epii37","covid_mo_epii38","covid_mo_epii39","covid_mo_epii40","covid_mo_epii41",
"covid_mo_epii_economic","covid_mo_epii42","covid_mo_epii43","covid_mo_epii44","covid_mo_epii45","covid_mo_epii46","covid_mo_epii47","covid_mo_epii48",
"covid_mo_epii49","covid_mo_epii_emotion","covid_mo_epii50","covid_mo_epii51","covid_mo_epii52","covid_mo_epii53","covid_mo_epii54","covid_mo_epii55",
"covid_mo_epii56","covid_mo_epii57","covid_mo_epii_health","covid_mo_epii58","covid_mo_epii59","covid_mo_epii60","covid_mo_epii61","covid_mo_epii62",
"covid_mo_epii63","covid_mo_epii64","covid_mo_epii65","covid_mo_epii_quarantine","covid_mo_epii66","covid_mo_epii67","covid_mo_epii68","covid_mo_epii69","covid_mo_epii70",
"covid_mo_epii71","covid_mo_epii72","covid_mo_epii73","covid_mo_epii_infection","covid_mo_epii74","covid_mo_epii75","covid_mo_epii76","covid_mo_epii77",
"covid_mo_epii78","covid_mo_epii79","covid_mo_epii80","covid_mo_epii81","covid_mo_epii82","covid_mo_epii83","covid_mo_epii84","covid_mo_epii85","covid_mo_epii86",
"covid_mo_epii87","covid_mo_epii88","covid_mo_epii89","covid_mo_epii90","covid_mo_epii91","covid_mo_epii92","covid_mo_epii_positive","covid_mo_epii_overall",
"covid_mo_epii_stressors","covid_mo_epii_other")

epiimonth_ERP <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = epiimonth_erp
)$data

epiimonth_nirs<- c("subj","covid_mo_date", "ques_respondent_covid_mo","covid_mo_epii_1","covid_mo_epii_2","covid_mo_epii_3","covid_mo_epii_4","covid_mo_epii_5",
                   "covid_mo_epii_6","covid_mo_epii_7","covid_mo_epii_8","covid_mo_epii_9","covid_mo_epii_10","covid_mo_epii_11","covid_mo_epii_workall","covid_mo_epii_12",
                   "covid_mo_epii_13","covid_mo_epii_edu","covid_mo_epii14","covid_mo_epii15","covid_mo_epii16","covid_mo_epii17","covid_mo_epii18","covid_mo_epii19",
                   "covid_mo_epii20","covid_mo_epii21","covid_mo_epii22","covid_mo_epii23","covid_mo_epii24","covid_mo_epii25","covid_mo_epii26","covid_mo_epii_home",
                   "covid_mo_epii27","covid_mo_epii28","covid_mo_epii29","covid_mo_epii30","covid_mo_epii31","covid_mo_epii32","covid_mo_epii33","covid_mo_epii34",
                   "covid_mo_epii35","covid_mo_epii36","covid_mo_epii_social","covid_mo_epii37","covid_mo_epii38","covid_mo_epii39","covid_mo_epii40","covid_mo_epii41",
                   "covid_mo_epii_economic","covid_mo_epii42","covid_mo_epii43","covid_mo_epii44","covid_mo_epii45","covid_mo_epii46","covid_mo_epii47","covid_mo_epii48",
                   "covid_mo_epii49","covid_mo_epii_emotion","covid_mo_epii50","covid_mo_epii51","covid_mo_epii52","covid_mo_epii53","covid_mo_epii54","covid_mo_epii55",
                   "covid_mo_epii56","covid_mo_epii57","covid_mo_epii_health","covid_mo_epii58","covid_mo_epii59","covid_mo_epii60","covid_mo_epii61","covid_mo_epii62",
                   "covid_mo_epii63","covid_mo_epii64","covid_mo_epii65","covid_mo_epii_quarantine","covid_mo_epii66","covid_mo_epii67","covid_mo_epii68","covid_mo_epii69","covid_mo_epii70",
                   "covid_mo_epii71","covid_mo_epii72","covid_mo_epii73","covid_mo_epii_infection","covid_mo_epii74","covid_mo_epii75","covid_mo_epii76","covid_mo_epii77",
                   "covid_mo_epii78","covid_mo_epii79","covid_mo_epii80","covid_mo_epii81","covid_mo_epii82","covid_mo_epii83","covid_mo_epii84","covid_mo_epii85","covid_mo_epii86",
                   "covid_mo_epii87","covid_mo_epii88","covid_mo_epii89","covid_mo_epii90","covid_mo_epii91","covid_mo_epii92","covid_mo_epii_positive","covid_mo_epii_overall",
                   "covid_mo_epii_stressors","covid_mo_epii_other")

epiimonth_NIRS <- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = epiimonth_nirs
)$data

epii_monthly <- full_join(epiimonth_ERP,epiimonth_NIRS)
#exclude ineligible subjects
epii_monthly <- left_join(fullsubjlist,epii_monthly)

epii_months <- epii_monthly[!is.na(epii_monthly$covid_mo_date), ]


### clean matrices for monthly epii --------------

#for now hard coding this but see if can be placed in a for loop or make shorter code
epii_months <- epii_months %>% 
  mutate(epii1=case_when(
    covid_mo_epii_1___1=='1'~'1',
    covid_mo_epii_1___2=='1'~'2',
    covid_mo_epii_1___0=='1'~'0',
    covid_mo_epii_1___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_1___1, -covid_mo_epii_1___2, -covid_mo_epii_1___0,-covid_mo_epii_1___9) %>%
  mutate(epii2=case_when(
    covid_mo_epii_2___1=='1'~'1',
    covid_mo_epii_2___2=='1'~'2',
    covid_mo_epii_2___0=='1'~'0',
    covid_mo_epii_2___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_2___1, -covid_mo_epii_2___2, -covid_mo_epii_2___0,-covid_mo_epii_2___9) %>%
  mutate(epii3=case_when(
    covid_mo_epii_3___1=='1'~'1',
    covid_mo_epii_3___2=='1'~'2',
    covid_mo_epii_3___0=='1'~'0',
    covid_mo_epii_3___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_3___1, -covid_mo_epii_3___2, -covid_mo_epii_3___0,-covid_mo_epii_3___9) %>%
  mutate(epii4=case_when(
    covid_mo_epii_4___1=='1'~'1',
    covid_mo_epii_4___2=='1'~'2',
    covid_mo_epii_4___0=='1'~'0',
    covid_mo_epii_4___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_4___1, -covid_mo_epii_4___2, -covid_mo_epii_4___0,-covid_mo_epii_4___9) %>%
  mutate(epii5=case_when(
    covid_mo_epii_5___1=='1'~'1',
    covid_mo_epii_5___2=='1'~'2',
    covid_mo_epii_5___0=='1'~'0',
    covid_mo_epii_5___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_5___1, -covid_mo_epii_5___2, -covid_mo_epii_5___0,-covid_mo_epii_5___9) %>%
  mutate(epii6=case_when(
    covid_mo_epii_6___1=='1'~'1',
    covid_mo_epii_6___2=='1'~'2',
    covid_mo_epii_6___0=='1'~'0',
    covid_mo_epii_6___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_6___1, -covid_mo_epii_6___2, -covid_mo_epii_6___0,-covid_mo_epii_6___9) %>%
  mutate(epii7=case_when(
    covid_mo_epii_7___1=='1'~'1',
    covid_mo_epii_7___2=='1'~'2',
    covid_mo_epii_7___0=='1'~'0',
    covid_mo_epii_7___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_7___1, -covid_mo_epii_7___2, -covid_mo_epii_7___0,-covid_mo_epii_7___9) %>%
  mutate(epii8=case_when(
    covid_mo_epii_8___1=='1'~'1',
    covid_mo_epii_8___2=='1'~'2',
    covid_mo_epii_8___0=='1'~'0',
    covid_mo_epii_8___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_8___1, -covid_mo_epii_8___2, -covid_mo_epii_8___0,-covid_mo_epii_8___9) %>%
  mutate(epii9=case_when(
    covid_mo_epii_9___1=='1'~'1',
    covid_mo_epii_9___2=='1'~'2',
    covid_mo_epii_9___0=='1'~'0',
    covid_mo_epii_9___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_9___1, -covid_mo_epii_9___2, -covid_mo_epii_9___0,-covid_mo_epii_9___9) %>%
  mutate(epii10=case_when(
    covid_mo_epii_10___1=='1'~'1',
    covid_mo_epii_10___2=='1'~'2',
    covid_mo_epii_10___0=='1'~'0',
    covid_mo_epii_10___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_10___1, -covid_mo_epii_10___2, -covid_mo_epii_10___0,-covid_mo_epii_10___9) %>%
  mutate(epii11=case_when(
    covid_mo_epii_11___1=='1'~'1',
    covid_mo_epii_11___2=='1'~'2',
    covid_mo_epii_11___0=='1'~'0',
    covid_mo_epii_11___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii_11___1, -covid_mo_epii_11___2, -covid_mo_epii_11___0,-covid_mo_epii_11___9) %>%
  mutate(epii12=case_when(
    covid_mo_epii_12___1=='1'~'1',
    covid_mo_epii_12___2=='1'~'2',
    covid_mo_epii_12___0=='1'~'0',
    covid_mo_epii_12___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii_12___1, -covid_mo_epii_12___2, -covid_mo_epii_12___0,-covid_mo_epii_12___9) %>%
  mutate(epii13=case_when(
    covid_mo_epii_13___1=='1'~'1',
    covid_mo_epii_13___2=='1'~'2',
    covid_mo_epii_13___0=='1'~'0',
    covid_mo_epii_13___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii_13___1, -covid_mo_epii_13___2, -covid_mo_epii_13___0,-covid_mo_epii_13___9) %>%
  mutate(epii14=case_when(
    covid_mo_epii14___1=='1'~'1',
    covid_mo_epii14___2=='1'~'2',
    covid_mo_epii14___0=='1'~'0',
    covid_mo_epii14___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii14___1, -covid_mo_epii14___2, -covid_mo_epii14___0,-covid_mo_epii14___9) %>%
  mutate(epii15=case_when(
    covid_mo_epii15___1=='1'~'1',
    covid_mo_epii15___2=='1'~'2',
    covid_mo_epii15___0=='1'~'0',
    covid_mo_epii15___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii15___1, -covid_mo_epii15___2, -covid_mo_epii15___0,-covid_mo_epii15___9) %>%
  mutate(epii16=case_when(
    covid_mo_epii16___1=='1'~'1',
    covid_mo_epii16___2=='1'~'2',
    covid_mo_epii16___0=='1'~'0',
    covid_mo_epii16___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii16___1, -covid_mo_epii16___2, -covid_mo_epii16___0,-covid_mo_epii16___9) %>%
  mutate(epii17=case_when(
    covid_mo_epii17___1=='1'~'1',
    covid_mo_epii17___2=='1'~'2',
    covid_mo_epii17___0=='1'~'0',
    covid_mo_epii17___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii17___1, -covid_mo_epii17___2, -covid_mo_epii17___0,-covid_mo_epii17___9) %>%
  mutate(epii18=case_when(
    covid_mo_epii18___1=='1'~'1',
    covid_mo_epii18___2=='1'~'2',
    covid_mo_epii18___0=='1'~'0',
    covid_mo_epii18___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii18___1, -covid_mo_epii18___2, -covid_mo_epii18___0,-covid_mo_epii18___9) %>%
  mutate(epii19=case_when(
    covid_mo_epii19___1=='1'~'1',
    covid_mo_epii19___2=='1'~'2',
    covid_mo_epii19___0=='1'~'0',
    covid_mo_epii19___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii19___1, -covid_mo_epii19___2, -covid_mo_epii19___0,-covid_mo_epii19___9) %>%
  mutate(epii20=case_when(
    covid_mo_epii20___1=='1'~'1',
    covid_mo_epii20___2=='1'~'2',
    covid_mo_epii20___0=='1'~'0',
    covid_mo_epii20___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
select(-covid_mo_epii20___1, -covid_mo_epii20___2, -covid_mo_epii20___0,-covid_mo_epii20___9) %>%
mutate(epii21=case_when(
  covid_mo_epii21___1=='1'~'1',
  covid_mo_epii21___2=='1'~'2',
  covid_mo_epii21___0=='1'~'0',
  covid_mo_epii21___9=='1'~'9',
  TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii21___1, -covid_mo_epii21___2, -covid_mo_epii21___0,-covid_mo_epii21___9) %>%
  mutate(epii22=case_when(
    covid_mo_epii22___1=='1'~'1',
    covid_mo_epii22___2=='1'~'2',
    covid_mo_epii22___0=='1'~'0',
    covid_mo_epii22___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii22___1, -covid_mo_epii22___2, -covid_mo_epii22___0,-covid_mo_epii22___9) %>%
  mutate(epii23=case_when(
    covid_mo_epii23___1=='1'~'1',
    covid_mo_epii23___2=='1'~'2',
    covid_mo_epii23___0=='1'~'0',
    covid_mo_epii23___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii23___1, -covid_mo_epii23___2, -covid_mo_epii23___0,-covid_mo_epii23___9) %>%
  mutate(epii24=case_when(
    covid_mo_epii24___1=='1'~'1',
    covid_mo_epii24___2=='1'~'2',
    covid_mo_epii24___0=='1'~'0',
    covid_mo_epii24___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii24___1, -covid_mo_epii24___2, -covid_mo_epii24___0,-covid_mo_epii24___9) %>%
  mutate(epii25=case_when(
    covid_mo_epii25___1=='1'~'1',
    covid_mo_epii25___2=='1'~'2',
    covid_mo_epii25___0=='1'~'0',
    covid_mo_epii25___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii25___1, -covid_mo_epii25___2, -covid_mo_epii25___0,-covid_mo_epii25___9) %>%
  mutate(epii26=case_when(
    covid_mo_epii26___1=='1'~'1',
    covid_mo_epii26___2=='1'~'2',
    covid_mo_epii26___0=='1'~'0',
    covid_mo_epii26___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii26___1, -covid_mo_epii26___2, -covid_mo_epii26___0,-covid_mo_epii26___9) %>%
  mutate(epii27=case_when(
    covid_mo_epii27___1=='1'~'1',
    covid_mo_epii27___2=='1'~'2',
    covid_mo_epii27___0=='1'~'0',
    covid_mo_epii27___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii27___1, -covid_mo_epii27___2, -covid_mo_epii27___0,-covid_mo_epii27___9) %>%
  mutate(epii28=case_when(
    covid_mo_epii28___1=='1'~'1',
    covid_mo_epii28___2=='1'~'2',
    covid_mo_epii28___0=='1'~'0',
    covid_mo_epii28___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii28___1, -covid_mo_epii28___2, -covid_mo_epii28___0,-covid_mo_epii28___9) %>%
  mutate(epii29=case_when(
    covid_mo_epii29___1=='1'~'1',
    covid_mo_epii29___2=='1'~'2',
    covid_mo_epii29___0=='1'~'0',
    covid_mo_epii29___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii29___1, -covid_mo_epii29___2, -covid_mo_epii29___0,-covid_mo_epii29___9) %>%
  mutate(epii30=case_when(
    covid_mo_epii30___1=='1'~'1',
    covid_mo_epii30___2=='1'~'2',
    covid_mo_epii30___0=='1'~'0',
    covid_mo_epii30___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii30___1, -covid_mo_epii30___2, -covid_mo_epii30___0,-covid_mo_epii30___9) %>%
  mutate(epii31=case_when(
    covid_mo_epii31___1=='1'~'1',
    covid_mo_epii31___2=='1'~'2',
    covid_mo_epii31___0=='1'~'0',
    covid_mo_epii31___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii31___1, -covid_mo_epii31___2, -covid_mo_epii31___0,-covid_mo_epii31___9) %>%
  mutate(epii32=case_when(
    covid_mo_epii32___1=='1'~'1',
    covid_mo_epii32___2=='1'~'2',
    covid_mo_epii32___0=='1'~'0',
    covid_mo_epii32___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii32___1, -covid_mo_epii32___2, -covid_mo_epii32___0,-covid_mo_epii32___9) %>%
  mutate(epii33=case_when(
    covid_mo_epii33___1=='1'~'1',
    covid_mo_epii33___2=='1'~'2',
    covid_mo_epii33___0=='1'~'0',
    covid_mo_epii33___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii33___1, -covid_mo_epii33___2, -covid_mo_epii33___0,-covid_mo_epii33___9) %>%
  mutate(epii34=case_when(
    covid_mo_epii34___1=='1'~'1',
    covid_mo_epii34___2=='1'~'2',
    covid_mo_epii34___0=='1'~'0',
    covid_mo_epii34___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii34___1, -covid_mo_epii34___2, -covid_mo_epii34___0,-covid_mo_epii34___9) %>%
  mutate(epii35=case_when(
    covid_mo_epii35___1=='1'~'1',
    covid_mo_epii35___2=='1'~'2',
    covid_mo_epii35___0=='1'~'0',
    covid_mo_epii35___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii35___1, -covid_mo_epii35___2, -covid_mo_epii35___0,-covid_mo_epii35___9) %>%
  mutate(epii36=case_when(
    covid_mo_epii36___1=='1'~'1',
    covid_mo_epii36___2=='1'~'2',
    covid_mo_epii36___0=='1'~'0',
    covid_mo_epii36___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii36___1, -covid_mo_epii36___2, -covid_mo_epii36___0,-covid_mo_epii36___9) %>%
  mutate(epii37=case_when(
    covid_mo_epii37___1=='1'~'1',
    covid_mo_epii37___2=='1'~'2',
    covid_mo_epii37___0=='1'~'0',
    covid_mo_epii37___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii37___1, -covid_mo_epii37___2, -covid_mo_epii37___0,-covid_mo_epii37___9) %>%
  mutate(epii38=case_when(
    covid_mo_epii38___1=='1'~'1',
    covid_mo_epii38___2=='1'~'2',
    covid_mo_epii38___0=='1'~'0',
    covid_mo_epii38___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii38___1, -covid_mo_epii38___2, -covid_mo_epii38___0,-covid_mo_epii38___9) %>%
  mutate(epii39=case_when(
    covid_mo_epii39___1=='1'~'1',
    covid_mo_epii39___2=='1'~'2',
    covid_mo_epii39___0=='1'~'0',
    covid_mo_epii39___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii39___1, -covid_mo_epii39___2, -covid_mo_epii39___0,-covid_mo_epii39___9) %>%
  mutate(epii40=case_when(
    covid_mo_epii40___1=='1'~'1',
    covid_mo_epii40___2=='1'~'2',
    covid_mo_epii40___0=='1'~'0',
    covid_mo_epii40___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii40___1, -covid_mo_epii40___2, -covid_mo_epii40___0,-covid_mo_epii40___9) %>%
  mutate(epii41=case_when(
    covid_mo_epii41___1=='1'~'1',
    covid_mo_epii41___2=='1'~'2',
    covid_mo_epii41___0=='1'~'0',
    covid_mo_epii41___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii41___1, -covid_mo_epii41___2, -covid_mo_epii41___0,-covid_mo_epii41___9) %>%
 # mutate(epii42=case_when(
  #  covid_mo_epii42___1=='1'~'1',
   # covid_mo_epii42___2=='1'~'2',
    #covid_mo_epii42___0=='1'~'0',
    #covid_mo_epii42___9=='1'~'9',
    #TRUE ~NA_character_
  #)) %>%
  #select(-covid_mo_epii42___1, -covid_mo_epii42___2, -covid_mo_epii42___0,-covid_mo_epii42___9) %>%
  #mutate(epii43=case_when(
   # covid_mo_epii43___1=='1'~'1',
    #covid_mo_epii43___2=='1'~'2',
    #covid_mo_epii43___0=='1'~'0',
    #covid_mo_epii43___9=='1'~'9',
 #   TRUE ~NA_character_
#  )) %>%
 # select(-covid_mo_epii43___1, -covid_mo_epii43___2, -covid_mo_epii43___0,-covid_mo_epii43___9) %>%
  mutate(epii44=case_when(
    covid_mo_epii44___1=='1'~'1',
    covid_mo_epii44___2=='1'~'2',
    covid_mo_epii44___0=='1'~'0',
    covid_mo_epii44___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii44___1, -covid_mo_epii44___2, -covid_mo_epii44___0,-covid_mo_epii44___9) %>%
  mutate(epii45=case_when(
    covid_mo_epii45___1=='1'~'1',
    covid_mo_epii45___2=='1'~'2',
    covid_mo_epii45___0=='1'~'0',
    covid_mo_epii45___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii45___1, -covid_mo_epii45___2, -covid_mo_epii45___0,-covid_mo_epii45___9) %>%
  mutate(epii46=case_when(
    covid_mo_epii46___1=='1'~'1',
    covid_mo_epii46___2=='1'~'2',
    covid_mo_epii46___0=='1'~'0',
    covid_mo_epii46___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii46___1, -covid_mo_epii46___2, -covid_mo_epii46___0,-covid_mo_epii46___9) %>%
  mutate(epii47=case_when(
    covid_mo_epii47___1=='1'~'1',
    covid_mo_epii47___2=='1'~'2',
    covid_mo_epii47___0=='1'~'0',
    covid_mo_epii47___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii47___1, -covid_mo_epii47___2, -covid_mo_epii47___0,-covid_mo_epii47___9) %>%
  mutate(epii48=case_when(
    covid_mo_epii48___1=='1'~'1',
    covid_mo_epii48___2=='1'~'2',
    covid_mo_epii48___0=='1'~'0',
    covid_mo_epii48___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii48___1, -covid_mo_epii48___2, -covid_mo_epii48___0,-covid_mo_epii48___9) %>%
  mutate(epii49=case_when(
    covid_mo_epii49___1=='1'~'1',
    covid_mo_epii49___2=='1'~'2',
    covid_mo_epii49___0=='1'~'0',
    covid_mo_epii49___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii49___1, -covid_mo_epii49___2, -covid_mo_epii49___0,-covid_mo_epii49___9) %>%
  mutate(epii50=case_when(
    covid_mo_epii50___1=='1'~'1',
    covid_mo_epii50___2=='1'~'2',
    covid_mo_epii50___0=='1'~'0',
    covid_mo_epii50___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii50___1, -covid_mo_epii50___2, -covid_mo_epii50___0,-covid_mo_epii50___9) %>%
  mutate(epii51=case_when(
    covid_mo_epii51___1=='1'~'1',
    covid_mo_epii51___2=='1'~'2',
    covid_mo_epii51___0=='1'~'0',
    covid_mo_epii51___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii51___1, -covid_mo_epii51___2, -covid_mo_epii51___0,-covid_mo_epii51___9) %>%
  mutate(epii52=case_when(
    covid_mo_epii52___1=='1'~'1',
    covid_mo_epii52___2=='1'~'2',
    covid_mo_epii52___0=='1'~'0',
    covid_mo_epii52___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii52___1, -covid_mo_epii52___2, -covid_mo_epii52___0,-covid_mo_epii52___9) %>%
  mutate(epii53=case_when(
    covid_mo_epii53___1=='1'~'1',
    covid_mo_epii53___2=='1'~'2',
    covid_mo_epii53___0=='1'~'0',
    covid_mo_epii53___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii53___1, -covid_mo_epii53___2, -covid_mo_epii53___0,-covid_mo_epii53___9) %>%
  mutate(epii54=case_when(
    covid_mo_epii54___1=='1'~'1',
    covid_mo_epii54___2=='1'~'2',
    covid_mo_epii54___0=='1'~'0',
    covid_mo_epii54___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii54___1, -covid_mo_epii54___2, -covid_mo_epii54___0,-covid_mo_epii54___9) %>%
  mutate(epii55=case_when(
    covid_mo_epii55___1=='1'~'1',
    covid_mo_epii55___2=='1'~'2',
    covid_mo_epii55___0=='1'~'0',
    covid_mo_epii55___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii55___1, -covid_mo_epii55___2, -covid_mo_epii55___0,-covid_mo_epii55___9) %>%
  mutate(epii56=case_when(
    covid_mo_epii56___1=='1'~'1',
    covid_mo_epii56___2=='1'~'2',
    covid_mo_epii56___0=='1'~'0',
    covid_mo_epii56___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii56___1, -covid_mo_epii56___2, -covid_mo_epii56___0,-covid_mo_epii56___9) %>%
  mutate(epii57=case_when(
    covid_mo_epii57___1=='1'~'1',
    covid_mo_epii57___2=='1'~'2',
    covid_mo_epii57___0=='1'~'0',
    covid_mo_epii57___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii57___1, -covid_mo_epii57___2, -covid_mo_epii57___0,-covid_mo_epii57___9) %>%
  mutate(epii58=case_when(
    covid_mo_epii58___1=='1'~'1',
    covid_mo_epii58___2=='1'~'2',
    covid_mo_epii58___0=='1'~'0',
    covid_mo_epii58___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii58___1, -covid_mo_epii58___2, -covid_mo_epii58___0,-covid_mo_epii58___9) %>%
  mutate(epii59=case_when(
    covid_mo_epii59___1=='1'~'1',
    covid_mo_epii59___2=='1'~'2',
    covid_mo_epii59___0=='1'~'0',
    covid_mo_epii59___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii59___1, -covid_mo_epii59___2, -covid_mo_epii59___0,-covid_mo_epii59___9) %>%
  mutate(epii60=case_when(
    covid_mo_epii60___1=='1'~'1',
    covid_mo_epii60___2=='1'~'2',
    covid_mo_epii60___0=='1'~'0',
    covid_mo_epii60___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii60___1, -covid_mo_epii60___2, -covid_mo_epii60___0,-covid_mo_epii60___9) %>%
  mutate(epii61=case_when(
    covid_mo_epii61___1=='1'~'1',
    covid_mo_epii61___2=='1'~'2',
    covid_mo_epii61___0=='1'~'0',
    covid_mo_epii61___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii61___1, -covid_mo_epii61___2, -covid_mo_epii61___0,-covid_mo_epii61___9) %>%
  mutate(epii62=case_when(
    covid_mo_epii62___1=='1'~'1',
    covid_mo_epii62___2=='1'~'2',
    covid_mo_epii62___0=='1'~'0',
    covid_mo_epii62___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii62___1, -covid_mo_epii62___2, -covid_mo_epii62___0,-covid_mo_epii62___9) %>%
  mutate(epii63=case_when(
    covid_mo_epii63___1=='1'~'1',
    covid_mo_epii63___2=='1'~'2',
    covid_mo_epii63___0=='1'~'0',
    covid_mo_epii63___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii63___1, -covid_mo_epii63___2, -covid_mo_epii63___0,-covid_mo_epii63___9) %>%
  mutate(epii64=case_when(
    covid_mo_epii64___1=='1'~'1',
    covid_mo_epii64___2=='1'~'2',
    covid_mo_epii64___0=='1'~'0',
    covid_mo_epii64___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii64___1, -covid_mo_epii64___2, -covid_mo_epii64___0,-covid_mo_epii64___9) %>%
  mutate(epii65=case_when(
    covid_mo_epii65___1=='1'~'1',
    covid_mo_epii65___2=='1'~'2',
    covid_mo_epii65___0=='1'~'0',
    covid_mo_epii65___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii65___1, -covid_mo_epii65___2, -covid_mo_epii65___0,-covid_mo_epii65___9) %>%
  mutate(epii66=case_when(
    covid_mo_epii66___1=='1'~'1',
    covid_mo_epii66___2=='1'~'2',
    covid_mo_epii66___0=='1'~'0',
    covid_mo_epii66___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii66___1, -covid_mo_epii66___2, -covid_mo_epii66___0,-covid_mo_epii66___9) %>%
  mutate(epii67=case_when(
    covid_mo_epii67___1=='1'~'1',
    covid_mo_epii67___2=='1'~'2',
    covid_mo_epii67___0=='1'~'0',
    covid_mo_epii67___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii67___1, -covid_mo_epii67___2, -covid_mo_epii67___0,-covid_mo_epii67___9) %>%
  mutate(epii68=case_when(
    covid_mo_epii68___1=='1'~'1',
    covid_mo_epii68___2=='1'~'2',
    covid_mo_epii68___0=='1'~'0',
    covid_mo_epii68___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii68___1, -covid_mo_epii68___2, -covid_mo_epii68___0,-covid_mo_epii68___9) %>%
  mutate(epii69=case_when(
    covid_mo_epii69___1=='1'~'1',
    covid_mo_epii69___2=='1'~'2',
    covid_mo_epii69___0=='1'~'0',
    covid_mo_epii69___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii69___1, -covid_mo_epii69___2, -covid_mo_epii69___0,-covid_mo_epii69___9) %>%
  mutate(epii70=case_when(
    covid_mo_epii70___1=='1'~'1',
    covid_mo_epii70___2=='1'~'2',
    covid_mo_epii70___0=='1'~'0',
    covid_mo_epii70___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii70___1, -covid_mo_epii70___2, -covid_mo_epii70___0,-covid_mo_epii70___9) %>%
  mutate(epii71=case_when(
    covid_mo_epii71___1=='1'~'1',
    covid_mo_epii71___2=='1'~'2',
    covid_mo_epii71___0=='1'~'0',
    covid_mo_epii71___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii71___1, -covid_mo_epii71___2, -covid_mo_epii71___0,-covid_mo_epii71___9) %>%
  mutate(epii72=case_when(
    covid_mo_epii72___1=='1'~'1',
    covid_mo_epii72___2=='1'~'2',
    covid_mo_epii72___0=='1'~'0',
    covid_mo_epii72___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii72___1, -covid_mo_epii72___2, -covid_mo_epii72___0,-covid_mo_epii72___9) %>%
  mutate(epii73=case_when(
    covid_mo_epii73___1=='1'~'1',
    covid_mo_epii73___2=='1'~'2',
    covid_mo_epii73___0=='1'~'0',
    covid_mo_epii73___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii73___1, -covid_mo_epii73___2, -covid_mo_epii73___0,-covid_mo_epii73___9) %>%
  mutate(epii74=case_when(
    covid_mo_epii74___1=='1'~'1',
    covid_mo_epii74___2=='1'~'2',
    covid_mo_epii74___0=='1'~'0',
    covid_mo_epii74___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii74___1, -covid_mo_epii74___2, -covid_mo_epii74___0,-covid_mo_epii74___9) %>%
  mutate(epii75=case_when(
    covid_mo_epii75___1=='1'~'1',
    covid_mo_epii75___2=='1'~'2',
    covid_mo_epii75___0=='1'~'0',
    covid_mo_epii75___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii75___1, -covid_mo_epii75___2, -covid_mo_epii75___0,-covid_mo_epii75___9) %>%
  mutate(epii76=case_when(
    covid_mo_epii76___1=='1'~'1',
    covid_mo_epii76___2=='1'~'2',
    covid_mo_epii76___0=='1'~'0',
    covid_mo_epii76___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii76___1, -covid_mo_epii76___2, -covid_mo_epii76___0,-covid_mo_epii76___9) %>%
  mutate(epii77=case_when(
    covid_mo_epii77___1=='1'~'1',
    covid_mo_epii77___2=='1'~'2',
    covid_mo_epii77___0=='1'~'0',
    covid_mo_epii77___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii77___1, -covid_mo_epii77___2, -covid_mo_epii77___0,-covid_mo_epii77___9) %>%
  mutate(epii78=case_when(
    covid_mo_epii78___1=='1'~'1',
    covid_mo_epii78___2=='1'~'2',
    covid_mo_epii78___0=='1'~'0',
    covid_mo_epii78___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii78___1, -covid_mo_epii78___2, -covid_mo_epii78___0,-covid_mo_epii78___9) %>%
  mutate(epii79=case_when(
    covid_mo_epii79___1=='1'~'1',
    covid_mo_epii79___2=='1'~'2',
    covid_mo_epii79___0=='1'~'0',
    covid_mo_epii79___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii79___1, -covid_mo_epii79___2, -covid_mo_epii79___0,-covid_mo_epii79___9) %>%
  mutate(epii80=case_when(
    covid_mo_epii80___1=='1'~'1',
    covid_mo_epii80___2=='1'~'2',
    covid_mo_epii80___0=='1'~'0',
    covid_mo_epii80___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii80___1, -covid_mo_epii80___2, -covid_mo_epii80___0,-covid_mo_epii80___9) %>%
  mutate(epii81=case_when(
    covid_mo_epii81___1=='1'~'1',
    covid_mo_epii81___2=='1'~'2',
    covid_mo_epii81___0=='1'~'0',
    covid_mo_epii81___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii81___1, -covid_mo_epii81___2, -covid_mo_epii81___0,-covid_mo_epii81___9) %>%
  mutate(epii82=case_when(
    covid_mo_epii82___1=='1'~'1',
    covid_mo_epii82___2=='1'~'2',
    covid_mo_epii82___0=='1'~'0',
    covid_mo_epii82___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii82___1, -covid_mo_epii82___2, -covid_mo_epii82___0,-covid_mo_epii82___9) %>%
  mutate(epii83=case_when(
    covid_mo_epii83___1=='1'~'1',
    covid_mo_epii83___2=='1'~'2',
    covid_mo_epii83___0=='1'~'0',
    covid_mo_epii83___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii83___1, -covid_mo_epii83___2, -covid_mo_epii83___0,-covid_mo_epii83___9) %>%
  mutate(epii84=case_when(
    covid_mo_epii84___1=='1'~'1',
    covid_mo_epii84___2=='1'~'2',
    covid_mo_epii84___0=='1'~'0',
    covid_mo_epii84___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii84___1, -covid_mo_epii84___2, -covid_mo_epii84___0,-covid_mo_epii84___9) %>%
  mutate(epii85=case_when(
    covid_mo_epii85___1=='1'~'1',
    covid_mo_epii85___2=='1'~'2',
    covid_mo_epii85___0=='1'~'0',
    covid_mo_epii85___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii85___1, -covid_mo_epii85___2, -covid_mo_epii85___0,-covid_mo_epii85___9) %>%
  mutate(epii86=case_when(
    covid_mo_epii86___1=='1'~'1',
    covid_mo_epii86___2=='1'~'2',
    covid_mo_epii86___0=='1'~'0',
    covid_mo_epii86___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii86___1, -covid_mo_epii86___2, -covid_mo_epii86___0,-covid_mo_epii86___9) %>%
  mutate(epii87=case_when(
    covid_mo_epii87___1=='1'~'1',
    covid_mo_epii87___2=='1'~'2',
    covid_mo_epii87___0=='1'~'0',
    covid_mo_epii87___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii87___1, -covid_mo_epii87___2, -covid_mo_epii87___0,-covid_mo_epii87___9) %>%
  mutate(epii88=case_when(
    covid_mo_epii88___1=='1'~'1',
    covid_mo_epii88___2=='1'~'2',
    covid_mo_epii88___0=='1'~'0',
    covid_mo_epii88___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii88___1, -covid_mo_epii88___2, -covid_mo_epii88___0,-covid_mo_epii88___9) %>%
  mutate(epii89=case_when(
    covid_mo_epii89___1=='1'~'1',
    covid_mo_epii89___2=='1'~'2',
    covid_mo_epii89___0=='1'~'0',
    covid_mo_epii89___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii89___1, -covid_mo_epii89___2, -covid_mo_epii89___0,-covid_mo_epii89___9) %>%
  mutate(epii90=case_when(
    covid_mo_epii90___1=='1'~'1',
    covid_mo_epii90___2=='1'~'2',
    covid_mo_epii90___0=='1'~'0',
    covid_mo_epii90___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii90___1, -covid_mo_epii90___2, -covid_mo_epii90___0,-covid_mo_epii90___9) %>%
  mutate(epii91=case_when(
    covid_mo_epii91___1=='1'~'1',
    covid_mo_epii91___2=='1'~'2',
    covid_mo_epii91___0=='1'~'0',
    covid_mo_epii91___9=='1'~'9',
    TRUE ~NA_character_
  )) %>% 
  select(-covid_mo_epii91___1, -covid_mo_epii91___2, -covid_mo_epii91___0,-covid_mo_epii91___9) %>%
  mutate(epii92=case_when(
    covid_mo_epii92___1=='1'~'1',
    covid_mo_epii92___2=='1'~'2',
    covid_mo_epii92___0=='1'~'0',
    covid_mo_epii92___9=='1'~'9',
    TRUE ~NA_character_
  )) %>%
  select(-covid_mo_epii92___1, -covid_mo_epii92___2, -covid_mo_epii92___0,-covid_mo_epii92___9) %>%
#rename 42 and 43  
  rename(epii42 = covid_mo_epii42, epii43=covid_mo_epii43,epii_workall =covid_mo_epii_workall,epii_edu =covid_mo_epii_edu,
         epii_home=covid_mo_epii_home,epii_social=covid_mo_epii_social,epii_economic=covid_mo_epii_economic,
         epii_emotion=covid_mo_epii_emotion,epii_health=covid_mo_epii_health,epii_quarantine=covid_mo_epii_quarantine,
         epii_infection=covid_mo_epii_infection,epii_positive=covid_mo_epii_positive,epii_overall=covid_mo_epii_overall,
         epii_stressors=covid_mo_epii_stressors,epii_other=covid_mo_epii_other
         ) %>%
  relocate(epii42,.after=epii41)%>%relocate(epii43,.after=epii42)
#monthly data clean 
#for (i in seq_along(epii_months)){
#  print(names(epii_months)[[i]])
 # print("sick")
#}

###--- long to wide: edit for each month going forward) ------
#rename redcap event name to months
epii_months <- epii_months %>%
  mutate(month_admin = case_when(
    redcap_event_name == "covid_questionnair_arm_1b" ~ "1",
    redcap_event_name == "covid_questionnair_arm_1c" ~ "2",
    redcap_event_name == "covid_questionnair_arm_1d" ~ "3",
    redcap_event_name == "covid_questionnair_arm_1e" ~ "4",
    redcap_event_name == "covid_questionnair_arm_1f" ~ "5",
    TRUE ~ NA_character_
  )) %>% 
  select(-redcap_event_name)%>%
  relocate(month_admin,.after="DOBm")

#epii_month_wide <- epii_month_wide %>% tidyr::spread(key =month_admin, value = v)
#calc age (fix later)

###---write csv---------------
#wide (need to figure out)
#long
write.csv(epii,"/Users/rachelkwon/Desktop/covid/epii_long.csv",na = "")
write.csv(epii_months,"/Users/rachelkwon/Desktop/covid/epiimonthly_long.csv",na = "")
###PSS API PULLL-----------------------------------------------------------------------------
#initial PSS 
pssi_erp <- c("subj","covid1_date","ques_respondent_covid1","covid1_pss1","covid1_pss2","covid1_pss3","covid1_pss4","covid1_pss5","covid1_pss6","covid1_pss7","covid1_pss8","covid1_pss9","covid1_pss10")
pssiERP <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = pssi_erp
)$data
#initial nirs pss
pssi_nirs <- c("subj","covid1_date","ques_respondent_covid1","covid1_pss1","covid1_pss2","covid1_pss3","covid1_pss4","covid1_pss5","covid1_pss6","covid1_pss7","covid1_pss8","covid1_pss9","covid1_pss10")
pssiNIRS <- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = pssi_nirs
)$data

#filter initial pss data
pssiERP <- pssiERP %>%
  filter(redcap_event_name=="covid_questionnair_arm_1")
pssiNIRS <- pssiNIRS %>%
  filter(redcap_event_name=="covid_questionnair_arm_1")
#combine data frames
pss_initial <- full_join(pssiERP,pssiNIRS)
#exclude ineligible subjects
pss_initial <- left_join(subjlist,pss_initial)
pss_initial <- pss_initial[!is.na(pss_initial$covid1_date), ]


#monthly PSS -------------------------------------------------------------------

pss_mo_erp <- c("subj","covid_mo_date","ques_respondent_covid_mo","covid_mo_pss1","covid_mo_pss2","covid_mo_pss3","covid_mo_pss4","covid_mo_pss5","covid_mo_pss6","covid_mo_pss7","covid_mo_pss8","covid_mo_pss9","covid_mo_pss10")
pss_mo_ERP <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = pss_mo_erp
)$data
#initial nirs pss
pss_mo_nirs <- c("subj","covid_mo_date","ques_respondent_covid_mo","covid_mo_pss1","covid_mo_pss2","covid_mo_pss3","covid_mo_pss4","covid_mo_pss5","covid_mo_pss6","covid_mo_pss7","covid_mo_pss8","covid_mo_pss9","covid_mo_pss10")
pss_mo_NIRS<- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = pss_mo_nirs
)$data

pss_month_full <- full_join(pss_mo_ERP,pss_mo_NIRS)
#exclude ineligible subjects
pss_month <- left_join(fullsubjlist,pss_month_full)

pss_month <-pss_month[!is.na(pss_month$covid_mo_date), ]

pss_month <- pss_month %>%
  mutate(month_admin = case_when(
    redcap_event_name == "covid_questionnair_arm_1b" ~ "1",
    redcap_event_name == "covid_questionnair_arm_1c" ~ "2",
    redcap_event_name == "covid_questionnair_arm_1d" ~ "3",
    redcap_event_name == "covid_questionnair_arm_1e" ~ "4",
    redcap_event_name == "covid_questionnair_arm_1f" ~ "5",
    TRUE ~ NA_character_
  )) %>% 
  select(-redcap_event_name)%>%
  relocate(month_admin,.after="DOBm")

### PSS initial SYNTAX-------------------------------------------------------------------
#PSS SYNTAX
#reverse code the positively worded items on the PSS (4,5,7,8) 
# recode 1 to 2 into 1 and 3 to 4 into 2

keys <- c(1,1,1,-1,-1,1,-1,-1,1,1)

pss_score<- pss[,c("covid1_pss1","covid1_pss2","covid1_pss3","covid1_pss4","covid1_pss5","covid1_pss6","covid1_pss7","covid1_pss8","covid1_pss9","covid1_pss10")]
pss_score1 <- reverse.code(keys,pss_score)
pss_score1 <- as.data.frame(pss_score1)

#score the pss data
pss_initial$pss_init_total<- rowSums(pss_score1[,c("covid1_pss1","covid1_pss2","covid1_pss3","covid1_pss4-","covid1_pss5-","covid1_pss6","covid1_pss7-","covid1_pss8-","covid1_pss9","covid1_pss10")])

#sanity check (randomly score data manually to confirm that this is correct
describe(pss$pss_init_total)
ggplot(pss, mapping = aes(x = pss_init_total)) + geom_histogram()

###---- monthly PSS syntax-------
pss_mo_score<- pss_month[,c("covid_mo_pss1","covid_mo_pss2","covid_mo_pss3","covid_mo_pss4","covid_mo_pss5","covid_mo_pss6","covid_mo_pss7","covid_mo_pss8","covid_mo_pss9","covid_mo_pss10")]
pss_score_monthly <- reverse.code(keys,pss_mo_score)
pss_score_monthly <- as.data.frame(pss_score_monthly)
pss_month$pss_month_totalsc<- rowSums(pss_score_monthly[,c("covid_mo_pss1","covid_mo_pss2","covid_mo_pss3","covid_mo_pss4-","covid_mo_pss5-","covid_mo_pss6","covid_mo_pss7-","covid_mo_pss8-","covid_mo_pss9","covid_mo_pss10")])

###----write pss csv----------
write.csv(pss,"/Users/rachelkwon/Desktop/covid/pss.csv",na='')
write.csv(pss_month,"/Users/rachelkwon/Desktop/covid/pss_month.csv",na='')

###----SCHOOLING QUESTIONNAIRES - HOT MESS - inital -----
#eventually need to create redcap pull for all subjects but this pull will just
#be for non sibling subjects
#only for NON SIBLINGs
school_init_erp <- c("subj","covid_mo_date","ques_respondent_covid_mo","covid_school_child","covid_schoolq1","covid_schoolq2",
                   "covid_schoolq3","covid_schoolq4","covid_school_iep","covid_school_iep1","covid_school_comment",
                   "covid_schoolmodel","covid_remotehr","covid_remotehr_2","covid_hybridhr","covid_hybridhr_2","covid_hybridhr_3",
                   "covid_homeschool","covid_schoolother","covid_schoolrate","covid_schoolrate_2","covid_schoolrate_3","covid_schoolrate_4",
                   "covid_schoolyr_iep","covid_schoolrate_iep","covid_schoolyr_cmnt")
schoolinit_ERP <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = school_init_erp
)$data


school_init_nirs <- c("subj","covid_mo_date","ques_respondent_covid_mo","covid_school_child","covid_schoolq1","covid_schoolq2",
                      "covid_schoolq3","covid_schoolq4","covid_school_iep","covid_school_iep1","covid_school_comment",
                      "covid_schoolmodel","covid_remotehr","covid_remotehr_2","covid_hybridhr","covid_hybridhr_2","covid_hybridhr_3",
                      "covid_homeschool","covid_schoolother","covid_schoolrate","covid_schoolrate_2","covid_schoolrate_3","covid_schoolrate_4",
                      "covid_schoolyr_iep","covid_schoolrate_iep","covid_schoolyr_cmnt")
schoolinit_NIRS <- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = school_init_nirs
)$data

school_init_full <- full_join(schoolinit_ERP,schoolinit_NIRS)
#exclude ineligible subjects
school_init<- left_join(fullsubjlist,school_init_full)

school_init<-school_init[!is.na(school_init$covid_mo_date), ] %>% 
  filter(redcap_event_name=="covid_questionnair_arm_1c") %>% filter(covid_school_child==0)%>%
  select(-DOBm,-DOBi,-DOBf,-eligibility)#filters only the 1 child in study

###----SCHOOLING MONTHLY - hot mess one child only for now----------------
school_mo_erp <- c("subj","covid_mo_date","ques_respondent_covid_mo_v2","covid_school_child_v2","covid_schoolq1_v2","covid_schoolq2_v2",
                     "covid_schoolq3_v2","covid_schoolq4_v2","covid_school_iep_v2","covid_school_iep1_v2","covid_school_comment_v2",
                     "covid_schoolmodel_v2","covid_remotehr_v2","covid_remotehr_2_v2","covid_hybridhr_v2","covid_hybridhr_2_v2","covid_hybridhr_3_v2",
                     "covid_homeschool_v2","covid_schoolother_v2","covid_schoolrate_v2","covid_schoolrate_2_v2","covid_schoolrate_3_v2","covid_schoolrate_4_v2",
                     "covid_schoolyr_iep_v2","covid_schoolrate_iep_v2","covid_schoolyr_cmnt_v2")
school_mo_ERP <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = school_mo_erp
)$data

school_mo_nirs <- c("subj","covid_mo_date","ques_respondent_covid_mo_v2","covid_school_child_v2","covid_schoolq1_v2","covid_schoolq2_v2",
                     "covid_schoolq3_v2","covid_schoolq4_v2","covid_school_iep_v2","covid_school_iep1_v2","covid_school_comment_v2",
                     "covid_schoolmodel_v2","covid_remotehr_v2","covid_remotehr_2_v2","covid_hybridhr_v2","covid_hybridhr_2_v2","covid_hybridhr_3_v2",
                     "covid_homeschool_v2","covid_schoolother_v2","covid_schoolrate_v2","covid_schoolrate_2_v2","covid_schoolrate_3_v2","covid_schoolrate_4_v2",
                     "covid_schoolyr_iep_v2","covid_schoolrate_iep_v2","covid_schoolyr_cmnt_v2")
school_mo_NIRS <- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = school_mo_nirs
)$data

school_mo_full <- full_join(school_mo_ERP,school_mo_NIRS)
#exclude ineligible subjects
school_monthly<- left_join(fullsubjlist,school_mo_full)

school_monthly<-school_monthly[!is.na(school_monthly$covid_mo_date), ] %>% 
  filter(redcap_event_name=="covid_questionnair_arm_1d") %>% filter(covid_school_child_v2==0)%>%
  select(-DOBm,-DOBi,-DOBf,-eligibility)#filters only the 1 child in study

school_monthly <- school_monthly %>%
  mutate(month_admin = case_when(
    redcap_event_name == "covid_questionnair_arm_1b" ~ "1",
    redcap_event_name == "covid_questionnair_arm_1c" ~ "2",
    redcap_event_name == "covid_questionnair_arm_1d" ~ "3",
    redcap_event_name == "covid_questionnair_arm_1e" ~ "4",
    redcap_event_name == "covid_questionnair_arm_1f" ~ "5",
    TRUE ~ NA_character_
  )) %>% 
  select(-redcap_event_name)%>%
  relocate(month_admin,.after="subj")
###-write csv for schooling q----------
write.csv(school_init,"/Users/rachelkwon/Desktop/covid/school_init.csv",na='')
write.csv(school_monthly,"/Users/rachelkwon/Desktop/covid/school_monthly.csv",na='')
###-------data request merge----------------------------------------------------------
library(lubridate)
merge1<-left_join(epii,pss,by=c("covid1_date"="covid1_date",'subj'='subj'))
merge2 <- left_join(epii_months,pss_month,by=c("covid_mo_date"="covid_mo_date","subj"="subj"))
merge1$DOBi<-as.Date(parse_date_time(merge1$DOBi, c('mdy')))
merge1$ageChildM<-calc_age(merge1$DOBi,refDate = merge1$covid1_date,unit='month')

#errors here
merge2$DOBi.x<-as.Date(parse_date_time(merge2$DOBi.x, c('mdy')))
merge2$ageChildM<-calc_age(merge2$DOBi.x,refDate = merge2$covid_mo_date,unit='month')

merge1<-merge1%>%select(-DOBi,-DOBf,-DOBm,)%>%
select(-redcap_event_name)%>%
  relocate(month_admin,.after="DOBm")

write.csv(merge1,"/Users/rachelkwon/Desktop/covid/initialfull.csv",na='')
write.csv(merge2,"/Users/rachelkwon/Desktop/covid/monthlyfull.csv",na='')

###-----helper functions------
#standardize dates
date_mdy<- function(x){ as.Date(x, format = "%m-%d-%Y") }

#calc age fuction - need to add in separate script that calls
calc_age <- function(birthDate, refDate = Sys.Date(), unit = "year") {
  
  require(lubridate)
  
  if(grepl(x = unit, pattern = "year")) {
    as.period(interval(birthDate, refDate), unit = 'year')$year
  } else if(grepl(x = unit, pattern = "month")) {
    as.period(interval(birthDate, refDate), unit = 'month')$month
  } else if(grepl(x = unit, pattern = "week")) {
    floor(as.period(interval(birthDate, refDate), unit = 'day')$day / 7)
  } else if(grepl(x = unit, pattern = "day")) {
    as.period(interval(birthDate, refDate), unit = 'day')$day
  } else {
    print("Argument 'unit' must be one of 'year', 'month', 'week', or 'day'")
    NA
  }
  
}