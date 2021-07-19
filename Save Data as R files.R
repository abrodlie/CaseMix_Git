

library(readxl)
library(openxlsx)
library(tidyverse)

memory.limit(size = 56000)

# Read in the data
SUSData <- read_csv("Data/APCHRG2122M2.csv")

save(SUSData, file = "Data/SUSData.Rda")

#load("J:/FPAEIG/FPMA/Analysis/Briefing and Reporting/Monthly CWA & Prody/National Activity Product/SR2020/SUSData.Rda")

#Does a summary of all the national data NOTE NEED TO REALLY ADD IN THE COMMISSIONER TYPE TO REMOVE PRIVATE PATIENTS

SUSDataNat <- SUSData %>% group_by(Discharge_Year ,Discharge_Month,Dimention_1,Dimention_7_HRG,adjusted) %>% summarise(Activity = sum(adjusted), Spend = sum(Total_Cost_current))

save(SUSDataNat, file = "Data/SUSDataNat.Rda")

#Limits to what the JAR classes as acute organisations by limiting to just codes that start with R 

SUSDataAcute <- SUSData %>% filter(substr(JAR_Provider_Reporting,1,1) == "R")

SUSDataAcute$FinYear <- ifelse(SUSDataAcute$Discharge_Month > 3, SUSDataAcute$Discharge_Year,SUSDataAcute$Discharge_Year - 1 ) 

SUSDataAcute <- SUSDataAcute %>% filter(PAT_Commissioner_Type != "Private Patient") 



SUSDataAcute <- SUSDataAcute %>% group_by(FinYear,Discharge_Month, Discharge_Year, Dimention_1,Dimention_7_HRG) %>% summarise(Activity = sum(adjusted), Spend = sum(Total_Cost_current))

InitialCheckAcute <- SUSDataAcute %>% group_by(FinYear,Dimention_1) %>% summarise(Activity = sum(Activity)) %>% pivot_wider(names_from = Dimention_1, values_from = Activity)


save(SUSDataAcute,file = "Data/SUSDataAcute.Rda")

write.table(InitialCheckAcute,"clipboard-16384",sep="\t")

# Now load in the outpatient data

OPdata <- read_csv("Data/OPHRG2122M2.csv")

OPdata$FinYear <- ifelse(OPdata$Discharge_Month > 3, OPdata$Discharge_Year,OPdata$Discharge_Year - 1 ) 

OPDataNat <- OPdata %>% group_by(Discharge_Year ,Discharge_Month,Dimention_1,Dimention_7_HRG,adjusted) %>% summarise(Activity = sum(adjusted), Spend = sum(Total_Cost_current))

OPDataAcute <- OPdata %>% filter(substr(JAR_Provider_Reporting,1,1) == "R")

OPDataAcute <- OPDataAcute %>% filter(PAT_Commissioner_Type != "Private Patient") 

OPDataAcute <- OPDataAcute %>% group_by(FinYear,Discharge_Month, Discharge_Year, Dimention_1,Dimention_7_HRG) %>% summarise(Activity = sum(adjusted), Spend = sum(Total_Cost_current)) 

save(OPDataAcute,file = "Data/OPDataAcute.Rda")

InitialCheckOPAcute <- OPDataAcute %>% group_by(FinYear,Dimention_1) %>% summarise(Activity = sum(Activity)) %>% pivot_wider(names_from = Dimention_1
                            , values_from = Activity) 

write.table(InitialCheckOPAcute,"clipboard-16384",sep="\t")

DevonTest <- OPdata %>% filter(JAR_Provider_Reporting == "RH8") %>% group_by(Discharge_Month,Discharge_Year,
       PAT_Commissioner_Type) %>%  summarise(Activity = sum(adjusted)) %>% pivot_wider(names_from = PAT_Commissioner_Type, values_from = Activity  ) %>% arrange(Discharge_Year,Discharge_Month)

write.table(DevonTest,"clipboard-16384",sep="\t")
DevonTest202102 <- OPdata %>% filter(Discharge_Month == 2, Discharge_Year == 2021, JAR_Provider_Reporting == "RH8") %>% group_by(PAT_Commissioner_Type,Dimention_7_HRG ) %>%  summarise(Activity = sum(adjusted)) %>% pivot_wider(names_from = PAT_Commissioner_Type, values_from = Activity) %>% arrange(Dimention_7_HRG) 

write.table(DevonTest202102,"clipboard-16384",sep="\t")
