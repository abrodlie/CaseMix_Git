#This code calculates the case-mix by month using principles of tidy data and reproducible analytical pipelines

#setwd("C:/Users/abrodlie/Department of Health and Social Care/NW029 - National Activity Product/QtrlyCaseMix")

library(readxl)
library(openxlsx)
library(tidyverse)
library(scales)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(plotly)

memory.limit(size = 56000)

load("SUSDataAcute.Rda")

RefCosts1819 <- read_excel("RefCosts1819.xlsx",1)

RefCosts1718 <- read_excel("RefCosts1718.xlsx",1)

RefCosts1617 <- read_excel("RefCosts1617.xlsx",1)

Tariff2021 <- read_excel("Tariff2021.xlsx",1)

#Rename two of the variables to make the code clearer

SUSDataAcuteC <- SUSDataAcute %>% rename(HRG = Dimention_7_HRG, PoD = Dimention_1)

#Add in high level PoD

SUSDataAcuteC <- SUSDataAcuteC %>% mutate(PoDh = if_else(PoD %in% c("Day Case","Ord. Elective Admission"),"EL",if_else(PoD %in% c("Emergency Admission","Other Non-elective Admission"), "NEL","OTH")))


SUSDataAcuteC$Spend <- as.numeric(SUSDataAcuteC$Spend)

#Produce a national PoD Level Table

PodActivity <- SUSDataAcuteC %>% group_by(PoD,FinYear) %>% summarise(Activity = sum(Activity)) %>% arrange(FinYear) %>% pivot_wider(names_from = PoD, values_from = Activity  )
PodActivity$`Day Case Growth` <- PodActivity$`Day Case`/lag(PodActivity$`Day Case`)-1
PodActivity$`Emergency Admission Growth` <- PodActivity$`Emergency Admission`/lag(PodActivity$`Emergency Admission`)-1
PodActivity$`Ord. Elective Admission Growth` <- PodActivity$`Ord. Elective Admission`/lag(PodActivity$`Ord. Elective Admission`)-1
PodActivity$`Other Non-elective Admission Growth` <- PodActivity$`Other Non-elective Admission`/lag(PodActivity$`Other Non-elective Admission`)-1

write.table(PodActivity,"clipboard-16384",sep="\t")

#Produce a table of non case-mix adjusted growths

RawGrowths <- SUSDataAcuteC %>% group_by(PoDh,FinYear) %>% summarise(Activity = sum(Activity)) %>% arrange(FinYear) %>% pivot_wider(names_from = PoDh, values_from = Activity  )
RawGrowths$ELGrwth <-  scales::percent( RawGrowths$EL/lag(RawGrowths$EL)- 1 )
RawGrowths$NELGrwth <- scales::percent(RawGrowths$NEL/lag(RawGrowths$NEL) -1 )

write.table(RawGrowths,"clipboard-16384",sep="\t")

#Group it by PoDh then calculate a unit cost at this level of granularity

AnnualUC <- SUSDataAcuteC %>% group_by(HRG, FinYear,PoDh) %>% summarise(Activity = sum(Activity), Spend = sum(Spend), UC = Spend/Activity
                                                                        ) %>% mutate(UC_E = if_else(is.na(UC),0,1)  )

#Replace all infs with NAs

AnnualUC[sapply(AnnualUC, is.infinite)] <- NA

#Put all NAs as zeros

AnnualUC <- AnnualUC %>% mutate_if(is.numeric, replace_na, replace = 0  )

#Look at Elective monthly profile using 2018 prices

MthlyEL <- SUSDataAcuteC %>% filter(PoDh == "EL") %>% mutate(FinMonth = if_else(Discharge_Month < 4 , Discharge_Month + 9, Discharge_Month -3)
) %>% group_by(FinYear,FinMonth,PoDh,HRG) %>% summarise(Activity = sum(Activity))

ELPrices2018 <- AnnualUC %>% filter(FinYear == 2018,PoDh == "EL") %>% ungroup %>% select(HRG,UC,UC_E)

#Add the 2018/19 prices to this table
MthlyELPrices2018 <- left_join(MthlyEL,ELPrices2018, by = "HRG")

InputTable <- MthlyELPrices2018 

#Call the Monthly Loop function

source("MonthlyLoop.R")

MTHEL2018P <- MoMPivot
YTDEL2018P <- YTDPivot
MTHEL2018P_Chart <- fig %>% layout(title = "Elective Monthly Case-Mix Index")

#Now use 18/19 costs

#Add the 2018/19 costs to the MthlyEL table

MthlyELCosts2018 <- left_join(MthlyEL,select(RefCosts1819,Currency,El_Cost_2018), by = c("HRG"="Currency" ))

MthlyELCosts2018 <- MthlyELCosts2018 %>% rename(UC = El_Cost_2018) %>% mutate(UC_E = if_else(is.na(UC),0,1))

InputTable <- MthlyELCosts2018 

#Call the Monthly Loop function

source("MonthlyLoop.R")

MTHEL2018C <- MoMPivot
YTDEL2018C <- YTDPivot
MTHEL2018C_Chart <- fig

MTHEL2018C_Chart <- MTHEL2018C_Chart %>% layout(title="Elective Case-Mix")

#Now do non-electives

MthlyNEL <- SUSDataAcuteC %>% filter(PoDh == "NEL") %>% mutate(FinMonth = if_else(Discharge_Month < 4 , Discharge_Month + 9, Discharge_Month -3)
) %>% group_by(FinYear,FinMonth,PoDh,HRG) %>% summarise(Activity = sum(Activity))

NELPrices2018 <- AnnualUC %>% filter(FinYear == 2018,PoDh == "NEL") %>% ungroup %>% select(HRG,UC,UC_E)

#Add the 2018/19 prices to this table
MthlyNELPrices2018 <- left_join(MthlyNEL,NELPrices2018, by = "HRG")

InputTable <- MthlyNELPrices2018 

#Call the Monthly Loop function

source("MonthlyLoop.R")

MTHNEL2018P <- MoMPivot
YTDNEL2018P <- YTDPivot
MTHNEL2018P_Chart <- fig

#Add the 2018/19 costs to the MthlyNEL table

MthlyNELCosts2018 <- left_join(MthlyNEL,select(RefCosts1819,Currency,NEl_Cost_2018), by = c("HRG"="Currency" ))

MthlyNELCosts2018 <- MthlyNELCosts2018 %>% rename(UC = NEl_Cost_2018) %>% mutate(UC_E = if_else(is.na(UC),0,1))

InputTable <- MthlyNELCosts2018 

#Call the Monthly Loop function

source("MonthlyLoop.R")

MTHNEL2018C <- MoMPivot
YTDNEL2018C <- YTDPivot
MTHNEL2018C_Chart <- fig

MTHNEL2018C_Chart <- MTHNEL2018C_Chart %>% layout(title="Non-Elective Case-Mix")

write.table(MTHEL2018C,"clipboard-16384",sep="\t")


saveRDS(MTHEL2018C_Chart, file = "ELCharts.RDS")
saveRDS(MTHNEL2018C_Chart, file = "NELCharts.RDS")

MTHNEL2018C_Chart
