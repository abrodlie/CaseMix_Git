#First create a table with all the unit costs by PoD y year
AnnualUC <- SUSDataAcuteC %>% group_by(HRG,PoDh, FinYear) %>% summarise(Activity = sum(Activity), Spend = sum(Spend), UC = Spend/Activity
) %>% mutate(UC_E = if_else(is.na(UC),0,1)  )

#Replace all infs with NAs

AnnualUC[sapply(AnnualUC, is.infinite)] <- NA

#Put all NAs as zeros

AnnualUC <- AnnualUC %>% mutate_if(is.numeric, replace_na, replace = 0  )

#This gives the prior year unit cost as these are needed in the Laspreyes calc then multiplies activity by these as also needed in Laspreyes calcs 

AnnualUC <- AnnualUC %>% mutate(PriorUC = lag(UC), PriorActivity = lag(Activity) , PriorUC_E = if_else(is.na(lag(UC_E)),0,lag(UC_E)) , 
    ActivityPriorUC = Activity*PriorUC*PriorUC_E, PriorActivityPriorUC = PriorActivity*PriorUC*PriorUC_E, ActivityInc = Activity*PriorUC_E, PriorActivityInc = PriorActivity*PriorUC_E)

#Replace all infs with NAs

AnnualUC[sapply(AnnualUC, is.infinite)] <- NA

#Put all NAs as zeros

AnnualUC <- AnnualUC %>% mutate_if(is.numeric, replace_na, replace = 0  )

#Now calculate the index 
Laspreyes <- AnnualUC %>% group_by(PoDh,FinYear) %>% summarise(ThisYearLasp = sum(ActivityPriorUC), PriorYearLasp = sum(PriorActivityPriorUC), 
        ThisYearRaw = sum(ActivityInc), LastYearRaw = sum(PriorActivityInc)   ) %>% mutate(LaspGrowth = ThisYearLasp/PriorYearLasp-1, 
              RawGrowth = ThisYearRaw/LastYearRaw -1 , CaseMixGrwth = (1+LaspGrowth)/(1+RawGrowth) -1 )
                                                               
LaspreyesOutput <- Laspreyes %>% mutate("Financial Year" = case_when(FinYear == 2014 ~ "2014/15",FinYear == 2015 ~ "2015/16",FinYear == 2016 ~ "2016/17",FinYear == 2017 ~ "2017/18",FinYear == 2018 ~ "2018/19",FinYear == 2019 ~ "2019/20", FinYear == 2020 ~ "2020/21" ) 
                                        ) %>% ungroup %>% dplyr::select(FinYear,  LaspGrowth,RawGrowth,CaseMixGrwth,PoDh) 

ElectiveOutput <- LaspreyesOutput %>% filter(FinYear != 2014,PoDh == "EL") %>% select(FinYear,RawGrowth,LaspGrowth,CaseMixGrwth )

NonElectiveOutput <- LaspreyesOutput %>% filter(FinYear != 2014,PoDh == "NEL") %>% select(FinYear,RawGrowth,LaspGrowth,CaseMixGrwth)

#Now do current year Year to Date

write.table(LaspreyesOutput,"clipboard-16384",sep="\t")
