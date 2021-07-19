#Calculate the day case effect on productivity

#Read in the reference costs data
RefCosts1819OrdDC <- read_excel("RefCosts1819.xlsx",4)

#Select juts the ord and day case unit costs as this is all you need from the ref costs data

RefCosts1819OrdDC <- RefCosts1819OrdDC %>% select(Currency, `Elective Unit Cost`,`Day Case Unit Cost` ) %>% rename(OrdUC = `Elective Unit Cost`,DCUC = `Day Case Unit Cost` )

#Limit the SUS acute data to just Electives as all we need for this analysis

ElecData <- SUSDataAcuteC %>% filter(PoDh == "EL") 

#Produce a table of PoD,HRG and Financial Year, but just activity as unit costs will come from the ref costs collection

ElecDataAnn <- ElecData %>% group_by(PoD,HRG,FinYear) %>% summarise(Activity = sum(Activity))

#Pivot wider to separate out the Day Cases and Ordinary values still tidy data principles

ElecDataAnn <- ElecDataAnn %>% pivot_wider(names_from = PoD , values_from = Activity)

#Put NAs as zero

ElecDataAnn <- ElecDataAnn %>% mutate_if(is.numeric, replace_na, replace = 0  )

# To simplify rename as DC and Ord

ElecDataAnn <- ElecDataAnn %>% rename(DC = `Day Case` , Ord = 'Ord. Elective Admission')

# Add in a total which is DC + Ord needed as denom for ratios but also easy to apply last years ratio to get prior year costs

ElecDataAnn <- ElecDataAnn %>% mutate(Total = DC + Ord ) %>% mutate(DCrate = DC/Total ) 

#Order so the lag function will work

ElecDataAnn <- arrange(ElecDataAnn,HRG,FinYear)

#Add in the prior year ratio so we can see what the cost would be at the prior year ratio

ElecDataAnn <- ElecDataAnn %>% mutate(PriorDCRate = lag(DCrate))

#Calculate the DC and Ordinary amounts if at last year's rates

ElecDataAnn <- ElecDataAnn %>% mutate(DCPriorRate = Total*PriorDCRate, OrdPriorRate = Total*(1-PriorDCRate))

#Add in the ref cost unit costs for DCs and Ords

ElecDataAnn <- left_join(ElecDataAnn,RefCosts1819OrdDC, by = c("HRG" = "Currency"))

#Add in a column which states whether there is a unit cost for that HRG so can be excluded from the analysis

ElecDataAnn <- ElecDataAnn %>% mutate(CostExist = if_else(is.na(DCUC) | is.na(OrdUC),0,1), PriorExist = if_else(is.na(lag(FinYear)),0,1), TotalUsed = Total*PriorExist*CostExist )

#Look at the Cost in each HRG either at this years ratio or last years

ElecDataAnn <- ElecDataAnn %>% mutate(Cost = if_else(CostExist == 1 & PriorExist == 1 , DC*DCUC + Ord*OrdUC,0)  , PriorCost = if_else(CostExist == 1 & PriorExist == 1, DCPriorRate*DCUC + OrdPriorRate*OrdUC,0 ) )

#Calculate proportion of activity used

((ElecDataAnn$CostExist * ElecDataAnn$PriorExist) %*% ElecDataAnn$Total)/sum(ElecDataAnn$Total)

Summary <- ElecDataAnn %>% group_by(FinYear) %>% summarise(Cost = sum(Cost,is.na=FALSE), PriorCost = sum(PriorCost, is.na=FALSE), Total = sum(Total), TotalUsed = sum(TotalUsed))

Summary <- Summary %>% mutate(DCEffect = scales::percent_format(0.1)( PriorCost/Cost - 1), PercUsed = scales::percent_format(0.1)(TotalUsed/Total))

scales::percent_format(0.005, accuracy = 0.1)

SummaryOutput <- Summary %>% filter(FinYear != 2014) %>% select(FinYear,DCEffect) %>% mutate(FinYear = paste(FinYear - 2000,"/",FinYear - 1999, sep="", collapse=NULL))

write.table(SummaryOutput ,"clipboard-16384",sep="\t")

#Now try and work out what it means in terms of day case numbers

ElecDataAnnVol <- ElecDataAnn %>% mutate(DCCurrentRateNoError = DC*CostExist*PriorExist , DCPriorRateNoError = DCPriorRate*CostExist*PriorExist ) 

ElecDataAnnVolByYear <- ElecDataAnnVol %>% group_by(FinYear) %>% summarise(DCCurrentRate = sum(DCCurrentRateNoError, is.na = TRUE), 
        DCPriorRate = sum(DCPriorRateNoError, na.rm = TRUE), DCTotal = sum(DC))  %>% mutate(ExtraDCs = DCCurrentRate - DCPriorRate, DCIncrease = DCCurrentRate - lag(DCCurrentRate), DCTotalIncrease = DCTotal - lag(DCTotal)) 

#Now look at a national level not HRG to see how much it takes to change the 

NonHRGrates <- ElecDataAnn %>% group_by(FinYear) %>% summarise(DC = sum(DC), Ord = sum(Ord)) %>% mutate(Total = DC + Ord ) %>% mutate(DCrate = DC/Total ) 

#Now calculate average national unit costs for the HRGs with unit costs

NatUnitCosts <- ElecDataAnn %>% filter(CostExist == 1) %>% select(FinYear,HRG, DC, Ord, DCUC, OrdUC) %>% mutate(DC_Cost = DC*DCUC, Ord_Cost = Ord*OrdUC)

NatUnitCosts <- NatUnitCosts %>% group_by(FinYear) %>% summarise(DC = sum(DC), DC_Cost = sum(DC_Cost), Ord = sum(Ord), Ord_Cost = sum(Ord_Cost)
    ) %>% mutate(DC_UC = DC_Cost/DC, Ord_UC = Ord_Cost/Ord, DCRate = DC/(Ord+DC), PriorDCRate = lag(DCRate),Total = DC+Ord, PriorDC = PriorDCRate*Total, 
                  PriorOrd = (1-PriorDCRate)*Total, Total_Cost = Ord_Cost + DC_Cost, Prior_Total_Cost = PriorDC*DC_UC + PriorOrd*Ord_UC , DCEffect = Prior_Total_Cost/Total_Cost - 1)

NonHRGSummary <- NatUnitCosts  %>%  filter(FinYear != 2014) %>%  select( FinYear, DCEffect ) %>% mutate(DCEffect = scales::percent_format(0.1)( DCEffect) , FinYear = paste(FinYear - 2000,"/",FinYear - 1999, sep="", collapse=NULL)) 

saveRDS(SummaryOutput, file = "SummaryOutput.RDS")

saveRDS(NonHRGSummary, file = "NonHRGSummary.RDS")
