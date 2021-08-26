#This Code takes a table with Financial Years, Months, HRG as grouping variables and Activity,Unit Costs,UC_E (whether the unit cost is available) 
#as the observed variables.

#It then uses this to produce two pivot tables one with UC by month and then unit costs YTD. 

#Replace all infs with NAs

InputTable[sapply(InputTable, is.infinite)] <- NA

#Put all NAs as zeros

InputTable <- InputTable %>% mutate_if(is.numeric, replace_na, replace = 0  )

#First step is to calculate the Spend and the Activity used by HRG so these can then be grouped up by month over all HRGs to get and overall unit cost
CalcTable <- InputTable %>% mutate(Spend = Activity*UC*UC_E, ActivityUsed = Activity*UC_E) 

#This produces a long table that gives by finyear and finmonth the spend and activity sumed over all HRGs
UCbyM <- CalcTable %>% group_by(FinYear,FinMonth) %>% summarise(Spend = sum(Spend), Activity = sum(ActivityUsed), UC = Spend/Activity ) 

#The next bit produces the YTD spend and activity used to get a YTD unit cost (YTDUC). This uses a loop along all the rows of 
n= nrow(UCbyM) 

UCbyM$YTDSpend <- 0
UCbyM$YTDActivity <- 0

for (i in 1:n)
{
  j <- UCbyM$FinMonth[i]
  UCbyM$YTDSpend[i] <- sum(UCbyM$Spend[(i-(j-1)):i])  
  UCbyM$YTDActivity[i] <- sum(UCbyM$Activity[(i-(j-1)):i])  
}

UCbyM <- UCbyM %>% mutate(YTDUC = YTDSpend/YTDActivity)

#This produces two pivot tables one for unit costs by month the next for YTD unit costs by month
MoMPivot <- UCbyM %>% select(FinYear,FinMonth,UC) %>% pivot_wider(values_from = UC, names_from = FinMonth)

YTDPivot <- UCbyM %>% select(FinYear,FinMonth,YTDUC) %>% pivot_wider(values_from = YTDUC, names_from = FinMonth)

#Now produce the monthly case-mix chart

#Remove the pre 2016 fin years and then add in a case-mix index where April 2016 = 100

UCbyM <- UCbyM %>% filter(FinYear > 2015) %>% arrange(FinYear,FinMonth) %>% ungroup %>% mutate(UCI = (UC/UC[1])*100)

ChartData <- UCbyM %>% select(FinYear,FinMonth,UCI) %>% pivot_wider(names_from = "FinYear" , values_from = "UCI" )

rm(fig)

fig <- plot_ly(width = 750, height = 500) %>%
  layout(title = "Monthly Case-Mix",
         xaxis = list(title = "Financial Month",autotick = F, dtick = 1),
         yaxis = list (title = "Case-Mix Index"),
         data = ChartData,
         autosize = F, 
         legend = list(x=0,y= -0.2,orientation = 'h')
         )

m= ncol(ChartData)

looper <- names(ChartData)[2:m] 

for(trace in looper){
  #Makes the 2020 line 5 times thicker
  n <- if_else(trace == 2020,5,3)
  fig <- fig %>% plotly::add_trace(x = ~FinMonth,
                                   y = as.formula(paste0("~`", trace, "`")), name = paste0(trace,"/",as.numeric(substring(trace,3,4)) + 1),
                                   type = 'scatter',
                                   mode = 'lines',
                                   line = list( width = n))
}

fig 

#Produce a chart which shows pandemic vs pre-pandemic levels

PandvPre <- UCbyM %>% select(FinYear, FinMonth,UCI) %>% mutate(PrePandComp = case_when(
  FinYear == 2020 & FinMonth <12 ~ UCI/lag(UCI,12)-1,
  FinYear == 2021 & FinMonth <12 ~ UCI/lag(UCI,24)-1,
  FinYear == 2020 & FinMonth == 12 ~ UCI/lag(UCI,24)-1
  )) %>% filter(FinYear > 2019) %>% mutate(CalMonth =if_else(FinMonth < 10 , FinMonth+3,FinMonth-9), CalYear=if_else(FinMonth < 10 , FinYear,FinYear+1) ) %>% mutate(X = as.Date(paste("01",CalMonth,CalYear), format = "%d %m %Y"))
  
PrePandCompChart <- plot_ly(PandvPre, x = ~X, y = ~ PrePandComp, type = 'scatter',
                            mode = 'lines')

        
