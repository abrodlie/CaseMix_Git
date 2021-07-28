OPandElec <- ElecDataAnn %>% select(HRG, FinYear,DC,Ord)

OPDataAcuteAnn <- OPDataAcute %>% group_by(FinYear,Dimention_7_HRG) %>% summarise(OP = sum(Activity))

OPandElec <- full_join(OPandElec,OPDataAcuteAnn, by = c("FinYear", "HRG" = "Dimention_7_HRG"))

OPandElec <- OPandElec %>% mutate_if(is.numeric, replace_na, replace = 0)

OPandElecAnn <- OPandElec %>% group_by(FinYear) %>% summarise(DC = sum(DC), Ord = sum(Ord), OP = sum(OP) ) %>% mutate(Total = DC + Ord + OP, OPRate = OP/Total)

OPandElec <- OPandElec %>% mutate(Total = DC + Ord + OP, OPRate = OP/Total)

