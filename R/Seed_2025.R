# Updated comments Jan 26, 2026
# Data for seed
library(dplyr)
library(tidyr)

N_Removal_Seed = read.csv("./Tables/Nutrient_Removal_Seed.csv")
Seed_Old_Methodology = read.csv("./Data/Seed_Old_Methodology.csv")
Seed_Old_Methodology = dplyr::filter(Seed_Old_Methodology,Year<2010,Element=="Seed")
Seed_Old_Methodology = dplyr::rename(Seed_Old_Methodology,Area.Code = Area.Code..FAO.)
Seed_Old_Methodology = dplyr::rename(Seed_Old_Methodology,Item.Code = Item.Code..FAO.)
Seed_Old_Methodology = dplyr::select(Seed_Old_Methodology,Area.Code,Item.Code,Year,Value)

# Updated seed data not available yet, and could not find element code 5527 in the dataset fbs_balanced
Seed_New_Methodology = read.csv("./Data/Seed_New_Methodology.csv")
# 
# Seed_key <- DatasetKey(domain = "disseminated", 
#                          dataset = "fbs_consolidated", 
#                          dimensions = list(
#                            geographicAreaM49 = Dimension(name = "geographicAreaM49", 
#                                                          keys = c(GetCodeList("suafbs", "fbs_balanced_", "geographicAreaM49")[type == "country", code],"159")), 
#                            Element = Dimension(name = "measuredElementSuaFbs", 
#                                                keys = c("5527")),
#                            Item = Dimension(name = "measuredItemFbsSua", 
#                                             keys = GetCodeList("suafbs", "fbs_balanced_", "measuredItemFbsSua")$code),
#                            timePointYears = Dimension(name = "timePointYears", 
#                                                       keys = as.character(2010:2024)))
# )
# 
# 
# Seed_New_Methodology <- GetData(key = Seed_key)
# Seed_New_Methodology_China = Seed_New_Methodology %>%
#   filter(geographicAreaM49%in%c("156"," 158","446","344")) %>%
#   group_by(measuredElementSuaFbs,measuredItemFbsSua,timePointYears) %>%
#   summarise(Value=sum(Value,na.rm=TRUE)) %>%
#   mutate(geographicAreaM49 ="159",
#          flagObservationStatus ="",
#          flagMethod="") %>%
#   ungroup()
# Seed_New_Methodology = rbind(Seed_New_Methodology,Seed_New_Methodology_China)
# Seed_New_Methodology = Seed_New_Methodology %>%
#   mutate(measuredItemFbsSua = gsub("S", "", measuredItemFbsSua))
Seed_New_Methodology = Seed_New_Methodology %>%
  mutate(Item.Code..FBS. = gsub("S", "", Item.Code..FBS.))
# Seed_New_Methodology$measuredItemFbsSua[Seed_New_Methodology$measuredItemFbsSua=="21910.03"] = "2555"
# Seed_New_Methodology = filter(Seed_New_Methodology,!measuredItemFbsSua=="21910.03")
# Seed_New_Methodology = Seed_New_Methodology %>%
#   group_by(geographicAreaM49,measuredElementSuaFbs,measuredItemFbsSua,timePointYears) %>%
#   # summarise(Value=sum(Value,na.rm=TRUE)/1000) %>%
#   summarise(Value=sum(Value,na.rm=TRUE)) %>%
#   mutate(flagObservationStatus="",flagMethod="") %>%
#   ungroup() 
# Seed_New_Methodology$measuredItemFbsSua = as.integer(Seed_New_Methodology$measuredItemFbsSua)
# Seed_New_Methodology = rename(Seed_New_Methodology,
#                               Element.Code=measuredElementSuaFbs ,
#                               Item.Code=measuredItemFbsSua)
# Seed_New_Methodology$geographicAreaM49 = as.integer(Seed_New_Methodology$geographicAreaM49)
# Seed_New_Methodology = left_join(Seed_New_Methodology,select(m49,M49.Code,Country.Code),by=c("geographicAreaM49" = "M49.Code"))
# Seed_New_Methodology = rename(Seed_New_Methodology,
#                               Area.Code=Country.Code,
#                               Year=timePointYears)
Seed_New_Methodology = dplyr::select(Seed_New_Methodology,Area.Code,Item.Code,Year,Value)
Seed = rbind(Seed_Old_Methodology,Seed_New_Methodology)

StartYear=1961
EndYear=2024
Seed$Item = Seed$Item.Code
Seed$Year = as.integer(Seed$Year)
Seed = Data_Update(StartYear,EndYear,Seed)
Seed = dplyr::select(Seed,-Item)
Item_Group = read.csv("./Tables/Item_Group_Updated.csv")

N_Removal_Seed$Item_Code_Removal = as.integer(N_Removal_Seed$Item_Code_Removal)
N_Removal_Seed = left_join(N_Removal_Seed,select(Item_Group,Item.Code,Item.Group.Code),by=c("Item_Code_Removal" = "Item.Code"))
Nutrient_Group_Seed = spread(Nutrient_Group,Nutrient.Code,Removal)
N_Removal_Seed = left_join(N_Removal_Seed,Nutrient_Group_Seed,by=c("Item.Group.Code"))
N_Removal_Seed$N_kg_per_t_fresh_wt[is.na(N_Removal_Seed$N_kg_per_t_fresh_wt)] = N_Removal_Seed$`3102`[is.na(N_Removal_Seed$N_kg_per_t_fresh_wt)]
N_Removal_Seed$P_kg_per_t_fresh_wt[is.na(N_Removal_Seed$P_kg_per_t_fresh_wt)] = N_Removal_Seed$`3103`[is.na(N_Removal_Seed$P_kg_per_t_fresh_wt)]
N_Removal_Seed$K_kg_per_t_fresh_wt[is.na(N_Removal_Seed$K_kg_per_t_fresh_wt)] = N_Removal_Seed$`3104`[is.na(N_Removal_Seed$K_kg_per_t_fresh_wt)]

N_Removal_Seed = gather(select(N_Removal_Seed, Item.Code..FAO..Seed,N_kg_per_t_fresh_wt, P_kg_per_t_fresh_wt, K_kg_per_t_fresh_wt),"Nutrient.Code","N_Removal",-Item.Code..FAO..Seed)
N_Removal_Seed$Nutrient.Code[N_Removal_Seed$Nutrient.Code=="N_kg_per_t_fresh_wt"] = 3102
N_Removal_Seed$Nutrient.Code[N_Removal_Seed$Nutrient.Code=="P_kg_per_t_fresh_wt"] = 3103
N_Removal_Seed$Nutrient.Code[N_Removal_Seed$Nutrient.Code=="K_kg_per_t_fresh_wt"] = 3104

# I AM HERE
Seed$Item.Code = as.integer(Seed$Item.Code)
Seed = left_join(Seed,N_Removal_Seed,by=c("Item.Code"="Item.Code..FAO..Seed"))
Seed = Seed %>%
  rowwise() %>%
  mutate(Value_Seed_2 = Value*N_Removal*1000) %>%
  ungroup() %>%
  group_by(Area.Code,Year,Nutrient.Code) %>%
  summarise(Value_Seed_2 = sum(Value_Seed_2,na.rm=TRUE)) %>%
  ungroup()
Seed$Nutrient.Code = as.double(Seed$Nutrient.Code)