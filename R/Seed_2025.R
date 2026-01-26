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
Seed_New_Methodology = read.csv("./Data/Seed_New_Methodology.csv")
# Seed_New_Methodology = dplyr::rename(Seed_New_Methodology,Area.Code = Area.Code..FAO.)
# Seed_New_Methodology = dplyr::rename(Seed_New_Methodology,Item.Code = Item.Code..FAO.)
Seed_New_Methodology = dplyr::select(Seed_New_Methodology,Area.Code,Item.Code,Year,Value)
Seed = rbind(Seed_Old_Methodology,Seed_New_Methodology)

StartYear=1961
EndYear=2023
Seed$Item = Seed$Item.Code
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
Seed = left_join(Seed,N_Removal_Seed,by=c("Item.Code"="Item.Code..FAO..Seed"))
Seed = Seed %>%
  rowwise() %>%
  mutate(Value_Seed_2 = Value*N_Removal*1000) %>%
  ungroup() %>%
  group_by(Area.Code,Year,Nutrient.Code) %>%
  summarise(Value_Seed_2 = sum(Value_Seed_2,na.rm=TRUE)) %>%
  ungroup()
Seed$Nutrient.Code = as.double(Seed$Nutrient.Code)