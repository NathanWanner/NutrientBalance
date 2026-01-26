# Updated comments Jan 26, 2026
library(dplyr)
library(tidyr)

# Calculate how much of total production comes from our selected crops in 2014
# Take only the aggregate crop data
Crops = read.csv(gzfile("./Data/Production_Crops_Livestock_E_All_Data_(Normalized).gz"))
Crops_2014 = filter(Crops,Element.Code==5312,Year==2014,!is.na(Value))

N_Removal = read.csv("./Data/Nutrient_Removal_Updated_3.csv")
# N_Removal = filter(N_Removal,fao_only_or_combined == "Combined")
N_Removal = dplyr::select(N_Removal,item_code,N_kg_per_t_fresh_wt,P_kg_per_t_fresh_wt, K_kg_per_t_fresh_wt)
# N_Removal = filter(N_Removal,Average==0)
N_Removal = dplyr::rename(N_Removal,
                   Item.Code = item_code,
                   N_Grain = N_kg_per_t_fresh_wt,
                   P_Grain = P_kg_per_t_fresh_wt,
                   K_Grain = K_kg_per_t_fresh_wt)

N_Removal = dplyr::select(N_Removal,Item.Code,N_Grain,P_Grain,K_Grain)
N_Removal = gather(N_Removal,Nutrient,Nutrient_Removal,-Item.Code)
N_Removal$Nutrient.Code[N_Removal$Nutrient=="N_Grain"] = 3102
N_Removal$Nutrient.Code[N_Removal$Nutrient=="P_Grain"] = 3103
N_Removal$Nutrient.Code[N_Removal$Nutrient=="K_Grain"] = 3104

Item_Group = read.csv("./Data/Item_Group_Updated.csv")
Item_Group = dplyr::left_join(Crops_2014,select(Item_Group,-Item),by="Item.Code")
Item_Group = dplyr::right_join(Item_Group,N_Removal,by="Item.Code")
# Item_Group = Item_Group %>%
#   rowwise() %>%
#   mutate(Item.Group.Code = ifelse(Legume == 1 && !is.na(Legume), 9999,Item.Group.Code)) %>%
#   mutate(Nutrient_Removal = ifelse(Legume == 1 && !is.na(Legume), Nutrient_Removal*.2,Nutrient_Removal)) %>%
#   ungroup()
Nutrient_Group = Item_Group %>%
  dplyr::group_by(Item.Group.Code,Nutrient.Code) %>%
  dplyr::summarise(Removal = weighted.mean(Nutrient_Removal,Value,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Removal))

# This nitrogen removal data can be used with aggregate harvested area data
