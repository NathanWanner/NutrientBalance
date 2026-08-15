# This R script calculate N Balance INPUTS by country/year
# Author: WANNERn Wanner
# Date: December 4, 2017
# Updated comments Jan 26, 2026
library(dplyr)
area_regions_hieararchy = read.csv("./Tables/areas_regions_hierarchy.csv")
FAO_Country_Region = read.csv("./Tables/FAO_Country_Region.csv")
# N_Fixation_Coefficients = read.csv("../Data/N_Fixation.csv")
# First gather the inputs: syntehtic fertilizer, Managed_Manure, and seed
# Data for synthetic fertilizers
# Unit of measure is tonnes of nutrients
Manure_Item_Codes = read.csv("./Tables/item_names_GHG.csv")
Manure_Item_Codes = unique(Manure_Item_Codes)
Item_Group = read.csv("./Data/Item_Group_Updated.csv",colClasses = c("CPCCode" = "character"))

Manure_Ratios = read.csv("./Tables/Manure_Nutrient_Ratios_Updated_2.csv",colClasses = c("CPCCode" = "character"))

Manure_Ratios = select(Manure_Ratios,
                       CPCCode,
                       N_ratio,
                       P_ratio,
                       K_ratio)
Manure_Ratios = rename(Manure_Ratios,
                       N_Ratio = N_ratio,
                       P_Ratio = P_ratio,
                       K_Ratio = K_ratio)
Manure_Ratios = gather(Manure_Ratios,Nutrient,Nutrient_Ratio,-CPCCode)
Manure_Ratios$Nutrient.Code[Manure_Ratios$Nutrient=="N_Ratio"] = 3102
Manure_Ratios$Nutrient.Code[Manure_Ratios$Nutrient=="P_Ratio"] = 3103
Manure_Ratios$Nutrient.Code[Manure_Ratios$Nutrient=="K_Ratio"] = 3104

# This should be read from SWS, but waiting for confirmation from Griffiths on which dataset
# Data is still read from csv file and data is updated through 2024
Fert = read.csv("./Data/Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv")
Fert = filter(Fert,Element.Code==5157,Item.Code%in%c(3102,3103,3104),Area.Code<400)
StartYear=1961
EndYear=2024

Fert = select(Fert,Area.Code,Item.Code,Year,Value)
Fert$Item[Fert$Item.Code==3102]="Nitrogen"
Fert$Item[Fert$Item.Code==3103]="Phosphorous"
Fert$Item[Fert$Item.Code==3104]="Potassium"
Fert = Data_Update(StartYear,EndYear,Fert)
Fert = select(Fert,-Item)
Fert = Fert %>%
  mutate(Value_Fert = Value*1000)
# Convert P2O5 to P
Fert$Value_Fert[Fert$Item.Code==3103] = Fert$Value_Fert[Fert$Item.Code==3103]*.436
# Convert K2O to K
Fert$Value_Fert[Fert$Item.Code==3104] = Fert$Value_Fert[Fert$Item.Code==3104]*.830

# Here is getData call for IFA data, but it is not updated through 2024 in SWS
# Fert_IFA_key <- DatasetKey(domain = "Fertilizers", 
#                          dataset = "ifa_data", 
#                          dimensions = list(
#                            geographicAreaM49 = Dimension(name = "geographicAreaM49", 
#                                                          keys = GetCodeList("Fertilizers", "ifa_data", "geographicAreaM49")[type == "country", code]), 
#                            ItemFert = Dimension(name = "measuredItemFertCom", 
#                                                keys = c("F100","F200","F300")),
#                            Element = Dimension(name = "measuredElement", 
#                                               keys = "5157"),
#                            Element2 = Dimension(name = "measuredElementFertNut", 
#                                             keys = c("K2O","P2O5","N")),
#                            timePointYears = Dimension(name = "timePointYears", 
#                                                       keys = as.character(1961:2024)))
# )
# 
# 
# Fert_IFA <- GetData(key = Fert_IFA_key)
Fert_IFA = read.csv("./Data/IFA_countries_2026.csv")
# Take out former terrirories from IFA fertilizer data
Fert_IFA = filter(Fert_IFA,!is.na(Year))
Fert_IFA = dplyr::rename(Fert_IFA,Value = Consumption)
Fert_IFA$ElementCode = 5157
Fert_IFA = dplyr::filter(Fert_IFA,ElementCode==5157)
# Fert_IFA = rename(Fert_IFA,ItemCode=Common_ItemCode)
# Fert_IFA$ItemCode = as.character(Fert_IFA$ItemCode)
Fert_IFA$ItemCode[Fert_IFA$Product=="Grand Total N"] = 3102
Fert_IFA$ItemCode[Fert_IFA$Product=="Grand Total P2O5"] = 3103
Fert_IFA$ItemCode[Fert_IFA$Product=="Grand Total K2O"] = 3104
Fert_IFA = dplyr::filter(Fert_IFA,!is.na(Value))
# Fert_IFA = dplyr::rename(Fert_IFA,ItemCode=Nutrient.Code)
Fert_IFA = select(Fert_IFA,
                  AreaCode,
                  ItemCode,
                  Year,
                  Value)
# NW:  Took out data tretatment for IFA data.  Many series now start from 
# 2013 onwards
# Fert IFA data is in 1000 tonnes
# Fert = rename(Fert,Value="Value_New")
# Fert = filter(Fert,Item.Code==3102)
# Fert_IFA = inner_join(Fert_IFA,select(FAO_Country_Region,
#                                       Country.Code,
#                                       Start.Year,
#                                       End.Year),
#                       by=c("AreaCode"="Country.Code"))
# Fert_IFA$Start.Year[is.na(Fert_IFA$Start.Year)] = 1961
# Fert_IFA$End.Year[is.na(Fert_IFA$End.Year)] = 2021
# Fert_IFA = filter(Fert_IFA,Year<=End.Year)
# Fert_IFA = filter(Fert_IFA,Year>=Start.Year)
# Fert_IFA = select(Fert_IFA,-Start.Year,-End.Year)
# Fert_IFA$Item = Fert_IFA$ItemCode
# Fert_IFA = Data_Update(StartYear,EndYear,rename(Fert_IFA,Area.Code=AreaCode,Item.Code=ItemCode))
# Fert_IFA = rename(Fert_IFA,AreaCode=Area.Code,ItemCode=Item.Code)
# Fert_IFA = select(Fert_IFA,-Item)
Fert_IFA = inner_join(Fert_IFA,select(areas_regions_hierarchy,AreaCode),by=c("AreaCode"="AreaCode"))
Fert_IFA = data.frame(Fert_IFA)
Fert_IFA = Fert_IFA %>%
  mutate(Value_Fert_IFA = Value*1000*1000)
# Convert P2O5 to P
Fert_IFA$Value_Fert_IFA[Fert_IFA$ItemCode==3103] = Fert_IFA$Value_Fert_IFA[Fert_IFA$ItemCode==3103]*.436
# Convert K2O to K
Fert_IFA$Value_Fert_IFA[Fert_IFA$ItemCode==3104] = Fert_IFA$Value_Fert_IFA[Fert_IFA$ItemCode==3104]*.830
# Data for Managed_Manure
# Unit of measure is kg of Nitrogen
# Here is 2026 version
Manure_key <- DatasetKey(domain = "ghg_emissions_at_farm_gate", 
                        dataset = "ghg_emissions_from_livestock_2026_silent_update", 
                        dimensions = list(
                          geographicAreaM49 = Dimension(name = "geographicAreaM49", 
                                                        keys = c(GetCodeList("ghg_emissions_at_farm_gate", "ghg_emissions_from_livestock_2026_silent_update", "geographicAreaM49")[type == "country", code],"159")), 
                          Element = Dimension(name = "ghg_measuredElement_disseminated", 
                                                 keys = "72381"),
                          Source = Dimension(name = "ghg_source", 
                                              keys = "3050"),
                          Item = Dimension(name = "ghg_measuredItemCPC_disseminated", 
                                           keys = "F1755"),
                          timePointYears = Dimension(name = "timePointYears", 
                                                     keys = as.character(1961:2024)))
)


Managed_Manure <- GetData(key = Manure_key)
Managed_Manure = rename(Managed_Manure,
                        Element.Code=ghg_measuredElement_disseminated,
                        Source.Code=ghg_source,
                        Item.Code..CPC.=ghg_measuredItemCPC_disseminated)
Managed_Manure$geographicAreaM49 = as.integer(Managed_Manure$geographicAreaM49)

# Managed_Manure = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))

Managed_Manure = filter(Managed_Manure,Element.Code ==72381,
                        Item.Code..CPC.%in%c("F1755"),Source.Code==3050)
Managed_Manure$Item.Code=3102
Managed_Manure$Item[Managed_Manure$Item.Code==3102] = "Nitrogen"
Managed_Manure = left_join(Managed_Manure,select(m49,M49.Code,Country.Code),by=c("geographicAreaM49" = "M49.Code"))
Managed_Manure = rename(Managed_Manure,
                        Area.Code=Country.Code,
                        Year=timePointYears)
Managed_Manure = select(Managed_Manure,Area.Code,Item.Code,Year,Value)
Managed_Manure$Item = "Manure"
Managed_Manure$Year = as.integer(Managed_Manure$Year)
Managed_Manure = Data_Update(StartYear,EndYear,Managed_Manure)
Managed_Manure = rename(Managed_Manure,Value_Managed_Manure=Value)
Managed_Manure = select(Managed_Manure,-Item)
# write.csv(Managed_Manure,file="C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Managed_Manure.csv")

# This is actually manure applied to soils (correct) and it is renamed to managed manure
# Here is 2025 version
Manure_key <- DatasetKey(domain = "ghg_emissions_at_farm_gate", 
                         dataset = "ghg_emissions_from_livestock_2026_silent_update", 
                         dimensions = list(
                           geographicAreaM49 = Dimension(name = "geographicAreaM49", 
                                                         keys = c(GetCodeList("ghg_emissions_at_farm_gate", "ghg_emissions_from_livestock_2026_silent_update", "geographicAreaM49")[type == "country", code],"159")), 
                           Element = Dimension(name = "ghg_measuredElement_disseminated", 
                                               keys = "72380"),
                           Source = Dimension(name = "ghg_source", 
                                              keys = "3050"),
                           Item = Dimension(name = "ghg_measuredItemCPC_disseminated", 
                                            keys = unique(Manure_Ratios$CPCCode)),
                           timePointYears = Dimension(name = "timePointYears", 
                                                      keys = as.character(1961:2024)))
)


Manure_Pasture <- GetData(key = Manure_key)
Manure_Pasture = rename(Manure_Pasture,
                        Element.Code=ghg_measuredElement_disseminated,
                        Source.Code=ghg_source,
                        Item.Code..CPC.=ghg_measuredItemCPC_disseminated,
                        Year=timePointYears)

# Manure_Pasture = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))

# Manure_Pasture = filter(Manure_Pasture,Element.Code==72380,
#                         Item.Code%in%unique(Manure_Ratios$Item_Code),Source.Code==3050)
Manure_Pasture$geographicAreaM49 = as.integer(Manure_Pasture$geographicAreaM49)
Manure_Pasture = left_join(Manure_Pasture,select(m49,M49.Code,Country.Code),by=c("geographicAreaM49" = "M49.Code"))
Manure_Pasture = rename(Manure_Pasture,
                        Area.Code=Country.Code)

Manure_Pasture = left_join(Manure_Pasture,Manure_Ratios,by=c("Item.Code..CPC." = "CPCCode")) %>%
  mutate(Value_Manure_Pasture = Value*Nutrient_Ratio) %>%
  group_by(Area.Code,Nutrient.Code,Year) %>%
  summarise(Value_Manure_Pasture = sum(Value_Manure_Pasture,na.rm=TRUE))

Manure_Pasture= rename(Manure_Pasture,Item.Code=Nutrient.Code)
Manure_Pasture$Item[Manure_Pasture$Item.Code==3102] = "Nitrogen"
Manure_Pasture$Item[Manure_Pasture$Item.Code==3103] = "Phosphate"
Manure_Pasture$Item[Manure_Pasture$Item.Code==3104] = "Potash"
Manure_Pasture = select(Manure_Pasture,Area.Code,Item.Code,Year,Value_Manure_Pasture)
Manure_Pasture = rename(Manure_Pasture,Value=Value_Manure_Pasture)
Manure_Pasture$Item = "Manure"
Manure_Pasture$Year = as.integer(Manure_Pasture$Year)
Manure_Pasture = Data_Update(StartYear,EndYear,Manure_Pasture)
Manure_Pasture = rename(Manure_Pasture,Value_Manure_Pasture=Value)
Manure_Pasture = select(Manure_Pasture,-Item)

# write.csv(Manure_Pasture,file="C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Pasture.csv")

# Manure_Pasture = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Pasture.csv")
# Manure_Pasture = dplyr::select(Manure_Pasture,-X)


# Here is 2026 version
Manure_key <- DatasetKey(domain = "ghg_emissions_at_farm_gate", 
                         dataset = "ghg_emissions_from_livestock_2026_silent_update", 
                         dimensions = list(
                           geographicAreaM49 = Dimension(name = "geographicAreaM49", 
                                                         keys = c(GetCodeList("ghg_emissions_at_farm_gate", "ghg_emissions_from_livestock_2026_silent_update", "geographicAreaM49")[type == "country", code],"159")), 
                           Element = Dimension(name = "ghg_measuredElement_disseminated", 
                                               keys = "72386"),
                           Source = Dimension(name = "ghg_source", 
                                              keys = "3050"),
                           Item = Dimension(name = "ghg_measuredItemCPC_disseminated", 
                                            keys = unique(Manure_Ratios$CPCCode)),
                           timePointYears = Dimension(name = "timePointYears", 
                                                      keys = as.character(1961:2024)))
)


Treated_Manure <- GetData(key = Manure_key)
Treated_Manure = rename(Treated_Manure,
                        Element.Code=ghg_measuredElement_disseminated,
                        Source.Code=ghg_source,
                        Item.Code..CPC.=ghg_measuredItemCPC_disseminated,
                        Year=timePointYears)
# Treated_Manure = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))
# 
# Treated_Manure = filter(Treated_Manure,Element.Code==72386,
#                         Item.Code%in%unique(Manure_Ratios$Item_Code),Source.Code==3050)
Treated_Manure$geographicAreaM49 = as.integer(Treated_Manure$geographicAreaM49)
Treated_Manure = left_join(Treated_Manure,select(m49,M49.Code,Country.Code),by=c("geographicAreaM49" = "M49.Code"))
Treated_Manure = rename(Treated_Manure,
                        Area.Code=Country.Code)
Treated_Manure = left_join(Treated_Manure,Manure_Ratios,by=c("Item.Code..CPC." = "CPCCode")) %>%
  mutate(Value_Treated_Manure = Value*Nutrient_Ratio) %>%
  group_by(Area.Code,Nutrient.Code,Year) %>%
  summarise(Value_Treated_Manure = sum(Value_Treated_Manure,na.rm=TRUE))

Treated_Manure= rename(Treated_Manure,Item.Code=Nutrient.Code)
Treated_Manure$Item[Treated_Manure$Item.Code==3102] = "Nitrogen"
Treated_Manure$Item[Treated_Manure$Item.Code==3103] = "Phosphate"
Treated_Manure$Item[Treated_Manure$Item.Code==3104] = "Potash"
Treated_Manure = select(Treated_Manure,Area.Code,Item.Code,Year,Value_Treated_Manure)
Treated_Manure = rename(Treated_Manure,Value=Value_Treated_Manure)
Treated_Manure$Item = "Manure"
Treated_Manure$Year = as.integer(Treated_Manure$Year)
Treated_Manure = Data_Update(StartYear,EndYear,Treated_Manure)
Treated_Manure = rename(Treated_Manure,Value_Managed_Manure=Value)
Treated_Manure = select(Treated_Manure,-Item)
Treated_Manure = filter(Treated_Manure,Item.Code%in%c(3103,3104))

# write.csv(Treated_Manure,file="C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Treated_Manure.csv")
# 
# Treated_Manure = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Treated_Manure.csv")
# Treated_Manure = dplyr::select(Treated_Manure,-X)

Managed_Manure = rbind(Managed_Manure,Treated_Manure)
# Temporary until updated manure data is received
# Managed_Manure = Managed_Manure %>%
#   group_by(Area.Code,Item.Code) %>% 
#   complete(Year = min(Year):2024) %>%
#   tidyr::fill(Value_Managed_Manure,.direction=c("down")) %>%
#   ungroup()
Managed_Manure = arrange(Managed_Manure,Area.Code,Item.Code,Year)

# Biological Nitrogen Fixataion
# Biological Nitrogen Fixataion
N_Fixation = read.csv("./Data/total-fixation-country-MgN.csv")
# N_Fixation = select(N_Fixation,-Country,-m49)
# N_Fixation = gather(N_Fixation,Year,Value,2:ncol(N_Fixation))
# N_Fixation$Year = as.integer(substr(N_Fixation$Year,2,length(N_Fixation$Year)))
# N_Fixation = rename(N_Fixation,Area.Code=AreaCode)
N_Fixation$Item = "Nitrogen"
N_Fixation$Item.Code = 3102
N_Fixation$Value = N_Fixation$Main*1000
N_Fixation = arrange(N_Fixation,Area.Code,Year)
N_Fixation = Data_Update(StartYear,EndYear,N_Fixation)
N_Fixation = rename(N_Fixation,BNF=Value)
# Calculate N fixation for China
# N_Fixation_China = N_Fixation %>%
#   filter(Area.Code%in%c(41,
#                         96,
#                         128,
#                         214)) %>%
#   group_by(Item,Item.Code,Year) %>%
#   summarise(BNF = sum(BNF,na.rm=TRUE)) %>%
#   ungroup() %>%
#   mutate(Area.Code=351) %>%
#   select(colnames(N_Fixation))

# N_Fixation = rbind(N_Fixation,N_Fixation_China)
N_Fixation = as.data.frame(N_Fixation)

N_Fixation$Nutrient.Code=3102

# Residues = select(Residues,Area.Code,Element.Code,Item,Item.Code,Year,Value)
# Residues = Data_Update_Biodiversity(StartYear,EndYear,Residues)
# Residues = Residues %>%
#   group_by(Area.Code,Year) %>%
#   summarise(Value_Residues=sum(Value,na.rm=TRUE))
# Residues$Value_Residues = Residues$Value_Residues*.8
# Residues$Nutrient.Code=3102


