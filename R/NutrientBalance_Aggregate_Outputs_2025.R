# This R script calculate N Balance OUTPUTS by country/year
# Author: Nathan Wanner
# Date: December 4, 2017
# Updated comments Jan 26, 2026
# This R script calculates the components of the nutrient balance through the
# use of auxillary scripts and the outputs from crop harvest.

message('The Pest_Cropland plugin has started.')

suppressMessages({
  library(data.table)
  library(ggplot2)
  library(faosws)
  library(faoswsModules)
  library(faoswsUtil)
  library(faoswsFlag)
  library(dplyr)
  library(tidyr)
  library(extremevalues)
  library(zoo)
})

if(CheckDebug()){
  SETT <- ReadSettings("sws.yml")
  GetTestEnvironment(baseUrl = SETT[["server"]], token = SETT[["token"]])
}

startTime <- Sys.time()
# Read the area/country hierarchical tables
areas_regions_hierarchy = read.csv("./Tables/areas_regions_hierarchy.csv")
areas_regions_hierarchy = select(areas_regions_hierarchy,
                                 AreaCode,
                                 RegionCode,
                                 RegionName,
                                 GroupCodeAnnex,
                                 GroupNameAnnex)
FAO_Country_Region = read.csv("./Tables/FAO_Country_Region.csv")
FAO_Country_Region = select(FAO_Country_Region,
                            Country.Code,
                            Start.Year,
                            End.Year)
m49 = read.csv("./Tables/m49.csv")
areas_merge = read.csv("./Tables/areas_merge_modified.csv")
FAO_Country_Group_M49 = read.csv("./Tables/FAO_Country_Group_M49_No_China.csv")
Crops_CPC = read.csv("./Tables/Production_Item_Codes.csv")
# Read the auxiliary functions
source("./Functions/Data_Aggregate.R")
source("./Functions/Data_Aggregate_Biodiversity_2025.R")
# Fodder_Crops = read.csv("../Data/Fodder_Crops_Production.csv")
# Calculate removal from pasture from R script below.
# This code is complimentary and not used for current dissemination
source('./R/Feed.R')
# Calculate and harmonize the inputs
source('./R/NutrientBalance_Inputs_2025.R')
# Import the deposition data and harmonize
source('./R/NDeposition_2025.R')
# Calculate item group averages to be used for gap-filling for missing nutrient
# concentration information
source('./R/Item_Aggregates_Nutrients_2025.R')
# Calculate the seed according to the food balance sheets.
# This code is auxiliary and the actual seed calculations used for
# dissemination are performed further down in the code of this R script
source('./R/Seed_2025.R')

# Import cropland and agricultural land information.
Agr_Area = read.csv("./Data/Inputs_LandUse_E_All_Data_(Normalized).csv")
# Agr_Area = dplyr::rename(Agr_Area,
#                          Area.Code = Area.Code..FAO.,
#                          Item.Code = Item.Code..FAO.,
#                          Element.Code=Element.Code..FAO.)

Agr_Area = filter(Agr_Area,Item.Code%in%c(6610,6620),Element.Code==5110)
Agr_Area = select(Agr_Area,
                  Area.Code,
                  Element.Code,
                  Item.Code,
                  Item,
                  Year,
                  Value)

# Max year for Agr_Area is 2016
# Min year for Agr_Area is 1961
StartYear=1961
EndYear=2024
Agr_Area = Data_Update(StartYear,EndYear,Agr_Area)
Agr_Area = select(Agr_Area,-Item.Code)
Agr_Area = spread(Agr_Area,key=Item,Value)
Agr_Area = rename(Agr_Area,Agr_Land=`Agricultural land`)
Agr_Area = rename(Agr_Area,CropLand=Cropland)

# Data for Agr Area is 1000 ha
# Convert to ha
Agr_Area$Agr_Land = Agr_Area$Agr_Land*1000
Agr_Area$CropLand = Agr_Area$CropLand*1000

# Fixes for cropland for Montenegro and the Netherlands


Item_Group = read.csv("./Tables/Item_Group_Updated.csv")
# Item_Group = Item_Group %>%
#   rowwise() %>%
#   mutate(Item.Group.Code = ifelse(Legume == 1 && !is.na(Legume), 9999,Item.Group.Code)) %>%
#   ungroup()

# Data for crops
# Crops = read.csv("../Data/Production_Crops_Livestock_E_All_Data_(Normalized).csv")
Item_Group_Crops = read.csv("./Data/Item_Group_Updated.csv",colClasses = c("CPCCode" = "character"))
Crops_key <- DatasetKey(domain = "disseminated", 
                        dataset = "aproduction_validated", 
                        dimensions = list(
                          geographicAreaM49 = Dimension(name = "geographicAreaM49", 
                                                        keys = GetCodeList("disseminated", "aproduction_validated", "geographicAreaM49")[,code]), 
                          geaElement = Dimension(name = "measuredElement", 
                                                 keys = "5510"),
                          geaItem = Dimension(name = "measuredItemCPC", 
                                              keys = as.character(unique(Item_Group_Crops$CPCCode))), 
                          timePointYears = Dimension(name = "timePointYears", 
                                                     keys = as.character(1961:2024)))
)


Crops <- GetData(key = Crops_key)
# Calculate China - only available from 1991 in SWS
Crops_China = Crops %>%
  filter(geographicAreaM49%in%c("156"," 158","446","344")) %>%
  group_by(measuredElement,measuredItemCPC,timePointYears) %>%
  summarise(Value=sum(Value,na.rm=TRUE)) %>%
  mutate(geographicAreaM49 ="159",
         flagObservationStatus ="",
         flagMethod="") %>%
  ungroup()
Crops = rbind(filter(Crops,!(geographicAreaM49=="159")),Crops_China)
Crops=left_join(Crops,select(Item_Group_Crops,Item.Code,CPCCode,Item),by=c("measuredItemCPC" = "CPCCode"))
Crops = rename(Crops,Flag=flagObservationStatus)
m49$M49.Code = as.character(m49$M49.Code)
Crops = left_join(Crops,select(m49,M49.Code,Country.Code),by=c("geographicAreaM49" = "M49.Code"))
Crops = rename(Crops,
               Area.Code=Country.Code,
               Element.Code=measuredElement,
               Year=timePointYears)

# Crops = read.csv("./Data/Production_Crops_Livestock_E_All_Data_(Normalized).csv")
# Crops = rbind(Crops,Fodder_Crops)
Crops = select(Crops,Area.Code,Item.Code,Item,Element.Code,Year,Value,Flag)
Crops$Value[Crops$Flag=="M"] = 0
# Take the production data and make conversion
# Read crops from SWS

StartYear=1961
EndYear=2024
Crops = filter(Crops,Element.Code%in%c(5510))
Crops = filter(Crops,Item.Code<1000|Item.Code>1900)
Crops = Data_Update_Biodiversity(StartYear,EndYear,Crops)

# # Crops_SWS = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2024/Data/export.csv")
# Crops_SWS = read.csv("./Data/data.csv")
# 
# Crops_SWS$geographicAreaM49[Crops_SWS$geographicAreaM49==1248] = 159
# Crops_SWS = filter(Crops_SWS,Status == "E")
# # Use only 27 EU countries
# FAO_Country_Group_EU = filter(FAO_Country_Group_M49,Country.Group=="European Union (27)")
# FAO_Country_Group_EU = filter(FAO_Country_Group_EU,!Country%in%c("Germany Fr","Germany Nl","Belgium-Luxembourg"))
# # Crops_CPC$CPC.Code = as.integer(Crops_CPC$CPC.Code)
# Crops_CPC$CPC.Code = as.character(Crops_CPC$CPC.Code)
# Crops_SWS$measuredItemCPC = as.character(Crops_SWS$measuredItemCPC)
# Crops_SWS = left_join(Crops_SWS,select(Crops_CPC,Item.Code,Item,CPC.Code),by=c("measuredItemCPC"="CPC.Code"))
# Crops_SWS = filter(Crops_SWS,!is.na(Item.Code),measuredElement==5510,timePointYears>=2020,geographicAreaM49%in%unique(FAO_Country_Group_EU$M49.Code))
# Crops_SWS = left_join(Crops_SWS,select(m49,M49.Code,Country.Code,Country),by=c("geographicAreaM49"="M49.Code"))
# Crops_SWS = rename(Crops_SWS,
#                    Area.Code = Country.Code,
#                    Area = Country,
#                    Area.Code..M49. = geographicAreaM49,
#                    Element.Code = measuredElement,
#                    Item.Code..CPC. = measuredItemCPC,
#                    Flag = Status)
# Crops_SWS$Year.Code = Crops_SWS$Year
# Crops_SWS$Unit = "tonnes"
# Crops_SWS$Element = "Production"
# Crops_SWS = filter(Crops_SWS,Element.Code%in%c(5510))
# Crops_SWS = filter(Crops_SWS,Item.Code<1000|Item.Code>1900)
# Crops_SWS = filter(Crops_SWS,!is.na(Item.Code))
# Crops_SWS$Item = Crops_SWS$Item.y
# Crops_SWS = left_join(Crops_SWS,dplyr::select(m49,M49.Code,Country.Code),by=c("Area.Code..M49." = "M49.Code"))
# Crops = rbind(Crops,dplyr::select(Crops_SWS,colnames(Crops)))


Crops = rename(Crops,Value_Prod=Value)
Crops = inner_join(Crops,Item_Group,by=c("Item.Code"))
Crops = left_join(Crops,N_Removal,by=c("Item.Code"))
Crops = left_join(Crops,Nutrient_Group,by=c("Item.Group.Code","Nutrient.Code"))

Crops = Crops %>%
  rowwise() %>%
  mutate(N_Removal_Prod = Value_Prod*Nutrient_Removal) %>%
  mutate(N_Removal_Prod = ifelse(is.na(N_Removal_Prod),Value_Prod*Removal,N_Removal_Prod)) %>%
  ungroup()

# Calculate Seed Input at this level
N_Seed_Input = read.csv("./Tables/Nutrient_Seed_Input_5.csv")
N_Seed_Input = select(N_Seed_Input,-Crop)
N_Seed_Input = tidyr::gather(N_Seed_Input,Nutrient,Nutrient_Content,-FAO_Code)
N_Seed_Input$Nutrient[N_Seed_Input$Nutrient =="N_Input"] = 3102
N_Seed_Input$Nutrient[N_Seed_Input$Nutrient =="P_Input"] = 3103
N_Seed_Input$Nutrient[N_Seed_Input$Nutrient =="K_Input"] = 3104
N_Seed_Input$Nutrient = as.integer(N_Seed_Input$Nutrient)
Crops_Seed = filter(Crops,Item.Code%in%unique(N_Seed_Input$FAO_Code))
Crops_Seed$Nutrient[Crops_Seed$Nutrient=="N_Grain"] = 3102
Crops_Seed$Nutrient[Crops_Seed$Nutrient=="P_Grain"] = 3103
Crops_Seed$Nutrient[Crops_Seed$Nutrient=="K_Grain"] = 3104
Crops_Seed = filter(Crops_Seed,Nutrient%in%c(3102,3103,3104))
N_Seed_Input = right_join(N_Seed_Input,Crops_Seed,by=c("FAO_Code" = "Item.Code","Nutrient"="Nutrient.Code"))
N_Seed_Input = N_Seed_Input %>%
  rowwise() %>%
  mutate(Value_Seed = N_Removal_Prod*Nutrient_Content) %>%
  ungroup() %>%
  group_by(Area.Code,Nutrient,Year) %>%
  summarise(Value_Seed = sum(Value_Seed,na.rm=TRUE)) %>%
  select(Nutrient,Area.Code,Year,Value_Seed) %>%
  ungroup()
N_Seed_Input$Nutrient=as.integer(N_Seed_Input$Nutrient)
N_Seed_Input$Year = as.integer(N_Seed_Input$Year)

write.csv(filter(Crops,N_Removal_Prod>0),"C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/Removal_By_Country_Crop.csv")

Crops = Crops %>%
  group_by(Area.Code,Nutrient.Code,Year) %>%
  summarise(Value_Crops = sum(N_Removal_Prod,na.rm=TRUE)) %>%
  filter(Value_Crops>0)

# Fodder = read.csv("../Data/CommodityBalances_Crops_E_All_Data_(Normalized).csv")
# Fodder = filter(Fodder,Element=="Feed") %>%
#   group_by(Area.Code,Year) %>%
#   summarise(Value_Fodder = sum(Value,na.rm=TRUE)) 

# Join al of the data together
Crops = rename(Crops,Item.Code=Nutrient.Code)

Fert_IFA$ItemCode = as.numeric(Fert_IFA$ItemCode)
Crops$Year = as.integer(Crops$Year)
Data = full_join(Crops,Fert,by=c("Area.Code","Item.Code","Year"))
Data = left_join(Data,select(Fert_IFA,-Value),by=c("Area.Code"= "AreaCode","Item.Code" = "ItemCode","Year"))
Data = left_join(Data,Managed_Manure,by=c("Area.Code","Item.Code","Year"))
Data = left_join(Data,Manure_Pasture,by=c("Area.Code","Item.Code","Year"))
Data = left_join(Data,N_Seed_Input,by=c("Area.Code","Item.Code"="Nutrient","Year"))
Data = left_join(Data,N_Fixation,by=c("Area.Code","Item.Code"="Nutrient.Code","Year"))
Data = left_join(Data,Pasture,by=c("Area.Code","Item.Code"="Nutrient.Code","Year"))
Data = left_join(Data,N_Deposition,by=c("Area.Code","Item.Code"="NutrientCode","Year"))
# Volatilisation and leaching no longer available in bulk
# Use Volatilisation and Leaching from previous dissemination, carried forward
source("./R/Volatilisation_Leaching.R")
Data = left_join(Data,Volatilisation_Leaching,by=c("Area.Code","Item.Code","Year"))
Data = left_join(Data,Seed,by=c("Area.Code","Item.Code" = "Nutrient.Code","Year"))


Data = left_join(Data,select(Agr_Area,
                             Area.Code,
                             Year,
                             Agr_Land,
                             CropLand),
                 by=c("Area.Code","Year"))
# Data = left_join(Data,Fodder,by=c("Area.Code","Year"))
# .99 is to take into account atmospheric N deposition

Data = Data %>%
  rowwise() %>%
  mutate(N_BALANCE_1  = (sum(Value_Fert,
                           Value_Managed_Manure,
                           BNF,
                           Value_Seed,
                           -1*Value_Crops,
                           Value_Deposition,
                           na.rm=TRUE)/CropLand)) %>%
  mutate(N_BALANCE_2  = (sum(Value_Fert,
                             Value_Managed_Manure,
                             Value_Manure_Pasture,
                             BNF,
                             Value_Seed,
                             -1*Value_Crops,
                             -1*Value_Pasture,
                             Value_Deposition*(Agr_Land/CropLand),
                             na.rm=TRUE)/Agr_Land)) %>%
  mutate(N_BALANCE_3  = (sum(Value_Fert,
                           Value_Managed_Manure,
                           Value_Manure_Pasture,
                           BNF,
                           Value_Seed,
                           -1*Value_Crops,
                           -1*Value_Pasture,
                           -1*Value_Volatilisation_Cropland,
                           -1*Value_Leaching_Cropland,
                           Value_Deposition*(Agr_Land/CropLand),
                           na.rm=TRUE)/Agr_Land)) %>%
  mutate(N_BALANCE_4  = (sum(Value_Fert,
                           Value_Managed_Manure,
                           Value_Manure_Pasture,
                           BNF,
                           Value_Seed,
                           -1*Value_Crops,
                           -1*Value_Pasture,
                           -1*Value_Volatilisation_Agr_Area,
                           -1*Value_Leaching_Agr_Area,
                           Value_Deposition*(Agr_Land/CropLand),
                           na.rm=TRUE)/Agr_Land))

write.csv(Data,"./Output/Data_Output_16Oct2026.csv")