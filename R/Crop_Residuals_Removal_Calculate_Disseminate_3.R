# Updated comments Jan 26, 2026
library(dplyr)
library(tidyr)
library(zoo)
source("./Functions/Data_Aggregate.R")

library(dplyr)
library(tidyr)

source("./Functions/Data_Aggregate_Biodiversity_2025.R")
Item_Group = read.csv("./Tables/Item_Group_Updated.csv")
FAO_Country_Group_M49 = read.csv("./Tables/FAO_Country_Group_M49_No_China.csv")
m49 = read.csv("./Tables/m49.csv")
areas_merge = read.csv("./Tables/areas_merge_modified.csv")
StartYear=1961
EndYear=2024

source("./Functions/Data_Aggregate.R")
FAO_Country_Region = read.csv("./Tables/FAO_Country_Region.csv")
FAO_Country_Region = select(FAO_Country_Region,
                            Country.Code,
                            Start.Year,
                            End.Year)

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
Agr_Area = select(Agr_Area,-Item.Code)
Agr_Area = spread(Agr_Area,key=Item,Value)
Agr_Area = rename(Agr_Area,Agr_Land=`Agricultural land`)
Agr_Area = rename(Agr_Area,CropLand=Cropland)
Agr_Area$Agr_Land = Agr_Area$Agr_Land*1000
Agr_Area$CropLand = Agr_Area$CropLand*1000

Crop_Residues_Removal = read.csv("./Data/Crop_residue_by_country.csv")
Crop_Residues_Coefficients = dplyr::filter(Crop_Residues_Removal,Element%in%c("N removed with crop residue",
                                                                            "P removed with crop residue",
                                                                            "K removed with crop residue"))
Crop_Residues_Coefficients$Nutrient.Code[Crop_Residues_Coefficients$Element == "N removed with crop residue"] = 3102
Crop_Residues_Coefficients$Nutrient.Code[Crop_Residues_Coefficients$Element == "P removed with crop residue"] = 3103
Crop_Residues_Coefficients$Nutrient.Code[Crop_Residues_Coefficients$Element == "K removed with crop residue"] = 3104
Crop_Residues_Coefficients = dplyr::rename(Crop_Residues_Coefficients,Residual_Removal_Per_Ha=Value)

# 2026 update - no new data for crope residues removal - carry forward most recent rate
Crop_Residues_Coefficients = Crop_Residues_Coefficients %>%
  group_by(Area.Code,Nutrient.Code) %>% 
  complete(Year = min(Year):2024) %>%
  tidyr::fill(Residual_Removal_Per_Ha ,.direction=c("down")) %>%
  ungroup()

Crop_Residues_Removal = inner_join(Agr_Area,Crop_Residues_Coefficients,by=c("Area.Code","Year"))

Crop_Residues_Removal =  Crop_Residues_Removal %>%
  dplyr::rowwise() %>%
  # dplyr::mutate(Residual_Removal_Total_Agr_Land = Residual_Removal_Per_Ha*Agr_Land) %>%
  dplyr::mutate(Residual_Removal_Total_Cropland = Residual_Removal_Per_Ha*CropLand) %>%
  ungroup()

Crop_Residues_Removal_China =  Crop_Residues_Removal %>%
  dplyr::filter(Area.Code%in%c(41,96,128,214)) %>%
  # dplyr::mutate(Residual_Removal_Total_Agr_Land = Residual_Removal_Per_Ha*Agr_Land) %>%
  dplyr::group_by(Nutrient.Code,Year) %>%
  dplyr::summarise(Residual_Removal_Total_Cropland = sum(Residual_Removal_Total_Cropland)) %>%
  mutate(Area.Code = 351) %>%
  ungroup()

Crop_Residues_Removal = dplyr:: select(Crop_Residues_Removal,Area.Code,Year,Nutrient.Code,Residual_Removal_Total_Cropland)
Crop_Residues_Removal = rbind(Crop_Residues_Removal,Crop_Residues_Removal_China)
Crop_Residues_Removal = gather(Crop_Residues_Removal,Element,Value,Residual_Removal_Total_Cropland)

StartYear=1961
EndYear=2024
Crop_Residues_Removal = dplyr::rename(Crop_Residues_Removal,Item.Code=Nutrient.Code)
Crop_Residues_Removal = select(Crop_Residues_Removal,-Element)
Crop_Residues_Removal$Item[Crop_Residues_Removal$Item.Code==3102]="Nitrogen"
Crop_Residues_Removal$Item[Crop_Residues_Removal$Item.Code==3103]="Phosphorous"
Crop_Residues_Removal$Item[Crop_Residues_Removal$Item.Code==3104]="Potassium"
Crop_Residues_Removal = select(Crop_Residues_Removal,Area.Code,Item.Code,Year,Value,Item)
Crop_Residues_Removal = arrange(Crop_Residues_Removal,Area.Code,Item.Code,Year,Value,Item)

Crop_Residues_Removal = Data_Update(StartYear,EndYear,Crop_Residues_Removal)

save(Crop_Residues_Removal,file="./Output/Crop_Residues_Removal_Disseminate_China_3_Update.RData")









