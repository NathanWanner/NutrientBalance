# Author: Nathan Wanner
# Get leaching and voltalisation for ESB domain from manure and synthetic fertilizers

# Here is 2025 version read from internal FAOSTAT
Manure = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))
Manure_Soils = filter(Manure,Element.Code%in%c(723811,723812),Source.Code==3050,Item.Code..CPC.=="'F1755")
# write.csv(Manure_Soils,"C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Soils_V_V.csv")
# Manure_Soils = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Soils_V_V.csv")
# Manure_Soils = dplyr::select(Manure_Soils,-X)

# Here is 2025 version read from internal FAOSTAT
Manure = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))
Manure_Pasture = filter(Manure,Element.Code%in%c(723801,723802),Source.Code==3050,Item.Code..CPC.=="'F1755")
# write.csv(Manure_Pasture,"C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Pasture_V_V.csv")
# Manure_Pasture = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Pasture_V_V.csv")
# Manure_Pasture = dplyr::select(Manure_Pasture,-X)

# Here is 2025 version read from internal FAOSTAT
Synthetic_Fert = read.csv(gzfile("./Data/Emissions_crops_E_All_Data_(Normalized).gz"))
Synthetic_Fert = filter(Synthetic_Fert,Element.Code%in%c(516201,516202),Source.Code==3050)
# write.csv(Synthetic_Fert,"C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Synthetic_Fert.csv")
# Synthetic_Fert = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Synthetic_Fert.csv")
# Synthetic_Fert = dplyr::select(Synthetic_Fert,-X)

Manure_Soils_Leaching = Manure_Soils %>%
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==723812,
         Area.Code<400,
         Year<=2023) %>%
  select(Area.Code,ElementCode,Year,Value)

Manure_Soils_Volatilisation = Manure_Soils %>% 
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==723811,
         Area.Code<400,
         Year<=2023) %>%
  select(Area.Code,ElementCode,Year,Value)

Manure_Pasture_Leaching = Manure_Pasture %>%
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==723802,
         Area.Code<400,
         Year<=2023) %>%
  select(Area.Code,ElementCode,Year,Value)

Manure_Pasture_Volatilisation = Manure_Pasture %>%
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==723801,
         Area.Code<400,
         Year<=2023) %>%
  select(Area.Code,ElementCode,Year,Value)

Synthetic_Fert_Leaching = Synthetic_Fert %>%
  rename(ElementCode = Element.Code) %>%
  dplyr::filter(ElementCode==516202,
         Area.Code<400,
         Year<=2023) %>%
  dplyr::select(Area.Code,ElementCode,Year,Value)

Synthetic_Fert_Volatilisation = Synthetic_Fert %>%
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==516201,
         Area.Code<400,
         Year<=2023) %>%
  select(Area.Code,ElementCode,Year,Value)

Leaching_Cropland = rbind(Manure_Soils_Leaching,Synthetic_Fert_Leaching)
Volatilisation_Cropland = rbind(Manure_Soils_Volatilisation,Synthetic_Fert_Volatilisation)

Leaching_Cropland = Leaching_Cropland %>%
  dplyr::group_by(Area.Code,Year) %>%
  dplyr::summarise(Value = sum(Value,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Item = "Nitrogen",
         Item.Code=3102)

Volatilisation_Cropland = Volatilisation_Cropland %>%
  dplyr::group_by(Area.Code,Year) %>%
  dplyr::summarise(Value = sum(Value,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Item = "Nitrogen",
         Item.Code=3102)

Leaching_Pasture = dplyr::select(Manure_Pasture_Leaching,Area.Code,Year,Value)
Leaching_Pasture$Item = "Nitrogen"
Leaching_Pasture$Item.Code = 3102
Volatilisation_Pasture = dplyr::select(Manure_Pasture_Volatilisation,Area.Code,Year,Value)
Volatilisation_Pasture$Item = "Nitrogen"
Volatilisation_Pasture$Item.Code = 3102

# We now have Leaching_Cropland, Leaching_Pasture, Volatilisation_Cropland, Volatilisation_Pasture
# Harmonize the datasets

StartYear = 1961
EndYear = 2023

Leaching_Cropland = Data_Update(StartYear,EndYear,Leaching_Cropland)
Leaching_Cropland = dplyr::rename(Leaching_Cropland,Value_Leaching_Cropland=Value)

Leaching_Pasture = Data_Update(StartYear,EndYear,Leaching_Pasture)
Leaching_Pasture = dplyr::rename(Leaching_Pasture,Value_Leaching_Pasture=Value)

Volatilisation_Cropland = Data_Update(StartYear,EndYear,Volatilisation_Cropland)
Volatilisation_Cropland = dplyr::rename(Volatilisation_Cropland,Value_Volatilisation_Cropland=Value)

Volatilisation_Pasture = Data_Update(StartYear,EndYear,Volatilisation_Pasture)
Volatilisation_Pasture = dplyr::rename(Volatilisation_Pasture,Value_Volatilisation_Pasture=Value)

Leaching = full_join(Leaching_Cropland,Leaching_Pasture)
Volatilisation = full_join(Volatilisation_Cropland,Volatilisation_Pasture)

Volatilisation_Leaching = full_join(Volatilisation,Leaching)
Volatilisation_Leaching = arrange(Volatilisation_Leaching,Area.Code,Year)
Volatilisation_Leaching = Volatilisation_Leaching %>%
  rowwise() %>%
  mutate(Value_Volatilisation_Agr_Area = sum(Value_Volatilisation_Cropland,Value_Volatilisation_Pasture,na.rm=TRUE),
         Value_Leaching_Agr_Area = sum(Value_Leaching_Cropland,Value_Leaching_Pasture,na.rm=TRUE)) %>%
  ungroup()

# max(filter(Volatilisation_Leaching,Area.Code==228)$Year)
# min(filter(Volatilisation_Leaching,Area.Code==228)$Year)

# # Create China Aggregates
# Volatilisation_Leaching_China = Volatilisation_Leaching %>%
#   dplyr::filter(Area.Code%in%c(41,
#                                96,
#                                128,
#                                214)) %>%
#   dplyr::group_by(Item,Item.Code,Year) %>%
#   dplyr::summarize(Value_Volatilisation_Cropland = sum(Value_Volatilisation_Cropland,na.rm=TRUE),
#                    Value_Volatilisation_Pasture = sum(Value_Volatilisation_Pasture,na.rm=TRUE),
#                    Value_Leaching_Cropland = sum(Value_Leaching_Cropland,na.rm=TRUE),
#                    Value_Leaching_Pasture = sum(Value_Leaching_Pasture,na.rm=TRUE),
#                    Value_Volatilisation_Agr_Area = sum(Value_Volatilisation_Agr_Area,na.rm=TRUE),
#                    Value_Leaching_Agr_Area = sum(Value_Leaching_Agr_Area,na.rm=TRUE)) %>%
#   dplyr::ungroup() %>%
#   mutate(Area.Code=351)
# 
# Volatilisation_Leaching = rbind(Volatilisation_Leaching,Volatilisation_Leaching_China)




