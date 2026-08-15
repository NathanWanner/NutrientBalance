# Author: Nathan Wanner
# Get leaching and voltalisation for ESB domain from manure and synthetic fertilizers
# Updated comments Jan 26, 2026

# Here is 2026 version read from SWS
Manure_key <- DatasetKey(domain = "ghg_emissions_at_farm_gate", 
                         dataset = "ghg_emissions_from_livestock_2026_silent_update", 
                         dimensions = list(
                           geographicAreaM49 = Dimension(name = "geographicAreaM49", 
                                                         keys = c(GetCodeList("ghg_emissions_at_farm_gate", "ghg_emissions_from_livestock_2026_silent_update", "geographicAreaM49")[type == "country", code])), 
                           Element = Dimension(name = "ghg_measuredElement_disseminated", 
                                               keys = c("723811","723812")),
                           Source = Dimension(name = "ghg_source", 
                                              keys = "3050"),
                           Item = Dimension(name = "ghg_measuredItemCPC_disseminated", 
                                            keys = "F1755"),
                           timePointYears = Dimension(name = "timePointYears", 
                                                      keys = as.character(1961:2024)))
)


Manure_Soils <- GetData(key = Manure_key)
Manure_Soils = rename(Manure_Soils,
                        Element.Code=ghg_measuredElement_disseminated,
                        Source.Code=ghg_source,
                        Item.Code..CPC.=ghg_measuredItemCPC_disseminated,
                      Year=timePointYears)
# Manure_Soils$geographicAreaM49 = as.integer(Manure_Soils$geographicAreaM49)
# Manure = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))
# Manure_Soils = filter(Manure,Element.Code%in%c(723811,723812),Source.Code==3050,Item.Code..CPC.=="'F1755")
# write.csv(Manure_Soils,"C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Soils_V_V.csv")
# Manure_Soils = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Soils_V_V.csv")
# Manure_Soils = dplyr::select(Manure_Soils,-X)
Manure_Soils = left_join(Manure_Soils,select(m49,M49.Code,Country.Code),by=c("geographicAreaM49" = "M49.Code"))
Manure_Soils = rename(Manure_Soils,
                        Area.Code=Country.Code)

# Here is 2026 version read from SWS
Manure_key <- DatasetKey(domain = "ghg_emissions_at_farm_gate", 
                         dataset = "ghg_emissions_from_livestock_2026_silent_update", 
                         dimensions = list(
                           geographicAreaM49 = Dimension(name = "geographicAreaM49", 
                                                         keys = c(GetCodeList("ghg_emissions_at_farm_gate", "ghg_emissions_from_livestock_2026_silent_update", "geographicAreaM49")[type == "country", code],"159")), 
                           Element = Dimension(name = "ghg_measuredElement_disseminated", 
                                               keys = c("723801","723802")),
                           Source = Dimension(name = "ghg_source", 
                                              keys = "3050"),
                           Item = Dimension(name = "ghg_measuredItemCPC_disseminated", 
                                            keys = "F1755"),
                           timePointYears = Dimension(name = "timePointYears", 
                                                      keys = as.character(1961:2024)))
)


Manure_Pasture <- GetData(key = Manure_key)
Manure_Pasture = rename(Manure_Pasture,
                      Element.Code=ghg_measuredElement_disseminated,
                      Source.Code=ghg_source,
                      Item.Code..CPC.=ghg_measuredItemCPC_disseminated)
# Manure_Pasture$geographicAreaM49 = as.integer(Manure_Pasture$geographicAreaM49)
Manure_Pasture = left_join(Manure_Pasture,select(m49,M49.Code,Country.Code),by=c("geographicAreaM49" = "M49.Code"))
Manure_Pasture = rename(Manure_Pasture,
                      Area.Code=Country.Code,
                      Year=timePointYears)
# Manure = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))
# Manure_Pasture = filter(Manure,Element.Code%in%c(723801,723802),Source.Code==3050,Item.Code..CPC.=="'F1755")
# write.csv(Manure_Pasture,"C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Pasture_V_V.csv")
# Manure_Pasture = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Manure_Pasture_V_V.csv")
# Manure_Pasture = dplyr::select(Manure_Pasture,-X)

# Here is 2026 version read from SWS

Synthetic_Fert_key <- DatasetKey(domain = "ghg_emissions_at_farm_gate", 
                         dataset = "ghg_emissions_from_crops_2026_silent_update", 
                         dimensions = list(
                           geographicAreaM49 = Dimension(name = "geographicAreaM49", 
                                                         keys = c(GetCodeList("ghg_emissions_at_farm_gate", "ghg_emissions_from_crops_2026_silent_update", "geographicAreaM49")[type == "country", code],"159")), 
                           # Item_CPC = Dimension(name = "ghg_measuredItemCPC", 
                           #                     keys = c("3102")),
                           Element = Dimension(name = "ghg_measuredElement_disseminated", 
                                               keys = c("516201","516202")),
                           Source = Dimension(name = "ghg_source",
                                              keys = "3050"),
                           Item = Dimension(name = "ghg_measuredItemCPC", 
                                            keys = "3102"),
                           timePointYears = Dimension(name = "timePointYears", 
                                                      keys = as.character(1961:2024)))
)


Synthetic_Fert <- GetData(key = Synthetic_Fert_key)
Synthetic_Fert = rename(Synthetic_Fert,
                        Element.Code=ghg_measuredElement_disseminated,
                        Source.Code=ghg_source,
                        Item.Code..CPC.=ghg_measuredItemCPC,
                        Year=timePointYears)
# Synthetic_Fert$geographicAreaM49 = as.integer(Synthetic_Fert$geographicAreaM49)
# Synthetic_Fert = read.csv(gzfile("./Data/Emissions_crops_E_All_Data_(Normalized).gz"))
Synthetic_Fert = filter(Synthetic_Fert,Element.Code%in%c(516201,516202),Source.Code==3050)
Synthetic_Fert = left_join(Synthetic_Fert,select(m49,M49.Code,Country.Code),by=c("geographicAreaM49" = "M49.Code"))
Synthetic_Fert = rename(Synthetic_Fert,
                        Area.Code=Country.Code)
# write.csv(Synthetic_Fert,"C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Synthetic_Fert.csv")
# Synthetic_Fert = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2025/Data/Synthetic_Fert.csv")
# Synthetic_Fert = dplyr::select(Synthetic_Fert,-X)

Manure_Soils_Leaching = Manure_Soils %>%
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==723812,
         Area.Code<400,
         Year<=2024) %>%
  select(Area.Code,ElementCode,Year,Value)

Manure_Soils_Volatilisation = Manure_Soils %>% 
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==723811,
         Area.Code<400,
         Year<=2024) %>%
  select(Area.Code,ElementCode,Year,Value)

Manure_Pasture_Leaching = Manure_Pasture %>%
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==723802,
         Area.Code<400,
         Year<=2024) %>%
  select(Area.Code,ElementCode,Year,Value)

Manure_Pasture_Volatilisation = Manure_Pasture %>%
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==723801,
         Area.Code<400,
         Year<=2024) %>%
  select(Area.Code,ElementCode,Year,Value)

Synthetic_Fert_Leaching = Synthetic_Fert %>%
  rename(ElementCode = Element.Code) %>%
  dplyr::filter(ElementCode==516202,
         Area.Code<400,
         Year<=2024) %>%
  dplyr::select(Area.Code,ElementCode,Year,Value)

Synthetic_Fert_Volatilisation = Synthetic_Fert %>%
  rename(ElementCode = Element.Code) %>%
  filter(ElementCode==516201,
         Area.Code<400,
         Year<=2024) %>%
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
EndYear = 2024
Leaching_Cropland$Year = as.integer(Leaching_Cropland$Year)
Leaching_Cropland = Data_Update(StartYear,EndYear,Leaching_Cropland)
Leaching_Cropland = dplyr::rename(Leaching_Cropland,Value_Leaching_Cropland=Value)

Leaching_Pasture$Year = as.integer(Leaching_Pasture$Year)
Leaching_Pasture = Data_Update(StartYear,EndYear,Leaching_Pasture)
Leaching_Pasture = dplyr::rename(Leaching_Pasture,Value_Leaching_Pasture=Value)

Volatilisation_Cropland$Year = as.integer(Volatilisation_Cropland$Year)
Volatilisation_Cropland = Data_Update(StartYear,EndYear,Volatilisation_Cropland)
Volatilisation_Cropland = dplyr::rename(Volatilisation_Cropland,Value_Volatilisation_Cropland=Value)

Volatilisation_Pasture$Year = as.integer(Volatilisation_Pasture$Year)
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

# Create China Aggregates
Volatilisation_Leaching_China = Volatilisation_Leaching %>%
  dplyr::filter(Area.Code%in%c(41,
                               96,
                               128,
                               214)) %>%
  dplyr::group_by(Item,Item.Code,Year) %>%
  dplyr::summarize(Value_Volatilisation_Cropland = sum(Value_Volatilisation_Cropland,na.rm=TRUE),
                   Value_Volatilisation_Pasture = sum(Value_Volatilisation_Pasture,na.rm=TRUE),
                   Value_Leaching_Cropland = sum(Value_Leaching_Cropland,na.rm=TRUE),
                   Value_Leaching_Pasture = sum(Value_Leaching_Pasture,na.rm=TRUE),
                   Value_Volatilisation_Agr_Area = sum(Value_Volatilisation_Agr_Area,na.rm=TRUE),
                   Value_Leaching_Agr_Area = sum(Value_Leaching_Agr_Area,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  mutate(Area.Code=351)

Volatilisation_Leaching = rbind(Volatilisation_Leaching,Volatilisation_Leaching_China)




