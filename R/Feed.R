# Updated comments Jan 26, 2026

library(dplyr)
library(tidyr)

IPCC_Regions = read.csv("./Tables/IPCCregions_areas.csv")
IPCC_Annex = filter(IPCC_Regions,IPCC_AreaCode%in%c(5871,5872))
IPCC_Annex = dplyr::rename(IPCC_Annex,AnnexCode=IPCC_AreaCode)

IPCC_Regions = filter(IPCC_Regions,!IPCC_AreaCode%in%c(5871,5872))


Feed_Old_Methodology = read.csv("./Data/Feed_Old_Methodology.csv")
Feed_Old_Methodology = dplyr::filter(Feed_Old_Methodology,Year<2010,Element=="Feed")
Feed_Old_Methodology = select(Feed_Old_Methodology,Area.Code,Item.Code,Year,Value)
Feed_New_Methodology = read.csv("./Data/Feed_New_Methodology.csv")
Feed_New_Methodology = dplyr::rename(Feed_New_Methodology,Area.Code = Area.Code..FAO.)
Feed_New_Methodology = dplyr::rename(Feed_New_Methodology,Item.Code = Item.Code..FAO.)
Feed_New_Methodology = select(Feed_New_Methodology,Area.Code,Item.Code,Year,Value)
Feed = rbind(Feed_Old_Methodology,Feed_New_Methodology)
# Feed = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Work/Domains/Agro-Environnmental/NewAEIS/NBalance/Data/Feed_3-8-2021.csv")
# Feed = dplyr::rename(Feed,Area.Code=Country.Code)
# Previous feed data was in tonnes and now data is expressed in 1000 tonnes
# Multiply by 1000000 to convert to kg

Feed = Feed %>%
  dplyr::group_by(Area.Code,Year) %>%
  dplyr::summarise(Value = sum(Value,na.rm=TRUE) * 1000000)
Feed$Item = "Feed"

# Max year for Land_Use to agriculture is 2016
# Min year for Credit to agriculture is 1961
StartYear=1961
EndYear=2023
Feed$Item.Code=1
Feed = Data_Update(StartYear,EndYear,Feed)
Feed = dplyr::rename(Feed,Feed_Total=Value)
Feed = select(Feed,-Item,-Item.Code)

Livestock = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))
Livestock = filter(Livestock,Element=="Stocks",Source=="FAO TIER 1")
Livestock = filter(Livestock,Item.Code%in%c(960,
                                            961,
                                            946,
                                            1049,
                                            1051,
                                            976,
                                            1016,
                                            1052,
                                            1053,
                                            1068,
                                            1079,
                                            1096,
                                            1107,
                                            1110,
                                            1126,
                                            1177))

StartYear=1961
EndYear=2023
Livestock = left_join(Livestock,FAO_Country_Region,by=c("Area.Code"="Country.Code"))
Livestock$Start.Year[is.na(Livestock$Start.Year)]  = StartYear
Livestock$End.Year[is.na(Livestock$End.Year)]  = EndYear

# TEMPORARY UNTIL WE GET UPDATED LIVESTOCK
# Livestock = Livestock %>%
#   dplyr::group_by(Area.Code,Item.Code,Item) %>%
#   tidyr::complete(Year = min(c(Year,Start.Year)):max(c(Year,End.Year))) %>%
#   dplyr::mutate(Value = na.approx(Value, na.rm = FALSE)) %>%
#   dplyr::arrange(Area.Code,Item.Code,Year) %>%
#   tidyr::fill(everything(),.direction="down") %>%
#   # tidyr::fill(everything(),.direction="up") %>%
#   dplyr::ungroup()

Livestock = dplyr::select(Livestock,-Start.Year,-End.Year)

Livestock = Data_Update_Biodiversity(StartYear,EndYear,Livestock)
Livestock = dplyr::rename(Livestock,Stocks=Value)
Livestock = dplyr::rename(Livestock,LivestockCode=Item.Code)
Livestock = dplyr::right_join(Livestock,IPCC_Regions,by=c("Area.Code" = "AreaCode"))
Livestock = dplyr::right_join(Livestock,IPCC_Annex,by=c("Area.Code" = "AreaCode"))
Livestock = dplyr::filter(Livestock,!is.na(Year))

DMI = read.csv("./Tables/DMI.csv")
DMI = gather(DMI,Livestock,DMI_Value,3:18)
DMI = dplyr::rename(DMI,AreaCode=Code)
DMI = DMI %>%
  dplyr::rowwise() %>%
  dplyr::mutate(LivestockCode = as.numeric(substr(Livestock,2,nchar(Livestock)))) %>%
  dplyr::select(-Livestock)

Livestock = left_join(Livestock,DMI,by=c("LivestockCode","IPCC_AreaCode" = "AreaCode"))
Livestock = dplyr::rename(Livestock,DMI_Value_Regional=DMI_Value)
Livestock = left_join(Livestock,DMI,by=c("LivestockCode","AnnexCode" = "AreaCode"))
Livestock = dplyr::rename(Livestock,DMI_Value_Annex=DMI_Value)

Livestock$DMI_Value = Livestock$DMI_Value_Regional
Livestock$DMI_Value[is.na(Livestock$DMI_Value)] = Livestock$DMI_Value_Annex[is.na(Livestock$DMI_Value)]

Livestock = Livestock %>%
  rowwise() %>%
  mutate(DMI_Livestock = 365*DMI_Value*Stocks) %>%
  ungroup %>%
  group_by(Area.Code,Year) %>%
  summarise(DMI_Total = sum(DMI_Livestock,na.rm=TRUE))

Feed = full_join(Livestock,Feed,by=c("Area.Code","Year"))
Feed$Feed_Total[is.na(Feed$Feed_Total)] = Feed$DMI_Total[is.na(Feed$Feed_Total)]
Feed$DMI_Total[is.na(Feed$DMI_Total)] = Feed$Feed_Total[is.na(Feed$DMI_Total)]
# 1/.85 to switch from dry to wet material
# 8/6 to switch from grains to moderate quaility forage
# Double check this with Francesco
Pasture = Feed %>%
  rowwise() %>%
  mutate(Pasture_Total = (max((DMI_Total - Feed_Total),0))/1000)

#Use bermuda grass for nutrient removal for pasture
Pasture$Nitrogen = Pasture$Pasture_Total*23.0
Pasture$Phosphorus = Pasture$Pasture_Total*6*.436
Pasture$Potash = Pasture$Pasture_Total*20.8*.830
Pasture = gather(select(Pasture,-DMI_Total,-Feed_Total,-Pasture_Total),Nutrient,Value_Pasture,3:5)
Pasture$Nutrient.Code=NA
Pasture$Nutrient.Code[Pasture$Nutrient=="Nitrogen"] = 3102
Pasture$Nutrient.Code[Pasture$Nutrient=="Phosphorus"] = 3103
Pasture$Nutrient.Code[Pasture$Nutrient=="Potash"] = 3104