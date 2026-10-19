# Updated comments Jan 26, 2026
library(dplyr)
library(tidyr)

Data = read.csv("./Output/Data_Output_16Oct2026.csv")
load(file="./Output/Crop_Residues_Removal_Disseminate_China_3_Update_2.RData")
Data = select(Data,-X)
Crop_Residues_Removal = dplyr::rename(Crop_Residues_Removal,Value_Crop_Residues_Removal = Value)
Data = left_join(Data,Crop_Residues_Removal)
Data = filter(Data,Area.Code<400)
m49 = read.csv("./Tables/m49.csv")
FAO_Country_Group_M49 = read.csv("./Tables/FAO_Country_Group_M49_No_China.csv")
Groups = read.csv("./Tables/Country_Regions.csv")
Groups_Use = read.csv("./Tables/Groups.csv")
# use updates for Netherlands and Montenegro cropland
Data$CropLand[Data$Area.Code==150&Data$Year<2004] = Data$CropLand[Data$Area.Code==150&Data$Year==2004]
Data$CropLand[Data$Area.Code==273&Data$Year>=2006&Data$Year<=2014] = Data$CropLand[Data$Area.Code==273&Data$Year==2014]
# Nutrient updates for Ireland and New Zealand
Data$Value_Fert_IFA = as.numeric(Data$Value_Fert_IFA)
Data = Data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Value_Fert = mean(c(Value_Fert,Value_Fert_IFA),na.rm=TRUE)) %>%
  dplyr::ungroup() 
Data$Value_Fert[is.nan(Data$Value_Fert)] = NA

# Use Value_Seed_2, but if there is no data, use Value_Seed
# When Value_Seed_2 is NA for whole time series use Value_Seed_1
# When Value_Seed_2 is zero for the whole time series use Value_Seed_1

Data_Check = Data %>%
  dplyr::group_by(Area.Code,Item.Code) %>%
  mutate(Count = length(Value_Seed_2[Value_Seed_2>0&!is.na(Value_Seed_2)])) %>%
  filter(Count>0) %>%
  ungroup()

Data$Value_Seed[Data$Area.Code%in%unique(Data_Check$Area.Code)] = Data$Value_Seed_2[Data$Area.Code%in%unique(Data_Check$Area.Code)]

Cropland_Fraction = read.csv("C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2024/Data/Fr_crop_estimates_simplified_Reformatted_N_P_K_Update_3.csv")
# Cropland_Fraction = select(Cropland_Fraction,-Country)
# Cropland_Fraction = gather(Cropland_Fraction,Year,Cropland_Fraction,3:ncol(Cropland_Fraction))
# Cropland_Fraction$Year = as.integer(substr(Cropland_Fraction$Year,2,length(Cropland_Fraction$Year)))
# Cropland_Fraction$ItemCode = as.numeric(Cropland_Fraction$ItemCode)

# Data = left_join(Data,Cropland_Fraction,by=c("Area.Code" = "AreaCode","Year","Item.Code" = "ItemCode"))
Data = left_join(Data,Cropland_Fraction,by=c("Area.Code" = "AreaCode","Item.Code" = "ItemCode"))

Data$Fraction[is.na(Data$Fraction)] = 1
Data$Value_Fert_Cropland = Data$Value_Fert*Data$Fraction
Data$Value_Manure_Cropland = Data$Value_Managed_Manure*Data$Fraction
Data$Value_Volatilisation_Cropland = Data$Value_Volatilisation_Cropland*Data$Fraction
Data$Value_Leaching_Cropland = Data$Value_Leaching_Cropland*Data$Fraction

Data = Data %>%
  rowwise() %>%
  mutate(Value_Manure_Agr_Land = sum(Value_Manure_Cropland,Value_Manure_Pasture,na.rm=TRUE),
         Value_Crops_Agr_Land = sum(Value_Crops,Value_Pasture,na.rm=TRUE),
         Value_Deposition_Agr_Land = Value_Deposition*(Agr_Land/CropLand))

# Data = filter(Data,Item.Code==3102)
Data = select(Data,
              Area.Code,
              Item.Code,
              Year,
              BNF,
              Value_Crops,
              Value_Seed,
              Value_Deposition,
              Value_Deposition_Agr_Land,
              Value_Fert,
              Value_Fert_IFA,
              Value_Fert_Cropland,
              Value_Manure_Cropland,
              Value_Manure_Agr_Land,
              Value_Crops_Agr_Land,
              CropLand,
              Agr_Land,
              Value_Leaching_Cropland,
              Value_Volatilisation_Cropland,
              Value_Leaching_Agr_Area,
              Value_Volatilisation_Agr_Area,
              Value_Crop_Residues_Removal)

Data = Data %>%
  rowwise() %>%
  mutate(Inputs_1 = sum(BNF,Value_Deposition,Value_Fert_Cropland,Value_Manure_Cropland,Value_Seed,na.rm=TRUE),
         Outputs_1 = sum(Value_Crops,na.rm=TRUE),
         Inputs_2 = sum(BNF,Value_Deposition_Agr_Land,Value_Fert,Value_Manure_Agr_Land,Value_Seed,na.rm=TRUE),
         Outputs_2 = sum(Value_Crops_Agr_Land,na.rm=TRUE),
         SNB_1 = Inputs_1-Outputs_1,
         SNB_2 = Inputs_2-Outputs_2)

Data_Cropland = select(Data,
                       Area.Code,
                       Item.Code,
                       Year,
                       BNF,
                       Value_Crops,
                       Value_Seed,
                       Value_Deposition,
                       Value_Fert_Cropland,
                       Value_Manure_Cropland,
                       Value_Volatilisation_Cropland,
                       Value_Leaching_Cropland,
                       CropLand,
                       Inputs_1,
                       Outputs_1,
                       SNB_1,
                       Value_Crop_Residues_Removal)

Data_Cropland = dplyr::rename(Data_Cropland,
                       Value_Volatilisation = Value_Volatilisation_Cropland,
                       Value_Leaching = Value_Leaching_Cropland,
                       Value_Fert = Value_Fert_Cropland,
                       Manure = Value_Manure_Cropland,
                       Inputs = Inputs_1,
                       Outputs = Outputs_1,
                       SNB = SNB_1,
                       Area = CropLand)
Data_Cropland$Boundary = "Cropland"
Data_Cropland = as.data.frame(Data_Cropland)

Fert_IFA = read.csv("./Data/ifa_countries_2026.csv")
Fert_IFA = filter(Fert_IFA,!is.na(Year))

# 2 countries with crop removal and no fertilizer data
List_No_Fert_Yes_Production_1 = dplyr::filter(Data_Cropland,is.na(Value_Fert),Value_Crops>0,!is.na(Value_Crops))
length(unique(List_No_Fert_Yes_Production_1$Area.Code))
# Same for just nitrogen
length(unique(filter(List_No_Fert_Yes_Production_1,Item.Code==3102)$Area.Code))
# None of these countries are in the IFA data
unique(unique(List_No_Fert_Yes_Production_1$Area.Code)%in%unique(Fert_IFA$AreaCode))

# 19 country with crop removal and fertilizer data = 0
List_No_Fert_Yes_Production_2 = dplyr::filter(Data_Cropland,Value_Fert<.001,!is.na(Value_Fert),Value_Crops>.001,!is.na(Value_Crops))
length(unique(List_No_Fert_Yes_Production_2$Area.Code))
# 9 countries with crop removal and fertilizer nitrogen data = 0
length(unique(filter(List_No_Fert_Yes_Production_2,Item.Code==3102)$Area.Code))

List_No_Fert_Yes_Production = unique(rbind(List_No_Fert_Yes_Production_1,List_No_Fert_Yes_Production_2))
# 21 total countries with production but fertilizers missing or equal to zero
length(unique(List_No_Fert_Yes_Production$Area.Code))
# Some are in IFA, but IFA has already been averaged at this point
unique(unique(List_No_Fert_Yes_Production$Area.Code)%in%unique(Fert_IFA$AreaCode))

# 0 countries with crop removal = 0 and fertilizer data
List_Yes_Fert_No_Production_1 = dplyr::filter(Data_Cropland,!is.na(Value_Fert),Value_Fert>.001,Value_Crops<.001)
length(unique(List_Yes_Fert_No_Production_1$Area.Code))
# 16 country with positive fertilizer data and no crop removal
List_Yes_Fert_No_Production_2 = dplyr::filter(Data_Cropland,!is.na(Value_Fert),Value_Fert>.001,is.na(Value_Crops))
length(unique(List_Yes_Fert_No_Production_2$Area.Code))

# Here are the countries not disseminated in Fertilizers domain
# List_Masked_Countries_Fert = c(5,22,35,36,39,128,96,45,72,61,209,64,69,85,86,87,175,93,83,120,122,123,135,136,145,140,142,163,151,160,164,180,299,177,182,191,192,193,197,200,25,201,277,176,227,240,155)
# None of these countries are in the IFA data
# unique(List_Masked_Countries_Fert%in%unique(Fert_IFA$AreaCode))
List_Production = filter(Data,!is.na(Value_Crops))
# All of these countries have production data
unique(List_No_Fert_Yes_Production$Area.Code%in%unique(List_Production$Area.Code))

# These are the final countries to mask
# These are the 47 countries from fertilizers plus Marshall Islands (Area.Code 127), Tokelau (Area.Code 218), and Bermuda (17)
# Marshall Islands and Tokelau have crop removal data and are missing from fertilizers and Bermuda has fertilizers data and is missing from production
# Mask_Countries = unique(c(List_No_Fert_Yes_Production_1$Area.Code,List_Yes_Fert_No_Production_2$Area.Code,List_Masked_Countries_Fert))
Mask_Countries = unique(c(List_No_Fert_Yes_Production_1$Area.Code,List_Yes_Fert_No_Production_2$Area.Code))
# These is the list to be shared for IFA discussion
write.csv(unique(rbind(List_No_Fert_Yes_Production_1,List_No_Fert_Yes_Production_2)),"./Output/Records_IFA.csv")

Data_AgrLand = select(Data,
                      Area.Code,
                      Item.Code,
                      Year,
                      BNF,
                      Value_Crops_Agr_Land,
                      Value_Seed,
                      Value_Deposition_Agr_Land,
                      Value_Fert,
                      Value_Manure_Agr_Land,
                      Value_Volatilisation_Agr_Area,
                      Value_Leaching_Agr_Area,
                      Agr_Land,
                      Inputs_2,
                      Outputs_2,
                      SNB_2,
                      Value_Crop_Residues_Removal)

Data_AgrLand = dplyr::rename(Data_AgrLand,
                      Value_Crops = Value_Crops_Agr_Land,
                      Value_Deposition = Value_Deposition_Agr_Land,
                      Value_Volatilisation = Value_Volatilisation_Agr_Area,
                      Value_Leaching = Value_Leaching_Agr_Area,
                      Manure = Value_Manure_Agr_Land,
                      Inputs = Inputs_2,
                      Outputs = Outputs_2,
                      SNB = SNB_2,
                      Area = Agr_Land)
Data_AgrLand$Boundary = "AgrLand"
Data = as.data.frame(rbind(Data_Cropland,Data_AgrLand))

# Here is where I get duplicates
# Data = filter(gather(Data,Item,Value_Total,4:10,12:(ncol(Data)-1)),!is.na(Area),!is.na(Value_Total))
Data = gather(Data,Item,Value_Total,4:11,13:(ncol(Data)-1))
# I put this as temporary fix
Data = unique(Data)

# Data = filter(Data,!(Item=="NUE"))
# China aggregate available for alll combinations
Data = Data %>%
  rowwise() %>%
  mutate(Value_Area = Value_Total/Area) %>%
  ungroup()
Data = left_join(Data,select(m49,M49.Code,Country.Code),by=c("Area.Code" = "Country.Code"))
Data = select(Data,-Area.Code)
Data_NUE_1 = select(Data,-Area,-Value_Area)
Data_NUE_1 = dplyr::filter(Data_NUE_1,Item%in%c("Inputs"))
Data_NUE_1 = dplyr::rename(Data_NUE_1,Inputs=Value_Total)
Data_NUE_1 = dplyr::select(Data_NUE_1,-Item)
Data_NUE_2 = select(Data,-Area,-Value_Area)
Data_NUE_2 = dplyr::filter(Data_NUE_2,Item%in%c("Outputs"))
Data_NUE_2 = dplyr:::rename(Data_NUE_2,Outputs=Value_Total)
Data_NUE_2 = dplyr::select(Data_NUE_2,-Item)
Data_NUE = left_join(Data_NUE_1,Data_NUE_2,by = c("M49.Code", "Item.Code", "Year", "Boundary"))

Data_FDI_1 = select(Data,-Area,-Value_Area)
Data_FDI_1 = dplyr::filter(Data_FDI_1,Item%in%c("Value_Fert","Manure"))
Data_FDI_1 = Data_FDI_1 %>%
  dplyr::group_by(M49.Code,Item.Code,Boundary,Year) %>%
  dplyr::summarise(Value_Fert = sum(Value_Total,na.rm=TRUE)) %>%
  ungroup()
Data_FDI_2 = select(Data,-Area,-Value_Area)
Data_FDI_2 = dplyr::filter(Data_FDI_2,Item%in%c("Inputs"))
Data_FDI_2 = dplyr:::rename(Data_FDI_2,Inputs=Value_Total)
Data_FDI_2 = dplyr::select(Data_FDI_2,-Item)
Data_FDI = left_join(Data_FDI_1,Data_FDI_2,by = c("M49.Code", "Item.Code", "Year", "Boundary"))


Data_SNMI = filter(Data,Item=="Outputs",!is.na(Value_Area),Item.Code==3102,Boundary=="Cropland")
Data_SNMI$NYield[Data_SNMI$Value_Area<=90] = Data_SNMI$Value_Area[Data_SNMI$Value_Area<=90]/90
Data_SNMI$NYield[Data_SNMI$Value_Area>90] = 1

# Data_NUE = tidyr::spread(Data_NUE,Item,Value,Boundary)
Data_NUE = Data_NUE %>%
  rowwise() %>%
  mutate(Value = (Outputs/Inputs)*100) %>%
  filter(!is.na(Value),!is.nan(Value),!is.infinite(Value)) %>%
  ungroup() %>%
  mutate(Element = "Percentage",
         Flag = "E",
         Item = "NUE")

Data_FDI = Data_FDI %>%
  rowwise() %>%
  mutate(Value = (Value_Fert/Inputs)*100) %>%
  filter(!is.na(Value),!is.nan(Value),!is.infinite(Value)) %>%
  ungroup() %>%
  mutate(Element = "Indicator",
         Flag = "E",
         Item = "FDI")

Data_NUE_SNMI = filter(Data_NUE,Item.Code==3102,Boundary=="Cropland")
Data_NUE_SNMI = select(Data_NUE_SNMI,-Item)
Data_NUE_SNMI$NUE_SNMI[Data_NUE_SNMI$Value<=100] = Data_NUE_SNMI$Value[Data_NUE_SNMI$Value<=100]/100
Data_NUE_SNMI$NUE_SNMI[Data_NUE_SNMI$Value>100 & Data_NUE_SNMI$Value<=200] = (200-Data_NUE_SNMI$Value[Data_NUE_SNMI$Value>100 & Data_NUE_SNMI$Value<=200])/100
Data_NUE_SNMI$NUE_SNMI[Data_NUE_SNMI$Value>200] = 0

Data_NUE_SNMI = select(Data_NUE_SNMI,-Value)
Data_NUE_SNMI = left_join(Data_NUE_SNMI,Data_SNMI)
Data_NUE_SNMI = Data_NUE_SNMI %>%
  rowwise() %>%
  mutate(Value = sqrt((1-NYield)^2)+(1-NUE_SNMI)^2) %>%
  filter(!is.na(Value),!is.nan(Value),!is.infinite(Value)) %>%
  ungroup() %>%
  mutate(Element = "Index",
         Flag = "E",
         Item = "SNMI")

# Data = filter(Data,!is.na(Value))
# I am here
Data_Aggregates = Data %>%
  dplyr::right_join(FAO_Country_Group_M49,Data,by=c("M49.Code")) %>%
  dplyr::group_by(Country.Group.Code.M49,Country.Group,Item.Code,Item,Boundary,Year) %>%
  dplyr::summarise(Value_Area = sum(Value_Total,na.rm=TRUE)/sum(Area,na.rm=TRUE),
                   Value_Total = sum(Value_Total,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  filter(!is.na(Boundary))
Data_Aggregates$Flag = "E"

Data_Aggregates_SNMI = filter(Data_Aggregates,Item=="Outputs",Item.Code==3102,Boundary=="Cropland")
Data_Aggregates_SNMI = select(Data_Aggregates_SNMI,-Item)  
Data_Aggregates_SNMI$NYield[Data_Aggregates_SNMI$Value_Area<=90&!is.na(Data_Aggregates_SNMI$Value_Area)] = Data_Aggregates_SNMI$Value_Area[Data_Aggregates_SNMI$Value_Area<=90&!is.na(Data_Aggregates_SNMI$Value_Area)]/90
Data_Aggregates_SNMI$NYield[Data_Aggregates_SNMI$Value_Area>90&!is.na(Data_Aggregates_SNMI$Value_Area)] = 1  
Data_Aggregates_SNMI$Flag = "E"

Data_Aggregates_NUE = dplyr::filter(Data_Aggregates,Item%in%c("Inputs","Outputs"))
Data_Aggregates_NUE = select(Data_Aggregates_NUE,-Flag,-Value_Area)
Data_Aggregates_NUE = spread(Data_Aggregates_NUE,Item,Value_Total)
Data_Aggregates_NUE = Data_Aggregates_NUE %>%
  rowwise() %>%
  mutate(Value = (`Outputs`/`Inputs`)*100) %>%
  filter(!is.na(Value),!is.nan(Value),!is.infinite(Value)) %>%
  ungroup() %>%
  mutate(Element = "Percentage",
         Flag = "E",
         Item = "NUE")

Data_Aggregates_FDI = dplyr::filter(Data_Aggregates,Item%in%c("Inputs","Value_Fert","Manure"))
Data_Aggregates_FDI = select(Data_Aggregates_FDI,-Flag,-Value_Area)
Data_Aggregates_FDI = spread(Data_Aggregates_FDI,Item,Value_Total)
Data_Aggregates_FDI = Data_Aggregates_FDI %>%
  rowwise() %>%
  mutate(Value = ((`Value_Fert`+`Manure`)/`Inputs`)*100) %>%
  filter(!is.na(Value),!is.nan(Value),!is.infinite(Value)) %>%
  ungroup() %>%
  mutate(Element = "Indicator",
         Flag = "E",
         Item = "FDI")

Data_Aggregates_NUE_SNMI = filter(Data_Aggregates_NUE,Item.Code==3102,Boundary=="Cropland")
Data_Aggregates_NUE_SNMI = select(Data_Aggregates_NUE_SNMI,-Item)
Data_Aggregates_NUE_SNMI$NUE_SNMI[Data_Aggregates_NUE_SNMI$Value<=100] = Data_Aggregates_NUE_SNMI$Value[Data_Aggregates_NUE_SNMI$Value<=100]/100
Data_Aggregates_NUE_SNMI$NUE_SNMI[Data_Aggregates_NUE_SNMI$Value>100 & Data_Aggregates_NUE_SNMI$Value<=200] = (200-Data_Aggregates_NUE_SNMI$Value[Data_Aggregates_NUE_SNMI$Value>100 & Data_Aggregates_NUE_SNMI$Value<=200])/100
Data_Aggregates_NUE_SNMI$NUE_SNMI[Data_Aggregates_NUE_SNMI$Value>200] = 0

Data_Aggregates_NUE_SNMI = select(Data_Aggregates_NUE_SNMI,-Value)
Data_Aggregates_NUE_SNMI = left_join(select(Data_Aggregates_NUE_SNMI,-Flag),select(Data_Aggregates_SNMI,-Flag),by=c("Country.Group.Code.M49", "Country.Group", "Item.Code", "Boundary", "Year"))
Data_Aggregates_NUE_SNMI = Data_Aggregates_NUE_SNMI %>%
  rowwise() %>%
  mutate(Value = sqrt((1-NYield)^2)+(1-NUE_SNMI)^2) %>%
  filter(!is.na(Value),!is.nan(Value),!is.infinite(Value)) %>%
  ungroup() %>%
  mutate(Element = "Index",
         Flag = "E",
         Item = "SNMI")
Data_Aggregates = gather(Data_Aggregates,Element,Value,7:8)
Data_Aggregates = rbind(Data_Aggregates,select(Data_Aggregates_NUE,colnames(Data_Aggregates)))
Data_Aggregates = rbind(Data_Aggregates,select(Data_Aggregates_FDI,colnames(Data_Aggregates)))
Data_Aggregates = rbind(Data_Aggregates,select(Data_Aggregates_NUE_SNMI,colnames(Data_Aggregates)))

Data = select(Data,-Area)
Data = gather(Data,Element,Value,5:6)
Data = rbind(Data,select(Data_NUE,colnames(Data)))
Data = rbind(Data,select(Data_NUE_SNMI,colnames(Data)))
Data = rbind(Data,select(Data_FDI,colnames(Data)))
Data_Aggregates = select(Data_Aggregates,-Flag,-Country.Group)
Data_Aggregates = dplyr::rename(Data_Aggregates,M49.Code=Country.Group.Code.M49)
Data = rbind(Data,Data_Aggregates)

Fert = read.csv("./Data/Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv")
Fert = filter(Fert,Element.Code==5157,Item.Code%in%c(3102,3103,3104),Area.Code<400)
Fert = select(Fert,Area.Code..M49.,Item.Code,Year,Value,Flag)
Fert = mutate(Fert,Fert_Value = Value*1000)
Fert = select(Fert,-Value)
Fert = dplyr::rename(Fert,Fert_Flag = Flag)
Fert$Area.Code..M49. = as.integer(substr(Fert$Area.Code..M49.,2,5))
Data = dplyr::left_join(Data,Fert,by=c("M49.Code"="Area.Code..M49.","Item.Code","Year"))
Data$Flag[Data$Item == "Value_Fert"&(abs(Data$Value-Data$Fert_Value)<1)&!is.na(Data$Value)&!is.na(Data$Fert_Value)&Data$Element=="Value_Total"] = 
  Data$Fert_Flag[Data$Item == "Value_Fert"&(abs(Data$Value-Data$Fert_Value)<1)&!is.na(Data$Value)&!is.na(Data$Fert_Value)&Data$Element=="Value_Total"]
Data = select(Data,-Fert_Value,-Fert_Flag)

# Data = filter(Data,Value>0)
Data$Flag[Data$Item%in%c("Manure",
                         "Inputs",
                         "Outputs",
                         "SNB")] = "E"
Data$Flag[Data$Element=="Value_Area"] = "E"
Data$Flag[Data$Item%in%c("Value_Deposition","Value_Crops","BNF","Value_Volatilisation","Value_Leaching")&(is.na(Data$Flag))] = "E"
# Data$Flag[Data$Item=="Value_Deposition"&Data$Element=="Value_Total"&Data$Year>=2018&!(Data$Flag=="")] = "E"
Data$Flag[is.na(Data$Flag)] = "E"
Data$Flag[Data$M49.Code%in%unique(FAO_Country_Group_M49$Country.Group.Code.M49)] = "E"

Data$`Domain Code` = "ESB"
Data$Domain = "Cropland Nutrient Balance"
Data$`Item Code`[Data$Item=="BNF"] = 5078
Data$Item[Data$Item=="BNF"] = "Biological Fixation"
Data$`Item Code`[Data$Item=="Value_Crops"] = 5077
Data$Item[Data$Item=="Value_Crops"] = "Crop Removal"
Data$`Item Code`[Data$Item=="Value_Deposition"] = 5076
Data$Item[Data$Item=="Value_Deposition"] = "Atmospheric Deposition"
Data$`Item Code`[Data$Item=="Value_Fert"] = 5087
Data$Item[Data$Item=="Value_Fert"] = "Mineral fertilizers"
Data$`Item Code`[Data$Item=="Manure"] = 5062
Data$Item[Data$Item=="Manure"] = "Manure applied to soils"
Data$`Item Code`[Data$Item=="Inputs"] = 5079
Data$Item[Data$Item=="Inputs"] = "Inputs (Total)"
Data$`Item Code`[Data$Item=="Outputs"] = 5080
Data$Item[Data$Item=="Outputs"] = "Outputs (Total)"
Data$`Item Code`[Data$Item=="SNB"] = 5081
Data$Item[Data$Item=="SNB"] = "Nutrient Balance (Total)"
Data$`Item Code`[Data$Item=="Value_Volatilisation"] = 5082
Data$Item[Data$Item=="Value_Volatilisation"] = "Volatilisation"
Data$`Item Code`[Data$Item=="Value_Leaching"] = 5083
Data$Item[Data$Item=="Value_Leaching"] = "Leaching"
Data$`Item Code`[Data$Item=="NUE"] = 5081
Data$Item[Data$Item=="NUE"] = "Nutrient Balance (Total)"
Data$`Item Code`[Data$Item=="SNMI"] = 5081
Data$Item[Data$Item=="SNMI"] = "Nutrient Balance (Total)"
Data$`Item Code`[Data$Item=="FDI"] = 5180
Data$Item[Data$Item=="FDI"] = "Fertilizers"
# I temporarily asigned 5084 to seeds (would need to check with FAOSTAT)
Data$`Item Code`[Data$Item=="Value_Seed"] = 5086
Data$Item[Data$Item=="Value_Seed"] = "Seed"
Data$`Item Code`[Data$Item=="Value_Crop_Residues_Removal"] = 5088
Data$Item[Data$Item=="Value_Crop_Residues_Removal"] = "Crop Residues Removal"

Data$`Element Code`[Data$Element=="Percentage"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = 7290
Data$Element[Data$Element=="Percentage"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = "Cropland nitrogen use efficiency"
Data$`Element Code`[Data$Element=="Percentage"&Data$Boundary=="Cropland"&Data$Item.Code==3103] = 7291
Data$Element[Data$Element=="Percentage"&Data$Boundary=="Cropland"&Data$Item.Code==3103] = "Cropland phosphorus use efficiency"
Data$`Element Code`[Data$Element=="Percentage"&Data$Boundary=="Cropland"&Data$Item.Code==3104] = 7292
Data$Element[Data$Element=="Percentage"&Data$Boundary=="Cropland"&Data$Item.Code==3104] = "Cropland potassium use efficiency"

Data$`Element Code`[Data$Element=="Indicator"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = 7293
Data$Element[Data$Element=="Indicator"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = "Cropland nitrogen fertilizer dependency"
Data$`Element Code`[Data$Element=="Indicator"&Data$Boundary=="Cropland"&Data$Item.Code==3103] = 7294
Data$Element[Data$Element=="Indicator"&Data$Boundary=="Cropland"&Data$Item.Code==3103] = "Cropland phosphorus fertilizer dependency"
Data$`Element Code`[Data$Element=="Indicator"&Data$Boundary=="Cropland"&Data$Item.Code==3104] = 7295
Data$Element[Data$Element=="Indicator"&Data$Boundary=="Cropland"&Data$Item.Code==3104] = "Cropland potassium fertilizer dependency"

Data$`Element Code`[Data$Element=="Index"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = 7296
Data$Element[Data$Element=="Index"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = "Sustainable Nitrogen Management Index"

Data$`Element Code`[Data$Element=="Percentage"&Data$Boundary=="AgrLand"&Data$Item.Code==3102] = 7299
Data$Element[Data$Element=="Percentage"&Data$Boundary=="AgrLand"&Data$Item.Code==3102] = "Agricultural land nitrogen use efficiency"
Data$`Element Code`[Data$Element=="Percentage"&Data$Boundary=="AgrLand"&Data$Item.Code==3103] = 7300
Data$Element[Data$Element=="Percentage"&Data$Boundary=="AgrLand"&Data$Item.Code==3103] = "Agricultural land phosphorus use efficiency"
Data$`Element Code`[Data$Element=="Percentage"&Data$Boundary=="AgrLand"&Data$Item.Code==3104] = 7301
Data$Element[Data$Element=="Percentage"&Data$Boundary=="AgrLand"&Data$Item.Code==3104] = "Agricultural land potassium use efficiency"


Data$`Element Code`[Data$Element=="Value_Total"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = 7275
Data$Element[Data$Element=="Value_Total"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = "Cropland nitrogen"
Data$`Element Code`[Data$Element=="Value_Area"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = 7276
Data$Element[Data$Element=="Value_Area"&Data$Boundary=="Cropland"&Data$Item.Code==3102] = "Cropland nitrogen per unit area"
Data$`Element Code`[Data$Element=="Value_Total"&Data$Boundary=="Cropland"&Data$Item.Code==3103] = 7280
Data$Element[Data$Element=="Value_Total"&Data$Boundary=="Cropland"&Data$Item.Code==3103] = "Cropland phosphorus"
Data$`Element Code`[Data$Element=="Value_Area"&Data$Boundary=="Cropland"&Data$Item.Code==3103] = 7281
Data$Element[Data$Element=="Value_Area"&Data$Boundary=="Cropland"&Data$Item.Code==3103] = "Cropland phosphorus per unit area"
Data$`Element Code`[Data$Element=="Value_Total"&Data$Boundary=="Cropland"&Data$Item.Code==3104] = 7282
Data$Element[Data$Element=="Value_Total"&Data$Boundary=="Cropland"&Data$Item.Code==3104] = "Cropland potassium"
Data$`Element Code`[Data$Element=="Value_Area"&Data$Boundary=="Cropland"&Data$Item.Code==3104] = 7283
Data$Element[Data$Element=="Value_Area"&Data$Boundary=="Cropland"&Data$Item.Code==3104] = "Cropland potassium per unit area"
Data$`Element Code`[Data$Element=="Value_Total"&Data$Boundary=="AgrLand"&Data$Item.Code==3102] = 7284
Data$Element[Data$Element=="Value_Total"&Data$Boundary=="AgrLand"&Data$Item.Code==3102] = "Agricultural land nitrogen"
Data$`Element Code`[Data$Element=="Value_Area"&Data$Boundary=="AgrLand"&Data$Item.Code==3102] = 7285
Data$Element[Data$Element=="Value_Area"&Data$Boundary=="AgrLand"&Data$Item.Code==3102] = "Agricultural land nitrogen per unit area"
Data$`Element Code`[Data$Element=="Value_Total"&Data$Boundary=="AgrLand"&Data$Item.Code==3103] = 7286
Data$Element[Data$Element=="Value_Total"&Data$Boundary=="AgrLand"&Data$Item.Code==3103] = "Agricultural land phosphorus"
Data$`Element Code`[Data$Element=="Value_Area"&Data$Boundary=="AgrLand"&Data$Item.Code==3103] = 7287
Data$Element[Data$Element=="Value_Area"&Data$Boundary=="AgrLand"&Data$Item.Code==3103] = "Agricultural land phosphorus per unit area"
Data$`Element Code`[Data$Element=="Value_Total"&Data$Boundary=="AgrLand"&Data$Item.Code==3104] = 7288
Data$Element[Data$Element=="Value_Total"&Data$Boundary=="AgrLand"&Data$Item.Code==3104] = "Agricultural land potassium"
Data$`Element Code`[Data$Element=="Value_Area"&Data$Boundary=="AgrLand"&Data$Item.Code==3104] = 7289
Data$Element[Data$Element=="Value_Area"&Data$Boundary=="AgrLand"&Data$Item.Code==3104] = "Agricultural land potassium per unit area"

Data$Value[Data$`Element Code`%in%c(7275,7280,7282,7284,7286,7288)] = Data$Value[Data$`Element Code`%in%c(7275,7280,7282,7284,7286,7288)]/1000
Data$Unit[Data$`Element Code`%in%c(7275,7280,7282,7284,7286,7288)] = "tonnes"
Data$Unit[Data$`Element Code`%in%c(7276,7281,7283,7285,7287,7289)] = "kg/ha"
Data$Unit[Data$`Element Code`%in%c(7290,7291,7292,7293,7294,7295,7299,7300,7301)] = "%"
Data$Unit[Data$`Element Code`%in%c(7296)] = ""
Data = left_join(Data,dplyr::select(m49,M49.Code,Country,Country.Code),by=c("M49.Code"))
Data = dplyr::rename(Data,`Area Code` = Country.Code)

Data$`Year Code` = Data$Year
Data = select(Data,`Domain Code`,`Domain`,M49.Code,`Area Code`,Country,`Element Code`,`Element`,`Item Code`,`Item`,`Year Code`,`Year`,`Unit`,`Value`,`Flag`)

# List_Mask_Countries  = unique(c(unique(rbind(List_No_Fert_Yes_Production_1,List_Yes_Fert_No_Production_2)$Area.Code),c(List_Masked_Countries_Fert)))
List_Mask_Countries  = unique(c(unique(rbind(List_No_Fert_Yes_Production_1,List_Yes_Fert_No_Production_2)$Area.Code)))

Data_FAOSTAT = filter(Data,!`Area Code`%in%List_Mask_Countries)
Data_FAOSTAT = select(Data_FAOSTAT,`Domain Code`,`Domain`,M49.Code,`Area Code`,Country,`Element Code`,`Element`,`Item Code`,`Item`,`Year Code`,`Year`,`Unit`,`Value`,`Flag`)
Data_FAOSTAT = filter(Data_FAOSTAT,!is.na(Value))
write.csv(Data_FAOSTAT, file=gzfile("./Output/ESB_2026_All_3.gz"),row.names=FALSE)
Data_FAOSTAT = filter(Data_FAOSTAT,`Element Code`%in%c(7275,7276,7280,7281,7282,7283,7290,7291,7292),`Item Code`%in%c(5078,5077,5076,5087,5062,5079,5080,5081,5082,5083,5086,5180,5088))

# Take out the countries for which we have crop removal but no fertilizer data or fertilizer data is 0 (43 countries)
# Also take out countries for which we have fertlizer data but no crop removal (1 country) 
# List_Mask_Countries  = unique(c(unique(rbind(List_No_Fert_Yes_Production_1,List_Yes_Fert_No_Production_2)$Area.Code),c(List_Masked_Countries_Fert)))
write.csv(Data_FAOSTAT,file=gzfile("./Output/ESB_Data_2026_3.gz"),row.names=FALSE)

