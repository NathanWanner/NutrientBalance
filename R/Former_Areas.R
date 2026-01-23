library(tidyr)
library(dplyr)
library(zoo)
source("./Functions/Data_Aggregate.R")

m49 = read.csv("./Tables/m49.csv")
areas_merge = read.csv("./Tables/areas_merge_modified.csv")
Land_Use = read.csv("./Data/Inputs_LandUse_E_All_Data_(Normalized).csv")
Land_Use = filter(Land_Use,Item.Code%in%c(6620),Element.Code==5110)
Land_Use = select(Land_Use,
                  Area.Code,
                  Element.Code,
                  Item.Code,
                  Item,
                  Year,
                  Value)

# Max year for Land_Use is 2023
# Min year for Land_Use is 1961
StartYear=1961
EndYear=2023
Land_Use = Data_Update(StartYear,EndYear,Land_Use)
Land_Use = dplyr::select(Land_Use,-Item.Code)
Land_Use = dplyr::rename(Land_Use,Area=Item,CropLand=Value)

ESB_2025 = read.csv(gzfile("./Output/ESB_Data_2025_7.gz"))
ESB_2025 = select(ESB_2025,-X)
ESB_2025 = filter(ESB_2025,Element%in%c("Cropland nitrogen","Cropland phosphorus","Cropland potassium"))
Land_Use = select(Land_Use,-Area)
Data = left_join(ESB_2025,Land_Use,by=c("Area.Code","Year"))

# I verified that cropland matches - this is the only real one to one
ESB_2025_Former_Areas = Data %>%
  dplyr::filter(Area.Code%in%unique(areas_merge$AREA_CODE)) %>%
  left_join(areas_merge,by=c("Area.Code" = "AREA_CODE")) %>%
  dplyr::filter(Year<=END1) %>%
  dplyr::group_by(Domain,Domain.Code,AREA_CODE_FINAL,Element.Code,Element,Item.Code,Item,Year.Code,Year) %>%
  dplyr::summarise(Value = sum(Value),
                   Value_Area = sum(Value)/sum(CropLand)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(Area.Code=AREA_CODE_FINAL) %>%
  ungroup()

ESB_2025_Former_Areas = gather(ESB_2025_Former_Areas,Element,Value,Value,Value_Area)
ESB_2025_Former_Areas = dplyr::filter(ESB_2025_Former_Areas,!(Item%in%c("Nutrient Balance (Total)","Inputs (Total)")))
ESB_2025_Former_Areas$Flag = "E"
ESB_2025_Former_Areas$Element.Code[ESB_2025_Former_Areas$Element=="Value_Area"&ESB_2025_Former_Areas$Element.Code==7275] = 7276
ESB_2025_Former_Areas$Element.Code[ESB_2025_Former_Areas$Element=="Value_Area"&ESB_2025_Former_Areas$Element.Code==7280] = 7281
ESB_2025_Former_Areas$Element.Code[ESB_2025_Former_Areas$Element=="Value_Area"&ESB_2025_Former_Areas$Element.Code==7282] = 7283

ESB_2025_Former_Areas$Element[ESB_2025_Former_Areas$Element.Code==7275] = "Cropland nitrogen"
ESB_2025_Former_Areas$Element[ESB_2025_Former_Areas$Element.Code==7280] = "Cropland phosphorus"
ESB_2025_Former_Areas$Element[ESB_2025_Former_Areas$Element.Code==7282] = "Cropland potassium"
ESB_2025_Former_Areas$Element[ESB_2025_Former_Areas$Element.Code==7276] = "Cropland nitrogen per unit area"
ESB_2025_Former_Areas$Element[ESB_2025_Former_Areas$Element.Code==7281] = "Cropland phosphorus per unit area"
ESB_2025_Former_Areas$Element[ESB_2025_Former_Areas$Element.Code==7283] = "Cropland potassium per unit area"


# For fertilizers, and manure, take data directly from FAOSTAT

Land_Use = read.csv("./Data/Inputs_LandUse_E_All_Data_(Normalized).csv")
Land_Use = filter(Land_Use,Item.Code%in%c(6620),Element.Code==5110)
Land_Use = select(Land_Use,
                  Area.Code,
                  Element.Code,
                  Item.Code,
                  Item,
                  Year,
                  Value)

# Max year for Land_Use is 2023
# Min year for Land_Use is 1961
StartYear=1961
EndYear=2023
Land_Use = dplyr::select(Land_Use,-Item.Code)
Land_Use = dplyr::rename(Land_Use,Area=Item,CropLand=Value)

Fert = read.csv("./Data/Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv")
Fert = filter(Fert,Element.Code==5157,Item.Code%in%c(3102,3103,3104),Area.Code<400)
Fert = select(Fert,Area.Code,Item.Code,Year,Flag,Value)
Fert$Item[Fert$Item.Code==3102]="Nitrogen"
Fert$Item[Fert$Item.Code==3103]="Phosphorous"
Fert$Item[Fert$Item.Code==3104]="Potassium"
Fert = select(Fert,-Item)
# Convert P2O5 to P
Fert$Value_Fert[Fert$Item.Code==3103] = Fert$Value_Fert[Fert$Item.Code==3103]*.436
# Convert K2O to K
Fert$Value_Fert[Fert$Item.Code==3104] = Fert$Value_Fert[Fert$Item.Code==3104]*.830
Fert = left_join(Fert,Land_Use,by=c("Area.Code","Year"))
Fert$Item = "Mineral fertilizers"

# I verfiied that cropland matches - this is the only real one to one
Fert_Former_Areas = Fert %>%
  dplyr::filter(Area.Code%in%unique(areas_merge$AREA_CODE_FINAL)) %>%
  dplyr::rowwise() %>%
  mutate(Value_Area = Value/CropLand) %>%
  dplyr::ungroup() 
Fert_Former_Areas = select(Fert_Former_Areas,-CropLand)

Managed_Manure = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))
Managed_Manure = filter(Managed_Manure,Element.Code ==72381,
                        Item.Code..CPC.%in%c("'F1755"),Source.Code==3050)
Managed_Manure$Item.Code=3102
Managed_Manure$Item[Managed_Manure$Item.Code==3102] = "Nitrogen"
Managed_Manure = select(Managed_Manure,Area.Code,Item.Code,Year,Value)
Managed_Manure$Item = "Manure"
Managed_Manure = select(Managed_Manure,-Item)

Treated_Manure = read.csv(gzfile("./Data/Emissions_livestock_E_All_Data_(Normalized).gz"))
Manure_Ratios = read.csv("./Tables/Manure_Nutrient_Ratios_Updated_2.csv")
Manure_Ratios = select(Manure_Ratios,
                       Item_code,
                       N_ratio,
                       P_ratio,
                       K_ratio)
Manure_Ratios = rename(Manure_Ratios,
                       Item_Code = Item_code,
                       N_Ratio = N_ratio,
                       P_Ratio = P_ratio,
                       K_Ratio = K_ratio)
Manure_Ratios = gather(Manure_Ratios,Nutrient,Nutrient_Ratio,-Item_Code)
Manure_Ratios$Nutrient.Code[Manure_Ratios$Nutrient=="N_Ratio"] = 3102
Manure_Ratios$Nutrient.Code[Manure_Ratios$Nutrient=="P_Ratio"] = 3103
Manure_Ratios$Nutrient.Code[Manure_Ratios$Nutrient=="K_Ratio"] = 3104
Treated_Manure = filter(Treated_Manure,Element.Code==72386,
                        Item.Code%in%unique(Manure_Ratios$Item_Code),Source.Code==3050)
Treated_Manure = left_join(Treated_Manure,Manure_Ratios,by=c("Item.Code" = "Item_Code")) %>%
  mutate(Value_Manure_Pasture = Value*Nutrient_Ratio) %>%
  group_by(Area.Code,Nutrient.Code,Year) %>%
  summarise(Value_Treated_Manure = sum(Value_Manure_Pasture,na.rm=TRUE)) %>%
  ungroup()

Treated_Manure= rename(Treated_Manure,Item.Code=Nutrient.Code)
Treated_Manure$Item[Treated_Manure$Item.Code==3102] = "Nitrogen"
Treated_Manure$Item[Treated_Manure$Item.Code==3103] = "Phosphate"
Treated_Manure$Item[Treated_Manure$Item.Code==3104] = "Potash"
Treated_Manure = select(Treated_Manure,Area.Code,Item.Code,Year,Value_Treated_Manure)
Treated_Manure = rename(Treated_Manure,Value=Value_Treated_Manure)
Treated_Manure$Item = "Manure"
Treated_Manure = select(Treated_Manure,-Item)
Treated_Manure = filter(Treated_Manure,Item.Code%in%c(3103,3104))

Managed_Manure = rbind(Managed_Manure,Treated_Manure)
Managed_Manure$Value = Managed_Manure$Value/1000

Managed_Manure = left_join(Managed_Manure,Land_Use,by=c("Area.Code","Year"))

Managed_Manure_Former_Areas = Managed_Manure %>%
  dplyr::filter(Area.Code%in%unique(areas_merge$AREA_CODE_FINAL)) %>%
  dplyr::rowwise() %>%
  mutate(Value_Area = Value/CropLand) %>%
  dplyr::ungroup() 
Managed_Manure_Former_Areas = select(Managed_Manure_Former_Areas,-CropLand)
Managed_Manure_Former_Areas$Flag = "E"
Managed_Manure_Former_Areas$Item = "Manure applied to soils"
Managed_Manure_Former_Areas = select(Managed_Manure_Former_Areas,colnames(Fert_Former_Areas))

FAOSTAT_Former_Areas = rbind(Fert_Former_Areas,Managed_Manure_Former_Areas)

FAOSTAT_Former_Areas = gather(FAOSTAT_Former_Areas,Element,Value,Value,Value_Area)

FAOSTAT_Former_Areas$Element.Code[FAOSTAT_Former_Areas$Item.Code==3102&FAOSTAT_Former_Areas$Element=="Value"] = 7275
FAOSTAT_Former_Areas$Element.Code[FAOSTAT_Former_Areas$Item.Code==3103&FAOSTAT_Former_Areas$Element=="Value"] = 7280
FAOSTAT_Former_Areas$Element.Code[FAOSTAT_Former_Areas$Item.Code==3104&FAOSTAT_Former_Areas$Element=="Value"] = 7282
FAOSTAT_Former_Areas$Element.Code[FAOSTAT_Former_Areas$Item.Code==3102&FAOSTAT_Former_Areas$Element=="Value_Area"] = 7276
FAOSTAT_Former_Areas$Element.Code[FAOSTAT_Former_Areas$Item.Code==3103&FAOSTAT_Former_Areas$Element=="Value_Area"] = 7281
FAOSTAT_Former_Areas$Element.Code[FAOSTAT_Former_Areas$Item.Code==3104&FAOSTAT_Former_Areas$Element=="Value_Area"] = 7283

FAOSTAT_Former_Areas$Element[FAOSTAT_Former_Areas$Element.Code==7275] = "Cropland nitrogen"
FAOSTAT_Former_Areas$Element[FAOSTAT_Former_Areas$Element.Code==7280] = "Cropland phosphorus"
FAOSTAT_Former_Areas$Element[FAOSTAT_Former_Areas$Element.Code==7282] = "Cropland potassium"
FAOSTAT_Former_Areas$Element[FAOSTAT_Former_Areas$Element.Code==7276] = "Cropland nitrogen per unit area"
FAOSTAT_Former_Areas$Element[FAOSTAT_Former_Areas$Element.Code==7281] = "Cropland phosphorus per unit area"
FAOSTAT_Former_Areas$Element[FAOSTAT_Former_Areas$Element.Code==7283] = "Cropland potassium per unit area"

FAOSTAT_Former_Areas$Item.Code[FAOSTAT_Former_Areas$Item == "Mineral fertilizers"] = 5087
FAOSTAT_Former_Areas$Item.Code[FAOSTAT_Former_Areas$Item == "Manure applied to soils"] = 5062

FAOSTAT_Former_Areas$Domain.Code = "ESB"
FAOSTAT_Former_Areas$Domain = "Cropland Nutrient Balance"
FAOSTAT_Former_Areas$Year.Code=FAOSTAT_Former_Areas$Year
FAOSTAT_Former_Areas = select(FAOSTAT_Former_Areas,-Area)

# Combine ESB and FAOSTAT
ESB_2025_Former_Areas = rbind(filter(ESB_2025_Former_Areas,!(Item%in%c("Manure applied to soils","Mineral fertilizers"))),FAOSTAT_Former_Areas)
# Calculate Inputs (Total)
# Calculate balance
# Calculate NUE
Land_Use = select(Land_Use,-Element.Code,-Area)
ESB_2025_Former_Areas_Totals = left_join(ESB_2025_Former_Areas,Land_Use)

ESB_2025_Former_Areas_Totals_1 = ESB_2025_Former_Areas_Totals %>%
  dplyr::filter(Item%in%c("Atmospheric Deposition","Biological Fixation","Seed","Mineral fertilizers","Manure applied to soils"),
                Element%in%c("Cropland nitrogen","Cropland phosphorus","Cropland potassium")) %>%
  dplyr::group_by(Domain,Domain.Code,Area.Code,Element.Code,Element,Year.Code,Year) %>%
  dplyr::summarise(Value = sum(Value,na.rm=TRUE),
                   Value_Area = sum(Value,na.rm=TRUE)/sum(CropLand,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(Flag = "E",
         Item.Code = 5079,
         Item = "Inputs (Total)")
ESB_2025_Former_Areas_Totals_1 = gather(ESB_2025_Former_Areas_Totals_1,Boundary,Value,Value,Value_Area)
ESB_2025_Former_Areas_Totals_1$Element[ESB_2025_Former_Areas_Totals_1$Boundary=="Value_Area"&ESB_2025_Former_Areas_Totals_1$Element=="Cropland nitrogen"] = "Cropland nitrogen per unit area"
ESB_2025_Former_Areas_Totals_1$Element[ESB_2025_Former_Areas_Totals_1$Boundary=="Value_Area"&ESB_2025_Former_Areas_Totals_1$Element=="Cropland phosphorus"] = "Cropland phosphorus per unit area"
ESB_2025_Former_Areas_Totals_1$Element[ESB_2025_Former_Areas_Totals_1$Boundary=="Value_Area"&ESB_2025_Former_Areas_Totals_1$Element=="Cropland potassium"] = "Cropland potassium per unit area"

ESB_2025_Former_Areas_Totals_1$Element.Code[ESB_2025_Former_Areas_Totals_1$Element=="Cropland nitrogen per unit area"] = 7276
ESB_2025_Former_Areas_Totals_1$Element.Code[ESB_2025_Former_Areas_Totals_1$Element=="Cropland phosphorus per unit area"] = 7281
ESB_2025_Former_Areas_Totals_1$Element.Code[ESB_2025_Former_Areas_Totals_1$Element=="Cropland potassium per unit area"] = 7283
ESB_2025_Former_Areas_Totals_1 = select(ESB_2025_Former_Areas_Totals_1,-Boundary)


ESB_2025_Former_Areas = rbind(ESB_2025_Former_Areas,ESB_2025_Former_Areas_Totals_1) 

ESB_2025_Former_Areas_Totals_2_Inputs = filter(ESB_2025_Former_Areas,Item=="Inputs (Total)",Element%in%c("Cropland nitrogen",
                                                                                                         "Cropland nitrogen per unit area",
                                                                                                         "Cropland phosphorus",
                                                                                                         "Cropland phosphorus per unit area",
                                                                                                         "Cropland potassium",
                                                                                                         "Cropland potassium per unit area"))
ESB_2025_Former_Areas_Totals_2_Outputs = filter(ESB_2025_Former_Areas,Item=="Outputs (Total)",Element%in%c("Cropland nitrogen",
                                                                                                           "Cropland nitrogen per unit area",
                                                                                                           "Cropland phosphorus",
                                                                                                           "Cropland phosphorus per unit area",
                                                                                                           "Cropland potassium",
                                                                                                           "Cropland potassium per unit area"))
ESB_2025_Former_Areas_Totals_2 = left_join(ESB_2025_Former_Areas_Totals_2_Inputs,ESB_2025_Former_Areas_Totals_2_Outputs,by=c("Domain", "Domain.Code", "Area.Code", "Element.Code", "Year.Code", "Year", "Element","Flag"))

# I AM HERE

ESB_2025_Former_Areas_Totals_2_Balance = ESB_2025_Former_Areas_Totals_2 %>%
  rowwise() %>%
  mutate(Value = Value.x - Value.y) %>%
  mutate(Flag = "E",
         Item.Code = 5081,
         Item = "Nutrient Balance (Total)") %>%
  ungroup()

ESB_2025_Former_Areas_Totals_2_Balance = select(ESB_2025_Former_Areas_Totals_2_Balance,-Value.x,-Value.y,-Item.Code.x,-Item.Code.y,-Item.x,-Item.y)

ESB_2025_Former_Areas_Totals_2_NUE = ESB_2025_Former_Areas_Totals_2 %>%
  filter(Element.Code%in%c(7275,7280,7282)) %>%
  rowwise() %>%
  mutate(Value = (Value.y/Value.x)*100) %>%
  mutate(Flag = "E",
         Item.Code=5081,
         Item="Nutrient Balance (Total)") 
ESB_2025_Former_Areas_Totals_2_NUE$Element.Code[ESB_2025_Former_Areas_Totals_2_NUE$Element.Code==7275] = 7290
ESB_2025_Former_Areas_Totals_2_NUE$Element.Code[ESB_2025_Former_Areas_Totals_2_NUE$Element.Code==7280] = 7291
ESB_2025_Former_Areas_Totals_2_NUE$Element.Code[ESB_2025_Former_Areas_Totals_2_NUE$Element.Code==7282] = 7292
ESB_2025_Former_Areas_Totals_2_NUE$Element[ESB_2025_Former_Areas_Totals_2_NUE$Element.Code==7290] = "Cropland nitrogen use efficiency"
ESB_2025_Former_Areas_Totals_2_NUE$Element[ESB_2025_Former_Areas_Totals_2_NUE$Element.Code==7291] = "Cropland phosphorus use efficiency"
ESB_2025_Former_Areas_Totals_2_NUE$Element[ESB_2025_Former_Areas_Totals_2_NUE$Element.Code==7292] = "Cropland potassium use efficiency"

ESB_2025_Former_Areas_Totals_2_NUE = 
  select(ESB_2025_Former_Areas_Totals_2_NUE,-Value.x,-Value.y,-Item.Code.x,-Item.Code.y,-Item.x,-Item.y)

ESB_2025_Former_Areas = rbind(ESB_2025_Former_Areas,select(ESB_2025_Former_Areas_Totals_2_Balance,colnames(ESB_2025_Former_Areas)))
ESB_2025_Former_Areas = rbind(ESB_2025_Former_Areas,select(ESB_2025_Former_Areas_Totals_2_NUE,colnames(ESB_2025_Former_Areas)))

ESB_2025_Former_Areas$Unit[ESB_2025_Former_Areas$`Element.Code`%in%c(7275,7280,7282)] = "tonnes"
ESB_2025_Former_Areas$Unit[ESB_2025_Former_Areas$`Element.Code`%in%c(7276,7281,7283)] = "kg/ha"
ESB_2025_Former_Areas$Unit[ESB_2025_Former_Areas$`Element.Code`%in%c(7290,7291,7292)] = "%"
ESB_2025_Former_Areas = left_join(ESB_2025_Former_Areas,select(m49,Country.Code,M49.Code,Country),by=c("Area.Code"="Country.Code"))

ESB_2025 = read.csv(gzfile("./Output/ESB_Data_2025_7.gz"))
ESB_2025 = select(ESB_2025,-X)
ESB_2025 = rbind(ESB_2025,select(ESB_2025_Former_Areas,colnames(ESB_2025)))
write.csv(ESB_2025,gzfile("./Output/ESB_Data_2025_With_Former_Areas_4.gz"),row.names=FALSE)
