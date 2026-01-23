Land_Area = read.csv("./Data/Inputs_LandUse_E_All_Data_(Normalized)_2024.csv")
# Land_Area = dplyr::rename(Land_Area,
#                           Area.Code = Area.Code..FAO.,
#                           Item.Code = Item.Code..FAO.,
#                           Element.Code = Element.Code..FAO.)
Land_Area = filter(Land_Area,Item.Code%in%c(6610,6620),Element.Code==5110)
Land_Area = select(Land_Area,
                   Area.Code,
                   Year,
                   Value,
                   Item.Code,
                   Item)
StartYear=1961
EndYear=2023
Land_Area = Data_Update(StartYear,EndYear,Land_Area)
Land_Area = select(Land_Area,-Item.Code)
Land_Area = spread(Land_Area,Item,Value)
Land_Area = Land_Area %>%
  rowwise() %>%
  mutate(Agr_Share = `Agricultural land`/Cropland) %>%
  ungroup()
Land_Area = filter(Land_Area,!is.na(Agr_Share))

N_Deposition = read.csv("./Data/N_Deposition_2024.csv")
N_Deposition = select(N_Deposition,-Country)
N_Deposition = gather(N_Deposition,Year,Value,-m49)
N_Deposition$Year = as.numeric(substr(N_Deposition$Year,2,5))
N_Deposition = left_join(N_Deposition,select(m49,Country.Code,M49.Code),by=c("m49"="M49.Code"))
N_Deposition = select(rename(N_Deposition,Area.Code=Country.Code),-m49)
N_Deposition = left_join(N_Deposition,Land_Area)
# N Deposition was in kg/ha
# Cropland is in 1000 ha
N_Deposition = N_Deposition %>%
  rowwise() %>%
  mutate(Value_Deposition = Value*Cropland*1000)
# Now N Deposition is in kg
N_Deposition = select(N_Deposition,Area.Code,Year,Value_Deposition)
N_Deposition$NutrientCode=3102
N_Deposition_China = N_Deposition %>%
  filter(Area.Code%in%c(41,
                        96,
                        128,
                        214)) %>%
  group_by(NutrientCode,Year) %>%
  summarise(Value_Deposition = sum(Value_Deposition,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(Area.Code=351) %>%
  select(colnames(N_Deposition))

N_Deposition = rbind(N_Deposition,N_Deposition_China)
N_Deposition = as.data.frame(N_Deposition)
# Temporary until updated manure data is received
N_Deposition = N_Deposition %>%
  group_by(Area.Code,NutrientCode) %>% 
  complete(Year = min(Year):2023) %>%
  tidyr::fill(Value_Deposition,.direction=c("down")) %>%
  ungroup()

