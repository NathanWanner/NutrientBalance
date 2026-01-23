Data_Update = function(StartYear,EndYear,Data){
  
  Data = left_join(Data,FAO_Country_Region,by=c("Area.Code"="Country.Code"))
  Data$Start.Year[is.na(Data$Start.Year)]  = StartYear
  Data$End.Year[is.na(Data$End.Year)]  = EndYear
  
  Data = Data %>%
    dplyr::group_by(Area.Code,Item.Code,Item) %>%
    complete(Year = min(c(Year,Start.Year)):max(c(Year,End.Year))) %>%
    mutate(Value = na.approx(Value, na.rm = FALSE)) %>%
    arrange(Area.Code,Year) %>%
    fill(everything(),-Area.Code,-Item.Code,-Item,.direction="down") %>%
    fill(everything(),-Area.Code,-Item.Code,-Item,.direction="up") %>%
    dplyr::ungroup()

  Data = filter(Data,
                    Year>=Start.Year,
                    Year<=End.Year)
  
  Data_Merge = left_join(Data,areas_merge,by=c("Area.Code"="AREA_CODE"))
  
  # First Fix Serbia and Montenegro
  
  if(272%in%Data$Area.Code&273%in%Data$Area.Code){
  Data_Merge = Data_Merge %>%
    filter(Area.Code%in%c(272,273)) %>%
    filter(Year==2006) %>%
    dplyr::group_by(Item.Code,Item) %>%
    mutate(Share = Value/sum(Value,na.rm=TRUE)) %>%
    mutate(AREA_CODE_2=186) %>%
    dplyr::ungroup()
  
  Data = left_join(Data,Data_Merge,by=c("Area.Code"="AREA_CODE_2","Item.Code","Item"))
  
  Data$Value[Data$Year.x<=2005&Data$Year.x>1991&!is.na(Data$Area.Code.y)] = Data$Value.x[Data$Year.x<=2005&Data$Year.x>1991&!is.na(Data$Area.Code.y)]*
    Data$Share[Data$Year.x<=2005&Data$Year.x>1991&!is.na(Data$Area.Code.y)]
  Data$Value[is.na(Data$Value)] = Data$Value.x[is.na(Data$Value)]
  Data$Area.Code[!is.na(Data$Area.Code.y)] = Data$Area.Code.y[!is.na(Data$Area.Code.y)]
  Data = filter(Data,Area.Code!=186)
  Data$Year = Data$Year.x
  
  Data$Item = as.character(Data$Item)
  Data = select(Data,Area.Code,Item,Item.Code,Year,Value)
  }
  # Now fix the remaining countries
  
  Data_Merge = left_join(Data,areas_merge,by=c("Area.Code"="AREA_CODE"))

  Data_Merge = Data_Merge %>%
    filter(!is.na(AREA_CODE_FINAL)) %>%
    dplyr::group_by(AREA_CODE_FINAL,Item.Code,Item) %>%
    filter(Year==END1+1) %>%
    mutate(Share = Value/sum(Value,na.rm=TRUE)) %>%
    dplyr::ungroup()
  
  Data = left_join(Data,Data_Merge,by=c("Area.Code"="AREA_CODE_FINAL","Item.Code","Item"))
  
  Data$Value[Data$Year.x<=Data$END1&!is.na(Data$Area.Code.y)] = Data$Value.x[Data$Year.x<=Data$END1&!is.na(Data$Area.Code.y)]*
    Data$Share[Data$Year.x<=Data$END1&!is.na(Data$Area.Code.y)]
  Data$Value[is.na(Data$Value)] = Data$Value.x[is.na(Data$Value)]
  Data$Area.Code[!is.na(Data$Area.Code.y)] = Data$Area.Code.y[!is.na(Data$Area.Code.y)]
  Data = filter(Data,!Area.Code%in%areas_merge$AREA_CODE_FINAL)
  Data$Year = Data$Year.x
  
  Data$Item = as.character(Data$Item)
  Data = select(Data,Area.Code,Item,Item.Code,Year,Value)
  
  return(Data)

}