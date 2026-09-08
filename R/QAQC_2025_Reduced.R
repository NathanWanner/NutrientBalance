# Updated comments Jan 26, 2026  

library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  source("./Functions/multiple_line_charts.R")
  m49 = read.csv("./Tables/m49_UTF-8.csv",fileEncoding ="UTF-8-BOM")
  FAO_Country_Group_M49 = read.csv("./Tables/FAO_Country_Group_M49.csv")
  area_regions = read.csv("./Tables/areas_regions_hierarchy_With_China.csv")
  FAO_Country_Region = read.csv("./Tables/FAO_Country_Region.csv")
  
  ESB_2026 = read.csv(gzfile("./Output/ESB_Data_2026_With_Former_Areas_1.gz"))
  # ESB_2026 = dplyr::select(ESB_2026,-Country,-X)
  ESB_2026 = dplyr::select(ESB_2026,-Country)
  ESB_2026$d.source = "ESB 2026"
  ESB_2026 = dplyr::left_join(ESB_2026,dplyr::select(m49,Country.Code,Country),by=c("Area.Code"="Country.Code"))
  ESB_2026$Item = as.character(ESB_2026$Item)
  
  ESB_2025 = read.csv(gzfile("./Output/ESB_Data_2025_With_Former_Areas_4.gz"))
  ESB_2025 = dplyr::select(ESB_2025,-Country)
  ESB_2025 = dplyr::left_join(ESB_2025,dplyr::select(m49,Country.Code,Country),by=c("Area.Code"="Country.Code"))
  ESB_2025$d.source = "ESB 2025"
  ESB_2025$Item = as.character(ESB_2025$Item)
  ESB_2025$Item[ESB_2025$Item=="Nutrient Budget (Total)"] = "Nutrient Balance (Total)"
  ESB_2025$Item[ESB_2025$Item=="Synthetic fertilizers"] = "Mineral fertilizers"
  ESB_2025$Item.Code[ESB_2025$Item.Code==5061] = 5087
  
  Data = rbind(ESB_2026,select(ESB_2025,Domain.Code,Domain,Area.Code,M49.Code,Country,Element.Code,Element,Item.Code,Item,Year.Code,Year,Unit,Value,Flag,d.source))
  # Data = rbind(Data,ESB_2025_Update)

  Data$Item = as.factor(as.character(Data$Item))
  Data = dplyr::rename(Data,AreaName=Country)
  Data = dplyr::left_join(Data,select(FAO_Country_Region,M49.Code,Start.Year,End.Year),by=c("M49.Code" = "M49.Code"))
  Data$Start.Year[is.na(Data$Start.Year)] = 1961
  Data$End.Year[is.na(Data$End.Year)] = 2024
  Data = filter(Data,Year>=Start.Year)
  Data = filter(Data,Year<=End.Year)
  
  colour_list_df <-
    Data %>%
    dplyr::select(d.source) %>%
    dplyr::distinct() %>%
    # red, green, 
    dplyr::mutate(colour = c("#33CC00", "#3366FF"))
  colour_assignment <-
    setNames(unlist(colour_list_df[,"colour"]), unname(unlist(colour_list_df[,"d.source"])))
  
  
  # Line type by d.source (variable in "series_by")
  lines_list_df <-
    Data %>%
    dplyr::select(d.source) %>%
    distinct() %>%
    dplyr::mutate(linetype = c("solid", "dashed"))
  line_assignment <-
    setNames(unlist(lines_list_df[,"linetype"]), unname(unlist(lines_list_df[,"d.source"])))
  
  
  # Symbol by flag (variable in "symbol_by")
  symbol_by = "Flag"
  symbol_list_df <- 
    Data %>%
    dplyr::select(Flag) %>%
    dplyr::distinct() %>%
    dplyr::mutate(shape = c(1:4))
  symbol_assignment <-
    setNames(unlist(symbol_list_df[,"shape"]), unname(unlist(symbol_list_df[,symbol_by])))
  Data = unique(Data)
  Data_Chart = dplyr::filter(Data,Element=="Cropland nitrogen",!Item.Code==5086)
  Data_Chart = dplyr::filter(Data_Chart,Item%in%c("Biological Fixation","Atmospheric Deposition","Mineral fertilizers","Manure applied to soils"))
  Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
  Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
  Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
  multiple_line_charts(data = Data_Chart,
                       output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                       series_by = "d.source",
                       x_variable = "Year",
                       y_variable = "Value",
                       plots_by = "Item",
                       pages_by = "AreaName",
                       files_by = NULL,
                       symbol_by = "Flag",
                       units = "Unit",
                       plots_per_row = 2,
                       plots_per_column = 2,
                       colour_assignment = colour_assignment,
                       line_assignment = line_assignment,
                       symbol_assignment = symbol_assignment,
                       file_name = "Cropland nitrogen inputs",
                       legend=TRUE
  )
  
  Data_Chart = dplyr::filter(Data,Element=="Cropland nitrogen",!Item.Code==5086)
  Data_Chart = dplyr::filter(Data_Chart,Item%in%c("Crop Removal"))
  Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
  Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
  Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
  multiple_line_charts(data = Data_Chart,
                       output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                       series_by = "d.source",
                       x_variable = "Year",
                       y_variable = "Value",
                       plots_by = "Item",
                       pages_by = "AreaName",
                       files_by = NULL,
                       symbol_by = "Flag",
                       units = "Unit",
                       plots_per_row = 1,
                       plots_per_column = 1,
                       colour_assignment = colour_assignment,
                       line_assignment = line_assignment,
                       symbol_assignment = symbol_assignment,
                       file_name = "Cropland nitrogen outputs",
                       legend=TRUE
  )
  
  Data_Chart = dplyr::filter(Data,Element=="Cropland nitrogen",!Item.Code==5086)
  Data_Chart = dplyr::filter(Data_Chart,Item%in%c("Volatilisation","Leaching"))
  Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
  Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
  Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
  multiple_line_charts(data = Data_Chart,
                       output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                       series_by = "d.source",
                       x_variable = "Year",
                       y_variable = "Value",
                       plots_by = "Item",
                       pages_by = "AreaName",
                       files_by = NULL,
                       symbol_by = "Flag",
                       units = "Unit",
                       plots_per_row = 2,
                       plots_per_column = 1,
                       colour_assignment = colour_assignment,
                       line_assignment = line_assignment,
                       symbol_assignment = symbol_assignment,
                       file_name = "Cropland nitrogen volatilisation and leaching",
                       legend=TRUE
  )
  
  Data_Chart = dplyr::filter(Data,Element=="Cropland nitrogen",!Item.Code==5086)
  Data_Chart = dplyr::filter(Data_Chart,Item%in%c("Inputs (Total)","Outputs (Total)","Nutrient Balance (Total)"))
  Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
  Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
  Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
  multiple_line_charts(data = Data_Chart,
                       output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                       series_by = "d.source",
                       x_variable = "Year",
                       y_variable = "Value",
                       plots_by = "Item",
                       pages_by = "AreaName",
                       files_by = NULL,
                       symbol_by = "Flag",
                       units = "Unit",
                       plots_per_row = 1,
                       plots_per_column = 3,
                       colour_assignment = colour_assignment,
                       line_assignment = line_assignment,
                       symbol_assignment = symbol_assignment,
                       file_name = "Cropland nitrogen totals",
                       legend=TRUE
  )


Data_Chart = dplyr::filter(Data,Element=="Cropland phosphorus",Item.Code%in%c(5077,5087,5062,5079,5080,5081))
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Cropland phosphorus",
                     legend=TRUE
)

Data_Chart = dplyr::filter(Data,Element=="Cropland potassium",Item.Code%in%c(5077,5087,5062,5079,5080,5081))
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Cropland potassium",
                     legend=TRUE
)

Data_Chart = dplyr::filter(Data,Element=="Cropland nitrogen per unit area",!Item.Code==5086)
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 5,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Cropland nitrogen per unit area",
                     legend=TRUE
)

Data_Chart = filter(Data,Element=="Cropland phosphorus per unit area",Item.Code%in%c(5077,5087,5062,5079,5080,5081))
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Cropland phosphorus per unit area",
                     legend=TRUE
)
Data_Chart = filter(Data,Element=="Cropland potassium per unit area",Item.Code%in%c(5077,5087,5062,5079,5080,5081))
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))

multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 5,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Cropland potassium per unit area",
                     legend=TRUE
)

Data_Chart = filter(Data,Element%in%c("Cropland nitrogen use efficiency",
                                      "Cropland phosphorus use efficiency",
                                      "Cropland potassium use efficiency"),
                    Item.Code%in%c(5081))
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Element",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 1,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Cropland nutrient use efficiency",
                     legend=TRUE
)

Data_Chart = dplyr::filter(Data,Element=="Agricultural land nitrogen",!Item.Code==5086)
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 5,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Agricultural land nitrogen",
                     legend=TRUE
)
Data_Chart = filter(Data,Element=="Agricultural land phosphorus",Item.Code%in%c(5077,5087,5062,5079,5080,5081))
# Data_Chart = dplyr::filter(Data,Element=="Agricultural land nitrogen")
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Agricultural land phosphorus",
                     legend=TRUE
)

Data_Chart = filter(Data,Element=="Agricultural land potassium",Item.Code%in%c(5077,5087,5062,5079,5080,5081))
# Data_Chart = dplyr::filter(Data,Element=="Agricultural land nitrogen")
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = filter(Data,Element=="Agricultural land potassium"),
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Agricultural land potassium",
                     legend=TRUE
)
Data_Chart = dplyr::filter(Data,Element=="Agricultural land nitrogen per unit area",!Item.Code==5086)
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 5,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Agricultural land nitrogen per unit area",
                     legend=TRUE
)
Data_Chart = filter(Data,Element=="Agricultural land phosphorus per unit area",Item.Code%in%c(5077,5087,5062,5079,5080,5081))
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Agricultural land phosphorus per unit area",
                     legend=TRUE
)

Data_Chart = filter(Data,Element=="Agricultural land potassium per unit area",Item.Code%in%c(5077,5087,5062,5079,5080,5081))
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Item",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Agricultural land potassium per unit area",
                     legend=TRUE
)

Data_Chart = filter(Data,Element%in%c("Agricultural land nitrogen use efficiency",
                                      "Agricultural land phosphorus use efficiency",
                                      "Agricultural land potassium use efficiency"),
                    Item.Code%in%c(5081))
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Element",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 1,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Agricultural land nutrient use efficiency",
                     legend=TRUE
)

Data_Chart = filter(Data,Item.Code==5086,Element%in%c("Cropland nitrogen",
                                         "Cropland phosphorus",
                                         "Cropland potassium",
                                         "Cropland nitrogen per unit area",
                                         "Cropland phosphorus per unit area",
                                         "Cropland potassium per unit area"))

Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Element",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 2,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Seeds",
                     legend=TRUE
)

Data_Chart = filter(Data,Item.Code==5180,Element%in%c("Cropland nitrogen fertilizer dependency",
                                                      "Cropland phosphorus fertilizer dependency",
                                                      "Cropland potassium fertilizer dependency"))

Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Element",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 1,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "FDI",
                     legend=TRUE
)

Data_Chart = filter(Data,Item.Code==5081,Element%in%c("Sustainable Nitrogen Management Index"))

Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Element",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 1,
                     plots_per_column = 1,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "SNMI",
                     legend=TRUE
)

Data_Chart = dplyr::filter(Data,Element%in%c("Cropland nitrogen",
                                             "Cropland phosphorus",
                                             "Cropland potassium"),Item.Code==5088)
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Element",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 1,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Crop Residues Removal",
                     legend=TRUE
)

Data_Chart = dplyr::filter(Data,Element%in%c("Cropland nitrogen per unit area",
                                             "Cropland phosphorus per unit area",
                                             "Cropland potassium per unit area"),Item.Code==5088)
Data_Chart$Item = as.factor(as.character(Data_Chart$Item))
Data_Chart$Element = as.factor(as.character(Data_Chart$Element))
Data_Chart$AreaName = as.factor(as.character(Data_Chart$AreaName))
multiple_line_charts(data = Data_Chart,
                     output_folder = "C:/Users/WANNER/OneDrive - Food and Agriculture Organization/Telworking_Work/SNB/2026/QAQC/",
                     series_by = "d.source",
                     x_variable = "Year",
                     y_variable = "Value",
                     plots_by = "Element",
                     pages_by = "AreaName",
                     files_by = NULL,
                     symbol_by = "Flag",
                     units = "Unit",
                     plots_per_row = 1,
                     plots_per_column = 3,
                     colour_assignment = colour_assignment,
                     line_assignment = line_assignment,
                     symbol_assignment = symbol_assignment,
                     file_name = "Crop Residues Removal per unit area",
                     legend=TRUE
)