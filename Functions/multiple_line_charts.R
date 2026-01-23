# 
# # TITLE:      	Function for plotting FAOSTAT data into .pdf files
# # DESCRIPTION:	
# # AUTHOR:     	Javier Montero (based on v1.0 by Eduard Bukin & Javier Montero)
# # STARTED ON: 	18/08/2017
# # VERSION:    	v2.0
# # PURPOSE:
# 
# 
require(dplyr)
require(plyr)
require(tidyr)
require(grid)
library(ggplot2)
library(scales)


multiple_line_charts <-
  function(data,
           output_folder = "output/",
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
           file_name = file_name,
           legend = FALSE
  )
  {
    
    ### PREPARE DATA ----------------------------------------------------------------------------- 
    
    # Select relevant columns
    data = data[,c(series_by, x_variable, y_variable, plots_by, pages_by, files_by, symbol_by, units)]
    
    # Complete so that all countries will have same number of plots (and in same position)
    data <-
      data %>% 
      complete_(unique(c(plots_by, pages_by, files_by)))
    
    # Check that this selection does not produce duplicated lines
    if(nrow(data)!=nrow(distinct(data))) stop("The selected table has duplicated rows", call. = TRUE)
    
    # Check that output folder exists
    if (!dir.exists(output_folder)) stop("The folder specification is wrong", call. = TRUE)
    
    # Truncate columns in series by, if they are characters and the result is not ambiguous
    if(sapply(data[,series_by], is.character)) {
      truncation_condition <- paste0(series_by, " = substr(", series_by, ", 1, 12)")
      data_trunc <-
        data %>% 
        mutate_(.dots=setNames(truncation_condition,series_by))
      if(nrow(distinct(data[,series_by])) == nrow(distinct(data_trunc[,series_by]))){
        data <- data_trunc
      }
    }
    
    # Identify min and max value for the x asis
    min_x <- min(data[,x_variable])
    max_x <- max(data[,x_variable])

    
    #### GENERATE FILES -----------------------------------------------------------------------
    
    # Check if there is a variable to split in several files
    if(is.null(files_by)){
      file_iterator_vector <- c(1)
    } else {
      file_iterator_vector <- unlist(unique(data[,files_by])) %>% unname()
    }
    # Save the text for the condition for splitting into files (if needed)
    file_iteration_condition <- paste(files_by,"== file_iterator_vector[i]")
    
    # Start the loop to generate several files (if needed) ----
    for (i in 1:length(file_iterator_vector)) {
      if(is.null(files_by)){
        data_for_one_file <- data   
      } else {
        data_for_one_file <-
          data %>%
          filter_(file_iteration_condition)  
      }
      
      # Add sub_page_iterator: assigns plots to pages
      # If required number of plots per page is higher than available plot spaces per page, 
      # this iterator will split plots into several pages 
      available_plots_per_page <- plots_per_row * plots_per_column
      sub_page_iterator <-
        data_for_one_file %>% 
        dplyr::select(one_of(plots_by)) %>% 
        distinct() %>% 
        mutate(sub_page_iterator =
                 rep(c(1:nrow(.)),
                     each = available_plots_per_page,
                     length.out = nrow(.)))
      # Add the sub_page iterator to the data
      data_for_one_file <-
        data_for_one_file %>%
        left_join(sub_page_iterator, by = plots_by)
      
      # Create the page iterator
      page_iterator <- 
        data_for_one_file %>% 
        dplyr::select(one_of(pages_by),sub_page_iterator) %>% 
        dplyr::distinct() %>% 
        dplyr::arrange_(pages_by,"sub_page_iterator") %>% 
        dplyr::mutate(page_iterator = rep(c(1:nrow(.))))
      # Add the page iterator to the data (and drop sub_page iterator)
      data_for_one_file <-
        data_for_one_file %>%
        left_join(page_iterator) %>% 
        select(-sub_page_iterator)
      
      # Establish the plot matrix for each page, with the location of plots by index of the plot
      page_layout <- 
        matrix(
          seq(1, available_plots_per_page),
          ncol = plots_per_row, nrow = plots_per_column,
          byrow = TRUE
        )
      
      ### OPEN THE PDF ------------------------------------------------------------------------------------------------------
      
      pdf(
        paste0(file_name,
               ".pdf"),
        width = 29.7 / 1.8,
        height = 21 / 1.8
      )
      
      ### GENERATE PAGES --------------------------------------------------------------------------------------------------
      
      d_ply(data_for_one_file,
            .(page_iterator),
            function(data_for_one_page) {
              
              
              # Generate plots for one page ----
              plots_for_one_page <-
                dlply(data_for_one_page,
                      plots_by,
                      function(data_for_one_plot) {
                        
                        ### GENERATE ONE PLOT -------------------------------------------------------------
                        
                        # Check if there are data in Value column
                        is.na(data_for_one_plot[,y_variable])
                        if (nrow(data_for_one_plot %>% filter(!is.na(data_for_one_plot[,y_variable]))) == 0)
                        {
                          # If there are no data, plot message only
                          g <- ggplot(data.frame(x = c(1), y = c(1)), aes(x = x, y = y))
                          g <- g + theme_minimal()
                          g <- g + annotate( geom = "text", x = 1, y = 1, 
                                             label = paste0("There is no data for the ", plots_by, " : \n", unique(data_for_one_plot[,plots_by])))
                          
                        } else {
                          
                          # If there are data, define axis, colours and theme. Axis y starts from 0.
                          if (legend==TRUE){
                            g <- ggplot(data = data_for_one_plot, aes_string(x = x_variable, y = y_variable, 
                                                                           group = series_by, colour = series_by, 
                                                                           fill = series_by,  linetype = series_by)) +
                            scale_colour_manual(values=colour_assignment) +
                            scale_linetype_manual(values=line_assignment)
                          g <- g + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + 
                            # theme(legend.text=element_text(size=5),legend.key.size = unit(.5, 'cm')) +  
                            theme(legend.key.height= unit(.2, 'cm'),
                                  legend.key.width= unit(.5, 'cm'))
                          g <- g + scale_x_continuous(
                            minor_breaks = seq(min(data_for_one_plot[,x_variable], na.rm = TRUE) , max(data_for_one_plot[,x_variable], na.rm = TRUE), 1),
                            breaks = scales::pretty_breaks(n = (max(data_for_one_plot[,x_variable], na.rm = TRUE) - min(data_for_one_plot[,x_variable], na.rm = TRUE)) / 5)) 
                          g <- g + scale_y_continuous(
                            labels = function(x) format(x, big.mark = " ", scientific = FALSE), 
                            breaks = scales::pretty_breaks(n = 10))
                          g <- g + expand_limits(x = c(min_x, max_x), y = 0)}
                          else{
                            g <- ggplot(data = data_for_one_plot, aes_string(x = x_variable, y = y_variable, 
                                                                             group = series_by, colour = series_by, 
                                                                             fill = series_by,  linetype = series_by)) +
                              scale_colour_manual(values=colour_assignment) +
                              scale_linetype_manual(values=line_assignment)
                            g <- g + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + 
                              # theme(legend.text=element_text(size=5),legend.key.size = unit(.5, 'cm')) +  
                            g <- g + scale_x_continuous(
                              minor_breaks = seq(min(data_for_one_plot[,x_variable], na.rm = TRUE) , max(data_for_one_plot[,x_variable], na.rm = TRUE), 1),
                              breaks = scales::pretty_breaks(n = (max(data_for_one_plot[,x_variable], na.rm = TRUE) - min(data_for_one_plot[,x_variable], na.rm = TRUE)) / 5)) 
                            g <- g + scale_y_continuous(
                              labels = function(x) format(x, big.mark = " ", scientific = FALSE), 
                              breaks = scales::pretty_breaks(n = 10))
                            g <- g + expand_limits(x = c(min_x, max_x), y = 0)
                          }
                          
                          # Plot data points, using symbol_by if available
                          if (all(is.na(data_for_one_plot[,symbol_by]))) {
                            g <- g + geom_point()
                          } else {
                            g <- g + geom_point(aes_string(shape = symbol_by))
                            g <- g + scale_shape_manual(values = symbol_assignment)
                          }
                          
                          # If at least one series has more than one point, lines are plotted
                          # First check if at least one group has more than one observation.
                          # This is to avoid the error "geom_path: Each group consists of only one observation"
                          two_or_more_points <-
                            (
                              data_for_one_plot %>%
                                filter(!is.na(y_variable)) %>%
                                group_by_(series_by) %>% 
                                dplyr::summarise(num_obs = n()) %>%
                                mutate(num_obs = ifelse(num_obs > 1, TRUE, FALSE))
                            )$num_obs %>%
                            any()
                          # Plot lines (if at least one series has two or more points)
                          if (two_or_more_points) {
                            g <- g + geom_line()
                          }
                          
                          # Add titles and labels
                          g <- g + ggtitle(paste0(unique(data_for_one_plot[,pages_by]),"\n",unique(data_for_one_plot[,plots_by])))
                          g <- g + xlab(x_variable)
                          # If the variable with units is provided, they are shown in the plot
                          if (!is.null(units)) {
                            g <- g + ylab(paste(y_variable, unique(data_for_one_plot[,units])))
                          } else {
                            g <- g + ylab(paste0(y_variable))
                          }
                          
                        }
                        # Draw the plot
                        g  
                      }
                )
              
              # Assign the plots to their spaces in the page ----
              grid.newpage()
              
              # Initialize the output grid
              pushViewport(
                viewport(
                  layout = grid.layout(
                    nrow(page_layout), 
                    ncol(page_layout)
                  )))
              
              # Assign all the plots
              plyr::l_ply(c(1:length(plots_for_one_page)),
                          function(x) {
                            plotLocation <- as.data.frame(which(page_layout == x, arr.ind = TRUE))
                            print(plots_for_one_page[[x]],
                                  vp = viewport(
                                    layout.pos.row = plotLocation$row,
                                    layout.pos.col = plotLocation$col
                                  ))
                            
                          })
              
            },
            .progress = "text")
      
      # Close the pdf ----
      dev.off()
    
    # Close the loop to generate several files (if needed) ----    
    }
    
  # End of the function  
  }

