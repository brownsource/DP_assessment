####################################################################################################################
#| FUNCTION TO DRAW A BAR CHART ####################################################################################
####################################################################################################################

#| Variables passed to the function
#| 
#| draw_bar_chart(
#| title            "All sectors data protection ratings by measure",                 
#| filename         "All_sectors_data_protection_ratings_by_measure",                 
#| ylabel           "Data protection rating",                                          
#| xlabel           "Data protection measure",                                        
#| chart_data       network_statistics_measures,                                      
#| label_value      as.numeric(network_statistics_measures$all),                      
#| xaxis            row.names(network_statistics_measures),                           
#| yaxis            as.numeric(network_statistics_measures$all)                       
#| ordered          c(TRUE,FALSE)
#| anon             c(TRUE,FALSE)
#| folder
#| object_name
# )

draw_long_bar_chart <- function(title, filename, ylabel, xlabel, chart_data, label_value, xaxis, yaxis, ordered, folder, object_name){
  
  temp_chart_data <- chart_data
  temp_chart_data <- cbind(temp_chart_data, xaxis)
  temp_chart_data <- cbind(temp_chart_data, yaxis)
  #Turn your 'treatment' column into a character vector
  temp_chart_data$xaxis <- as.character(temp_chart_data$xaxis)
  #Then turn it back into an ordered factor
  temp_chart_data$xaxis <- factor(temp_chart_data$xaxis, levels=unique(temp_chart_data$xaxis))
  # sorts the data if specified // DOESN'T WORK AT THE MOMENT
  # if(ordered == TRUE) temp_chart_data <- temp_chart_data[order(-temp_chart_data$yaxis),]
  
  
  # Prepares the ggplot
  object_name <- ggplot(data = temp_chart_data, #source data
                        aes(x=temp_chart_data$xaxis,
                            y=temp_chart_data$yaxis)) +
    
    #change the bar colour to highligh above and below mean
    geom_bar(stat="identity",
             fill= ifelse(temp_chart_data$yaxis < mean(temp_chart_data$yaxis, na.rm = TRUE),"grey60","#0072B6")) +
    
    #Add the mean line
    geom_hline(yintercept = mean(temp_chart_data$yaxis, na.rm = TRUE),
               color="grey30",
               size=1.5,
               alpha=0.5,
               linetype="dashed") +
    
    #add a label inside the bar with the value
    geom_text(aes(label=round(temp_chart_data$yaxis,2)),
              vjust=1.6,
              color="white",
              size=3.5) +
    
    #Add chart title and axis labels
    ggtitle(title) +
    ylab(ylabel) +
    xlab(xlabel) +
    
    #set the theme, no legend, no gridlines, no background, no axis ticks
    theme(legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1)
          )


  #Save the chart as png
  File <- paste("../not_shared/output/",country,"/data_protection/",folder,"/",filename,".png", sep="")
  dir.create(dirname(File), showWarnings = FALSE)
  ggsave(File,
         plot = object_name,
         device = "png",
         scale = 1,
         width = 12,
         height = 6,
         units = "in",
         dpi = 300)
  
  plot(object_name)
}