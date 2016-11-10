# Function to draw bar chart
#
# draw_bar_chart(
#  title            "All sectors data protection ratings by measure",                 
#  filename         "All_sectors_data_protection_ratings_by_measure",                 
#  ylabel           "Data protection rating",                                          
#  xlabel           "Data protection measure",                                        
#  chart_data       network_statistics_measures,                                      
#  label_value      as.numeric(network_statistics_measures$all),                      
#  xaxis            row.names(network_statistics_measures),                           
#  yaxis            as.numeric(network_statistics_measures$all)                       
#  ordered          c(TRUE,FALSE)
# )

draw_bar_chart <- function(title, filename, ylabel, xlabel, chart_data, label_value, xaxis, yaxis, ordered){
  
  # Prepares the ggplot
  print(title)
  print(filename)
  print(ylabel)
  print(xlabel)
  print(chart_data)
  print(label_value)
  print(xaxis)
  print(yaxis)
  print(ordered)
  
  chart_export <- ggplot(data = chart_data, #source data
                         aes(x=reorder(xaxis,-yaxis), 
                         y=yaxis)) + 
    
                         geom_bar(stat="identity",
                         fill="#0072B6") +
    
                         geom_text(aes(label=round(label_value,2)), 
                                   vjust=1.6, 
                                   color="white", 
                                   size=3.5) + 
    
                        theme(legend.position="none",
                              panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black"),
                              axis.ticks.x=element_blank(),
                              axis.ticks.y=element_blank()) +

                        ggtitle(title) + 
    
                        ylab(xlabel) +
    
                        xlab(ylabel) +
    
                        geom_hline(yintercept = mean(yaxis, na.rm = TRUE), 
                                   color="grey30", 
                                   size=1.5, 
                                   alpha=0.5, 
                                   linetype="dashed") +
    
                        coord_cartesian(ylim=c(0,1))
    
    
  
  File <- paste("../not_shared/output/",country,"/data_protection/sectors/",filename,".png", sep="")
  dir.create(dirname(File), showWarnings = FALSE)
  ggsave(File, 
         plot = chart_export, 
         device = "png", 
         scale = 1, 
         width = 8, 
         height = 6, 
         units = "in", 
         dpi = 300) 
}