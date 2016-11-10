# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

## export charts
draw_bar_chart <- function(title, filename, ylabel, xlabel, chart_data, label_value, xaxis, yaxis){
  chart_export <- ggplot(data=chart_data, 
                         aes(x=reorder(xaxis,-as.numeric(yaxis)), y=yaxis)) + 
    geom_bar(stat="identity",
             fill="#0072B6") +
    geom_text(aes(label=round(as.numeric(label_value),2)), vjust=1.6, color="white", size=3.5) + 
    #theme_minimal() + 
    theme(legend.position="none") + 
    ggtitle(title) + 
    ylab(xlabel)+
    xlab(ylabel) +
    geom_hline(yintercept = mean(as.numeric(yaxis), na.rm = TRUE), color="grey30", size=1.5, alpha=0.5, linetype="dashed") +
    coord_cartesian(ylim=c(0,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
  
  File <- paste("../not_shared/output/",country,"/data_protection/measures/",filename,".png", sep="")
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