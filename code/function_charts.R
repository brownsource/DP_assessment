### GRAPH FUNCTIONS

### FUNCTION TO DRAW BAR CHART WITH FORMATTING

#| Receives the following arguements:
#|
#| filename       required - what the file will be called
#| folder	        required - where the file will be saved
#| title          required - the chart title displayed in the chart
#| data           the data table
#| xaxis          the dimenstion to plot on the x-axis
#| yaxis          the dimenstion to plot on the y-axis
#| sort           if the chart should be sorted. default = FALSE, TRUE means descending based on value of y-axis
#| anonymise      if the chart should be anonymised. default = FALSE, TRUE means removinf x-axis labels

draw_bar_chart <- function(filename, folder, title, data, xaxis, yaxis, sort = FALSE, anonymise=FALSE){
  #ensure folder is created to save png to
  dir.create(dirname(folder), showWarnings = FALSE)
  
  #create small data frame of charting data
  chart_data <- data.frame(cbind(as.character(data[,xaxis]),
                                 as.numeric(data[,yaxis])))
  cnames <- c(xaxis,yaxis)
  colnames(chart_data) <- cnames
  
  print(chart_data)
  print(as.numeric(as.character(chart_data[,yaxis])))
  #print(numeric(chart_data[,yaxis]))
  #print(chart_data[order(chart_data[,cnames$yaxis])])
  
}

