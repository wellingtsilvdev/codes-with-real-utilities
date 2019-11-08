allvar_plots <- function(data,type="hist"){
  library(RColorBrewer)
  if(type=="hist"){
    #loop to go through all dataframe cols
    for (i in 1:length(colnames(data))) {
      #ploting histograms
      if(is.factor(data[,i])==F){
        hist(data[,i],
             main = paste("Histogram of" ,colnames(data)[i])
             ,xlab=colnames(data)[i]
             ,col = 'blue')
      }
      i=+1
    }
  }
  if(type=="plot"){
    #loop to go through all dataframe cols
    for (i in 1:length(colnames(data))) {
      #ploting histograms
      if(is.factor(data[,i])==T){
        plot(data[,i],type="l",
             main = paste("plot of" ,colnames(data)[i])
             ,xlab=colnames(data)[i]
             ,ylab = "Frequency"
             ,col= brewer.pal(n = 12, name = "Set3"))
      }
      if(is.numeric(data[,i])==T){
        plot(data[,i],type="l",
             main = paste("plot of" ,colnames(data)[i])
             ,xlab=colnames(data)[i]
             ,ylab = "variation"
             ,col='blue')
      }
      i=+1
    }
  }
}
