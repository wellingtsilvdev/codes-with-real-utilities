travel <-  function(data,element = 0,last.na = FALSE,first_row.na=F){
  
  #verifying first_row.na
  if(first_row.na==T){
    
    #loop to replace first row NA's by selected element
    for (i in 1:length(colnames(data))) {
      if(is.na(data[1,i])==T){
        data[1,i] <- element
      }
      i=+1
    }
  }
  
  #difyning variables to verify and print final messages
  x = 0
  y = 0
  
  #loop to go through rows
  for (i in 1:dim(data)[1]) {
    
    #lopp to go through columns
    for (j in 1:dim(data)[2]) {
      
      #searching for missing values to replace
      if(is.na(data[i,j])){
        if(last.na==TRUE){
          data[i,j] <-  data[i-1,j]
          x = x+1
          y = y+1
        }else{
          data[i,j] <-  element
          x = x+1
        }
      }else if(is.infinite(data[i,j])){
        if(last.na==TRUE){
          data[i,j] <-  data[i-1,j]
          x = x+1
          y = y+1
        }else{
          data[i,j] <-  element
          x = x+1
        }
      }else if(is.null(data[i,j])){
        if(last.na==TRUE){
          data[i,j] <-  data[i-1,j]
          x = x+1
          y = y+1
        }else{
          data[i,j] <-  element
          x = x+1
        }
      }else if(is.nan(data[i,j])){
        if(last.na==TRUE){
          data[i,j] <-  data[i-1,j]
          x = x+1
          y = y+1
        }else{
          data[i,j] <-  element
          x = x+1
        }
      }
      j = j+1
    }
    i = i+1
  }
  
  #print final messages
  if(x!=0 && y==0){
    cat("All missing values(NA, NaN, inf and NULL) have been replaced by:",element,"\n")
  }else if(x!=0 && y!=0){
    print("All missing values have been replaced with previous value")
  }else if(x==0){
    print("There are no missing values to replace!")
  }
  return(data)
}
