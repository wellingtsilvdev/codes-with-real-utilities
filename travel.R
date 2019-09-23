travel <-  function(data,element = 0,last.na = FALSE){
  x = 0
  y = 0
  for (i in 1:dim(data)[1]) {
    for (j in 1:dim(data)[2]) {
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
  if(x!=0 && y==0){
    cat("All missing values(NA, NaN, inf and NULL) have been replaced by:",element,"\n")
  }else if(x!=0 && y!=0){
    print("All missing values have been replaced with previous value")
  }else if(x==0){
    print("There are no missing values to replace!")
  }
  return(data)
}