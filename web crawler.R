# loading librarys
library(RSelenium)
library(tidyverse)

# creating the driver to conect with the server
driver <- rsDriver(browser = c("firefox"))

remote_driver <- driver[["client"]] # selecting the client

# navegating to the principal page
remote_driver$navigate("https://www.municipioonline.com.br/al/prefeitura/campoalegre/cidadao/servidor")

# function to select tags
selectTag = function(element){
  if(!inherits(element, "webElement")){
    stop("element should be a web element.")
  }
  if(!identical(element$getElementTagName()[[1]], "select")){
    stop("element does not appear to point to a select element in DOM.")
  }
  options <- element$findChildElements("css", "option")
  optiontext <- vapply(options, 
                       function(x)x$getElementText()[[1]], 
                       character(1)
  )
  optionvalues<- vapply(options, 
                        function(x)x$getElementAttribute("value")[[1]], 
                        character(1)
  )
  list(elements = options, text = optiontext, value = optionvalues)
}

years = c(6,5,4,3) # creating a vector to defyne the year selection order

z = 0

for(y in years){ # loop to select years
  
  # selecting the year element
  year_elem <- remote_driver$findElement(using = 'id', value = 'ddlAnoFolhaPagamento')
  
  select_year <- selectTag(year_elem) # taking year tags
  
  select_year$elements[[y]]$clickElement() # clicking on current tag that match with y loop
  
  for(i in 1:12){ # loop to select months
    
    z = z+1
    
    # selecting the month element
    month_elem <- remote_driver$findElement(using = 'id', value = 'ddlMesFolhaPagamento')
    
    select_month <- selectTag(month_elem) # taking month tags
    
    select_month$elements[[i]]$clickElement() # clicking on current tag that match with i loop
    
    # selecting the searching bottom
    scr_bottom <- remote_driver$findElement(using = 'id', value = 'btnFiltrarFolhaPagamento')
    scr_bottom$clickElement()
    
    Sys.sleep(45) # giving s pause in loop before click on download bottom
    
    cat("Downloading...",select_month$text[i],select_year$text[y],"\n")
    
    # selecting the download bottom
    dowl_bottom <- remote_driver$findElement(using = 'xpath', value = '/html/body/form/div[4]/section/div/div/section[2]/div[2]/div/div/div/div/div/div[1]/div/div[6]/div/div/div/div/div[2]/div/div/div/div[1]/a[2]/span/img')
    dowl_bottom$clickElement()
    
# ----------------------------------------- treating the downloaded file -----------------------------------------#
    
    setwd("D:/Wellington/Desktop/Dados") # set your directory here
    
    dados = read.csv('Município Online.csv',header=T,sep=',') # loading downloaded file
    
    # naming columns
    names(dados) <- c('date','nome','matricula','cargo'
                      ,'nivel','valor_base','proventos'
                      ,'descontos','liquido')
    
    dados$date <- paste(select_month$text[i],select_year$text[y]) # adding dates to date column
    
    # treating the numeric columns
    for(j in 1:9){
      if(j!=3){
        dados[,j] <- as.character(dados[,j]) # converting all columns to characters except matricula column
      }
      if(j>=6 & i<=9){
        dados[,j] <- gsub("[a-zA-Z$.]", "", dados[,j]) # removing R$ and .
        dados[,j] <- gsub("[,]", ".", dados[,j]) # replacing , by .
        dados[,j] <- as.numeric(dados[,j]) # converting into numeric again
      }
    }
    
    # exporting new file treated
    write.table(dados,paste("file",z,".csv"),sep=',',dec='.',row.names = F)
    
    if(z==1){
      file1 = read.csv('file 1 .csv',header=T,sep=',') # loading downloaded file
    }

    # importing download data and join
    if(z==2){
      file_name = paste("file",z,".csv")
      file2 = read.csv(file_name,header=T,sep=',') # loading downloaded file
      
      total_file = rbind(file1,file2)
    }
    if(z>2){
      file_name = paste("file",z,".csv")
      file2 = read.csv(file_name,header=T,sep=',') # loading downloaded file
      
      total_file = rbind(total_file,file2)
      
      # exporting new file treated
      write.table(total_file,"final_file.csv",sep=',',dec='.',row.names = F)
      file.remove(file_name)
    }
    
    file.remove("Município Online.csv") # deliting old file
  }
  if(y=3 & i=12){
    print("web crawler finish all downloads!")
  }
}