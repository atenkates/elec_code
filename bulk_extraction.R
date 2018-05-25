cat("\014")
rm(list=ls())
library("dplyr")

##### Created   5/24/2018
##### Modified: 5/24/2018 

MainDir<-"G:/DC_Users/atenkat/IrvFile/Elec/PMLs"
setwd(MainDir)

### Read the csv files in the folder
list_files <- list.files(pattern="*.csv")
num_file<-length(list_files)
## num_file<-300 ## Borrar esta linea para jalar todos

### Read the files...
dfNod<-{}
dfZon<-{}
nod_file<-{}
zone_file<-{}


for (ix in 1:num_file){
  ## Find the relevant information in the file
  temp0<-read.csv(list_files[ix], stringsAsFactors = F, nrows=40)
  skip_row<-grep("Hora",temp0[,1])
  fecha_id<-temp0 %>% filter(grepl("Fecha:",.[[1]])) %>% as.character()

    ## Read
  temp0<-read.csv(list_files[ix], stringsAsFactors = F, skip=skip_row)
  temp0<-temp0 %>% mutate(name.file=list_files[ix] , fecha.id=fecha_id   )
  # print(paste("Reading file:",list_files[ix]," reported date is",fecha_id ))

  ## Merge:
  
  if (ncol(temp0)==8  ){ 
    nod_file<-c(nod_file,list_files[ix]) 
    dfNod<-rbind(dfNod,temp0)
  }
  else { 
    zone_file<-c(zone_file,list_files[ix]) 
    temp0<-temp0 %>% select(-one_of("X"))
    dfZon<-rbind(dfZon,temp0)
    
   
    } 
}
