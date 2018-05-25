cat("\014")
rm(list=ls())
library("dplyr")
library("stringr")
##### Created   5/25/2018
##### Modified: 5/25/2018 

#######################################################
############### Node tracking #########################
#######################################################
IniDir<-"G:/DC_Users/atenkat/IrvFile/Elec"
MainDir<-"G:/DC_Users/atenkat/IrvFile/Elec/PMLs"
OutDir<- "G:/DC_Users/atenkat/IrvFile/Elec/out"

setwd(IniDir)
NodeDet<-read.csv("nodeDetails.csv", stringsAsFactors = F, skip=1)
NodeDet<-NodeDet%>% select(CLAVE, SISTEMA, ZONA.DE.CARGA, NOMBRE, NIVEL.DE.TENSIîN..kV.)

setwd(MainDir)

### Read the csv files in the folder
list_files <- list.files(pattern="*.csv")
num_file<-length(list_files)
month_vec<-c( "Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
              "Agosto","Septiembre","Octubre","Noviembre","Diciembre")

### Create a stupid initializing vector 
Clave.del.nodo<-c("07ACU-115", "07APD-230" )
Inivals<- c(-99999,-99999)
dfNod<-data.frame(Clave.del.nodo,Inivals,stringsAsFactors = F)



for (ix in 1:num_file){
  ## Find the relevant information in the file
  temp0<-read.csv(list_files[ix], stringsAsFactors = F, nrows=40)
  skip_row<-grep("Hora",temp0[,1])
  fecha_id<-temp0 %>% filter(grepl("Fecha:",.[[1]])) %>% as.character()
  
  ### Transform date to be read
  fecha_id<-sub("\\s","",fecha_id)
  fecha_id<-sub("Fecha:","",fecha_id)
  fecha_id<-strsplit(fecha_id, "[/]")
  day_v<-as.numeric(fecha_id[[1]][1])
  month_v<-which(month_vec==fecha_id[[1]][2])
  year_v<-as.numeric(fecha_id[[1]][3])
  
  date_str<-paste0(day_v,".",month_v,".",year_v)
  
  ## Read the whole file
  temp0<-read.csv(list_files[ix], stringsAsFactors = F, skip=skip_row)
  
  ## Node extraction
  if (ncol(temp0)==6  ){ 
  temp0<-temp0 %>% select(Clave.del.nodo) %>% unique()  
  temp0<-cbind(temp0,rep(1,nrow(temp0)))
  names(temp0)[2]<-date_str  
  
  ## Append to the full description
  dfNod<-dfNod%>% full_join(temp0, by="Clave.del.nodo")
    
    
  }  else { 
    # zone_file<-c(zone_file,list_files[ix]) 
    # temp0<-temp0 %>% select(-one_of("X"))
    # dfZon<-rbind(dfZon,temp0)
    } 
}

dfNod<-dfNod %>% select(-Inivals)
dfNod<-NodeDet %>% right_join(dfNod, by=c("CLAVE"="Clave.del.nodo"))


setwd(OutDir)
write.csv(dfNod,"nodeDistribution.csv")
