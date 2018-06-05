cat("\014")
rm(list=ls())
library("dplyr")
library("stringr")
library("lubridate")
library("ggplot2")

##### Created   5/29/2018
##### Modified: 5/30/2018 

#######################################################
############### Node tracking #########################
#######################################################
IniDir<-"G:/DC_Users/atenkat/IrvFile/Elec"
MainDir<-"G:/DC_Users/atenkat/IrvFile/Elec/PML nodo Distribuido"
OutDir<- "G:/DC_Users/atenkat/IrvFile/Elec/out"

setwd(IniDir)
NodeDet<-read.csv("nodeDetails.csv", stringsAsFactors = F, skip=1)
NodeDet<-NodeDet%>% select(CLAVE, SISTEMA, ZONA.DE.CARGA, NOMBRE, NIVEL.DE.TENSIîN..kV.)

setwd(MainDir)
### Read the csv files in the folder
list_files <- list.files(pattern="*.csv")
system_str<-rep("NA",length(list_files))

### Add data for the system in which the region operates
system_vec<-c("BCA MTR","BCS MTR","SIN MTR")

for (ix in system_vec){
 key<-grepl(ix,list_files)
 system_str[key]<-ix
}
system_str<-sub(" MTR","",system_str)

### Additional information for the loop
num_file<-length(list_files)
month_vec<-c( "Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
              "Agosto","Septiembre","Octubre","Noviembre","Diciembre")
dfReg<-{}

for (ix in 1:num_file){
  temp0<-read.csv(list_files[ix], stringsAsFactors = F, nrows=40)
  ### Determine where to start the reading of the file
  skip_row<-grep("Hora",temp0[,1])
  fecha_id<-temp0 %>% filter(grepl("Fecha:",.[[1]])) %>% as.character()

  ### Transform date to be read
  fecha_id<-sub("\\s","",fecha_id)
  fecha_id<-sub("Fecha:","",fecha_id)
  fecha_id<-strsplit(fecha_id, "[/]")

  day_v<-fecha_id[[1]][1]
  month_v<-which(month_vec==fecha_id[[1]][2])
  year_v<-fecha_id[[1]][3]
  
  ## Read the whole file
  temp0<-read.csv(list_files[ix], stringsAsFactors = F, skip=skip_row)
  
  ## Variables of interest
  temp0<-temp0 %>% mutate(name.file=list_files[ix] , fecha.iso=ISOdate(year_v,month_v,day_v),
                          system=system_str[ix])
  

  temp0<-temp0 %>% mutate(lag.value=lag(Precio.Zonal.....MWh., n = 1, default = NA,order_by = Hora))
  temp0<-temp0 %>% mutate( Price.diff=(log(Precio.Zonal.....MWh./lag.value)) )
  temp0<-temp0 %>% mutate( week=strftime(fecha.iso, format = "%V"),
                           day_week=strftime(fecha.iso, format = "%u"),  day_month=strftime(fecha.iso, format = "%m") )
  temp0<-temp0 %>% select( -X )
  
  dfReg<-rbind(dfReg,temp0) 
}

#####################################################
#########  Create summaries and lag data ############
#####################################################

dfReg<-dfReg %>% group_by(Zona.de.Carga, fecha.iso) %>%
 summarise( mean.pri.diff=sum(Price.diff, na.rm=T), sd.price=sd(Precio.Zonal.....MWh.),
            mean.pri=mean(Precio.Zonal.....MWh.), week.year=first(week),
            week.day=first(day_week), month_v=first(day_month), system=first(system))

dfReg$id<-(1:nrow(dfReg)-1)%%458
dfReg<-dfReg %>% group_by(Zona.de.Carga) %>% 
         mutate(lag1.rdiff = lag(mean.pri, n = 1, default = NA,order_by = id))

dfReg<-dfReg %>% group_by(Zona.de.Carga) %>% mutate(r.diff=log(mean.pri/lag1.rdiff) )

dfReg<-dfReg %>% group_by(Zona.de.Carga) %>%
  mutate(lag1.rdiff = lag(r.diff, n = 1, default = NA,order_by = id),
         lag2.rdiff = lag(r.diff, n = 2, default = NA,order_by = id),
         lag3.rdiff = lag(r.diff, n = 3, default = NA,order_by = id),
         lag4.rdiff = lag(r.diff, n = 4, default = NA,order_by = id),
         lag5.rdiff = lag(r.diff, n = 5, default = NA,order_by = id),
         lag6.rdiff = lag(r.diff, n = 6, default = NA,order_by = id),
         lag7.rdiff = lag(r.diff, n = 7, default = NA,order_by = id) )


#### Modelling 
model.lag<-with(dfReg, lm(r.diff~lag1.rdiff+lag2.rdiff+lag3.rdiff+
                            lag4.rdiff+lag5.rdiff+lag6.rdiff+lag7.rdiff+
                            factor(week.day)+factor(month_v)+factor(Zona.de.Carga)))

#### Get rid of the NAs from the dataset
dfReg<-dfReg %>% filter(!is.na(lag7.rdiff))
dfReg$fitted<-model.lag$fitted.values

### Plot the fit of the model
name_vec<-"VDM CENTRO"
key<-which(dfReg$Zona.de.Carga==name_vec)
### Plot the difference

plot(8:457,dfReg$r.diff[key],type="o", xlim=c(length(key)-60,length(key)),
     ylim=c(-1,1),main=paste("Zona:",dfReg[key[1],1]))
lines(8:457,dfReg$fitted[key],type="o",col="red")

plot(8:457,dfReg$sd.price[key],type="o",
     main=paste("Zona:",dfReg[key[1],1]))




plot(dfReg[key,"id"],dfReg[key,"r.diff"])



plot_regions<- c("ENSENADA","ACAPULCO","CENTRO SUR","PIEDRAS NEGRAS","VILLAHERMOSA")

key<-is.element(dfReg$Zona.de.Carga,plot_regions)
dfReg<-dfReg[key,]


p<-ggplot(dfReg, aes(y=mean.pri.diff, x=id, group=Zona.de.Carga)
          ) +
  geom_line(aes(color=Zona.de.Carga))+
  geom_point(aes(color=Zona.de.Carga))+
  scale_x_continuous(limits = c(400, 458))+
  scale_y_continuous(limits = c(-.2, 2.1))
p

qplot(1:458,dfReg[key,3], main=paste("Zona:",TEST[key[1],1]),xlab = "Weeks",
      ylab = "% Variation",xlim=c(length(key)-60,length(key)))+
  geom_line(linetype = "dashed",color="red")
